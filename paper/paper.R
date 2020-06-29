# ---- paper ----
library(magrittr)
library(grid)
library(tikzDevice)
library(rgdal)
library(ggplot2)
options(tikzDefaultEngine='pdftex')

texOutputDir = Sys.getenv("TEX_OUTPUT_DIR")
if (texOutputDir == "") {
  texOutputDir = ".texpadtmp"
}
figuresDir = paste0(texOutputDir, "/figures")
if (!dir.exists(figuresDir)) {
  dir.create(figuresDir, recursive=TRUE)
}
options(tikzMetricsDictionary=paste0(texOutputDir, '/tikzDictionary.dat'))

inlinePlotWidth = 3.1
inlinePlotHeight = 2.5
pagePlotWidth = inlinePlotWidth * 2.1
pagePlotHeight = inlinePlotHeight * 2.1 * 2 / 5
plotTextBaseSize = 8

# Formatting utilities
formatDate = function(date) {
  format(date, "%B, %e, %Y")
}

formatYear = function(date) {
  format(date, "%Y")
}

formatMonth = function(date) {
  format(date, "%B %Y")
}

formatThreshold = function(threshold) {
  sprintf("%.2f", threshold %% 52)
}

regionOnset = function(c) {
  formatThreshold((thresholdsByCounty %>% filter(county==c))$onset.median)
}

regionOffset = function(c) {
  formatThreshold((thresholdsByCounty %>% filter(county==c))$offset.median)
}

formatThresholdCI = function(lower, upper, level) {
  sprintf("%d\\%% CI: week %.2f -- %.2f", level * 100, lower %% 52, upper %% 52)
}

regionOnsetCI = function(c, level) {
  formatThresholdCI(
    (thresholdsByCounty %>% filter(county==c))$onset.lower,
    (thresholdsByCounty %>% filter(county==c))$onset.upper,
    level
  )
}

regionOffsetCI = function(c, level) {
  formatThresholdCI(
    (thresholdsByCounty %>% filter(county==c))$offset.lower,
    (thresholdsByCounty %>% filter(county==c))$offset.upper,
    level
  )
}

formatPct = function(pct) {
  sprintf("%.2f\\%%", pct * 100)
}

formatPctCI = function(lower, upper, level) {
  sprintf("%d\\%% CI: %.2f -- %.2f\\%%", level * 100, lower * 100, upper * 100)
}

regionStrategyCoverage = function(c, s, r) {
  formatPct((unprotectedByCounty %>% filter(county==c, strat==s, rounding==r))$frac.median)
}

regionStrategyCoverageCI = function(c, s, r, level) {
  formatPctCI(
    (unprotectedByCounty %>% filter(county==c, strat==s, rounding==r))$frac.lower,
    (unprotectedByCounty %>% filter(county==c, strat==s, rounding==r))$frac.upper,
    level
  )
}

bestCaseCoverage = function(r) {
  formatPct((bestCaseCoverageByRounding %>% filter(rounding==r))$frac.median)
}

bestCaseCoverageCI = function(r, level) {
  formatPctCI(
    (bestCaseCoverageByRounding %>% filter(rounding==r))$frac.lower,
    (bestCaseCoverageByRounding %>% filter(rounding==r))$frac.upper,
    level
  )
}

formatDrift = function(lm) {
  sprintf("%.2f days/year", summary(lm)$coefficients['epiyear', 1] * 7)
}

formatDriftCI = function(lm, level) {
  ci = confint(lm, "epiyear", level=level) * 7
  sprintf(
    "%d\\%% CI: %.2f -- %.2f days/year", level * 100,
    min(ci),
    max(ci)
  )
}

latexPercent = function(f) {
  sprintf("%.0f\\%%", f * 100)
}

onsetOffsetLabeller = labeller(
  variable=c(`onset` = "RSV season onset", `offset` = "RSV season offset")
)

roundingLabels = c(`0` = "No rounding", `1` = "Weekly rounding", `2` = "Biweekly rounding", `4` = "Monthly rounding")

stratLabels = c(
  `stateOnset` = "Statewide all-years onset",
  `stateMiddle` = "Statewide all-years midseason", 
  `stateOffset` = "Statewide all-years offset", 
  `countyOnset` = "County-level all-years onset",
  `countyMiddle` = "County-level all-years midseason",
  `countyOffset` = "County-level all-years offset", 
  `stateSlidingOnset` = "Statewide recent-years onset",
  `stateSlidingMiddle` = "Statewide recent-years midseason",
  `stateSlidingOffset` = "Statewide recent-years offset", 
  `aap` = "AAP guidelines"
)

countyLabels = c(
  `all` = "Statewide",
  `New Haven` = "New Haven county",
  `Fairfield` = "Fairfield county",
  `Hartford` = "Hartford county",
  `lowIncidence` = "Low-population counties"
)

countyLabelsShort = c(
  `all` = "Statewide",
  `New Haven` = "New Haven c.",
  `Fairfield` = "Fairfield c.",
  `Hartford` = "Hartford c.",
  `lowIncidence` = "Low-population c."
)

# .01 to let minor gridlines show through
epiWeekBreaks = c(3.01, 7.25, 11.75, 16.01, 20.25, 24.75, 29.01, 33.01, 37.5, 42.01, 46.25, 50.75)
epiWeekLabels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

breaks.df = data.frame(i=seq(1, 12), mid=epiWeekBreaks)
monthBoundaries = breaks.df %>%
  inner_join(
    breaks.df %>% mutate(i=i %% 12 + 1) %>% rename(prevMid=mid),
    by="i"
  ) %>%
  inner_join(
    breaks.df %>% mutate(i=(i - 2) %% 12 + 1) %>% rename(nextMid=mid),
    by="i"
  ) %>%
  mutate(
    min=(prevMid + (mid - prevMid) %% 52 / 2) %% 52,
    max=mid + (nextMid - mid) %% 52 / 2
  ) %>%
  select(i, min, mid, max)

epiWeekBreaks = sort(unique(c(monthBoundaries$min, monthBoundaries$max)))

onsetOffsetMonthBoundaries = data.frame(i=c(4, 5, 9, 10), variable=c("onset", "onset", "offset", "offset")) %>%
  inner_join(monthBoundaries, by="i") %>%
  mutate(label=epiWeekLabels[i])
  
legendLabels = function(labels, leading=0, trailing=2) {
  as.vector(sapply(labels, function(label) {
    if(substr(label, 1, 1) == "_") {
      paste0(rep(" ", as.numeric(substring(label, 2))), collapse="")
    } else {
      if (leading > 0) {
        label = sprintf("%s%s", strrep("\\qquad{}", leading), label)
      }
      if (trailing > 0) {
        label = sprintf("%s%s", label, strrep("\\qquad{}", trailing))
      }
      label
    }
  }))
}

twoTone = c("#018571", "#80cdc1")

lightBlue = "#a6cee3"
darkBlue = "#1f78b4"
lightGreen = "#b2df8a"
darkGreen = "#33a02c"
lightRed = "#fb9a99"
darkRed = "#e31a1c"
lightOrange = "#FFBF80"
darkOrange = "#ff7f00"
lightPurple = "#cab2d6"
darkPurple = "#6a3d9a"
lightBrown = "#E3C0AC"
darkBrown = "#b15928"

# Counties which we plot (so, not individual low-population counties)
countiesForPlots = thresholdsByCounty$county[!thresholdsByCounty$county %in% lowIncidenceCounties]

# Use this to draw legend indicators that don't take up the entire height of the key
draw_key_vline_small = function(data, params, size) {
  segmentsGrob(0.5, 0.125, 0.5, 0.875,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )    
}