# ---- util ----
library(grid)
library(tikzDevice)
options(tikzDefaultEngine='pdftex')

plotWidth=3.1
plotHeight=2.5

# Formatting utilities
formatDate = function(date) {
  format(date, "%B, %d, %Y")
}

formatYear = function(date) {
  format(date, "%Y")
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
  sprintf("%d\\%% CI: %.2f -- %.2f", level * 100, lower %% 52, upper %% 52)
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

regionStrategyCoverage = function(c, s) {
  formatPct((unprotectedByCounty %>% filter(county==c, strat==s))$frac.median)
}

regionStrategyCoverageCI = function(c, s, level) {
  formatPctCI(
    (unprotectedByCounty %>% filter(county==c, strat==s))$frac.upper,
    (unprotectedByCounty %>% filter(county==c, strat==s))$frac.lower,
    level
  )
}

driftYearToWeek = function(year, negate) {
  drift = 365 / 7 * year
  if (negate) {
    -drift
  } else {
    drift
  }
}

formatDrift = function(lm, negate=FALSE) {
  sprintf("%.2f weeks", driftYearToWeek(summary(lm)$coefficients['year', 1], negate))
}

formatDriftCI = function(lm, level, negate=TRUE) {
  ci = confint(lm, "year", level=level)
  sprintf(
    "%d\\%% CI: %.2f -- %.2f weeks", level * 100,
    driftYearToWeek(min(ci), negate),
    driftYearToWeek(max(ci), negate)
  )
}

latexPercent = function(f) {
  sprintf("%.0f\\%%", f * 100)
}

# Counties which we plot (so, not individual low-incidence counties)
countiesForPlots = thresholdsByCounty$county[!thresholdsByCounty$county %in% lowIncidenceCounties]

