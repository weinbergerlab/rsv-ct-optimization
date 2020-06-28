# ---- libraries ----
library(reshape)
library(dplyr)
library(mgcv)
library(pspline.inference)
library(zipcode)
library(ggplot2)
library(easyGgplot2)
library(data.table)
library(splitstackshape)
library(maps)

# ---- external ----
data(zipcode)
set.seed(0)

if (Sys.getenv("KNITR_DRAFT_MODE", "") != "") {
  simulations = 5
} else {
  simulations = 2000
}

# *** Data munging helpers

# Augment a (county, time, rsv) data frame with rows for regions consisting of >1 county
summarizeCountyGroups = function(data) {
  rbind(
    as.data.frame(data) %>%
      select(county, time, rsv),
    as.data.frame(data) %>%
      filter(group.all==TRUE) %>%
      summarize(county="all", time=max(time), rsv=sum(rsv)),
    as.data.frame(data) %>%
      filter(group.lowIncidence==TRUE) %>%
      summarize(county="lowIncidence", time=max(time), rsv=sum(rsv))
  )
}

# *** Analysis parameters

lowIncidenceCounties = c("Tolland", "Windham", "Middlesex", "Litchfield", "New London")
eps = .05
level = .95
seasonThreshold = 0.025
ppxDuration = 24 # weeks
slidingWindowDuration = 3 # Years
ppxRounding = c(0, 1, 2, 4)

# *** Load / munge data

# Load data
admissions = fread("data/ct rsv time series.csv", sep=",", header=TRUE, colClasses=c("patzip"="character"))

epiyear = function(date) {
  ifelse(month(date) > 6, year(date), year(date) - 1)
}

epiyday = function(date) {
  as.numeric(date - as.Date(ISOdate(epiyear(date), 6, 30)))
}

epiweek = function(date) {
  floor((epiyday(date) - 1) / 7) + 1
}

# Add week-of-year, with zero = first week of January, drop first 5.5 years because of coding differences
datasetAll = admissions %>%
  mutate(
    adate=as.Date(adate, "%d%b%Y"),
    epiyear=epiyear(adate),
    epiyday=epiyday(adate),
    epiweek=epiweek(adate)
  ) %>%
  select(patzip, adate, weeki, rsv, epiyear, epiyday, epiweek)

dataset = datasetAll %>%
  filter(epiyear >= 1996, epiyear <= 2012)
  
# Add state based on zipcode and drop everything not in CT
zipState = zipcode %>% select(zip, state)
dataset = dataset %>%
  left_join(zipcode %>% select(zip, state), by=c("patzip"="zip")) %>%
  filter(state=="CT")
# Add county based on zipcode
countyByZip = fread("data/ct counties.csv", sep=",", header=TRUE, colClasses=c("zip"="character"))
counties = countyByZip %>% select(county) %>% distinct()
dataset = dataset %>% inner_join(countyByZip, by=c("patzip"="zip"))

# Sort counties by total RSV
totalRSVByCounty = dataset %>%
  group_by(county) %>%
  summarize(total_rsv=sum(rsv)) %>%
  arrange(total_rsv) %>%
  mutate(county=factor(county, county))

rsvCounties = totalRSVByCounty$county
rsvEpiYears = sort(unique(dataset$epiyear))
rsvEpiWeekRange = sort(unique(dataset$epiweek))

highIncidenceCounties = rsvCounties[!rsvCounties %in% lowIncidenceCounties]

dataset = dataset %>%
  mutate(county=factor(county, levels=levels(rsvCounties)))

# Data frame of which county is in which group
rsvCountyGroups = data.frame(county=rsvCounties) %>%
  mutate(
    group.all=TRUE,
    group.lowIncidence=county %in% lowIncidenceCounties
  )

# Sort county factor by total RSV
dataset = dataset %>%
  mutate(county=factor(county, levels=levels(rsvCounties)))

startDate = min(dataset$adate)
endDate = max(dataset$adate)

rsvTime = dataset$epiweek
modelTime = data.frame(time=seq(min(rsvTime) - 1 + eps, max(rsvTime), eps))

# *** Prophyaxis assessment helpers

epochRound = Vectorize(function(time, epoch) {
  if (epoch > 0) {
    # Rounding for prophylaxis strategies has phase as follows
    # For rounding=1, .5 .. 1.5 round to 1
    # For rounding=2, 1 .. 3 round to 2
    # For rounding=4, 2 .. 6 round to 4
    time = epoch * round(time / epoch)
  }
  time
})

evalStrategy = function(start, end, time) {
  as.numeric((time >= start) & (time < end))
}

# *** Prophylaxis regimen definitions

ppxFixedStrategyStart = function(c, rounding) {
  stateOnset = stateThresholds$onset.median
  stateOffset = stateThresholds$offset.median
  
  if (!is.null(c)) {
    countyOnset = (thresholdsByCounty %>% filter(county==c))$onset.median
    countyOffset = (thresholdsByCounty %>% filter(county==c))$offset.median
  } else {
    countyOnset = countyOffset = NA
  }
  
  list(
    stateOnset = epochRound(stateOnset, rounding),
    stateMiddle = epochRound((stateOnset + stateOffset - ppxDuration) / 2, rounding),
    stateOffset = epochRound(stateOffset - ppxDuration, rounding),
    countyOnset = epochRound(countyOnset, rounding),
    countyMiddle = epochRound((countyOnset + countyOffset - ppxDuration) / 2, rounding),
    countyOffset = epochRound(countyOffset - ppxDuration, rounding),
    # Epi week 20 starts Nov 12, 13, 14, 15, 16, 17, or 18
    aap = 20
  )
}

# Return a list of prophylaxis strategies for a county. If c is NULL, then it returns a list of prophylaxis strategy names
ppxFixedStrategies = function(c=NULL, rounding=0) {
  lapply(ppxFixedStrategyStart(c, rounding), function(stratStart) {
    function(time) {
      evalStrategy(stratStart, stratStart + ppxDuration, time)
    }
  })
}

ppxSlidingStrategies = function(c=NULL, y=NULL) {
  if (!is.null(y) && !is.null(c)) {
    stateSlidingOnset = (stateThresholdsByWindow %>% filter(epiyear==y))$onset.median
    stateSlidingOffset = (stateThresholdsByWindow %>% filter(epiyear==y))$offset.median
  }

  list(
    stateSlidingOnset=function(time) {
      start = stateSlidingOnset
      evalStrategy(start, start + ppxDuration, time)
    },
    stateSlidingMiddle=function(time) {
      start = (stateSlidingOnset + stateSlidingOffset - ppxDuration) / 2
      evalStrategy(start, start + ppxDuration, time)
    },
    stateSlidingOffset=function(time) {
      start = stateSlidingOffset - ppxDuration
      evalStrategy(start, start + ppxDuration, time)
    }
  )
}

# *** Prophylaxis coverage estimation

# Estimate % left unprotected by prophylaxis strategy
pspline.outbreak.unprotected = function(strategies) {
  function(model, data) {
    # Calculate total # of unprotected cases for each strategy
    unprotected = as.data.frame(lapply(strategies, function(strat) {
      sum(data$rsv * (1 - sapply(data$time, strat)))
    }))

    # Calculate total # of cases
    total = sum(data$rsv)

    unprotected %>%
      rename_all(list(
        sprintf=~sprintf("%s.count", .)
      )) %>%
      cbind(
        (unprotected / total) %>%
          rename_all(list(
            sprintf=~sprintf("%s.frac", .)
          ))
      ) %>%
      mutate(total=total)
  }
}

# *** State-level all-years analysis

stateObs = dataset %>%
  rename(time=epiweek) %>%
  group_by(time) %>%
  summarize(rsv=sum(rsv)) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / sum(rsv)) %>%
  as.data.frame()

stateModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=stateObs)

statePred = stateModel %>%
  predict(type="response", newdata=modelTime, se.fit=TRUE)

statePred = stateModel %>%
  pspline.estimate.timeseries(modelTime, pspline.outbreak.cumcases.relative, samples=simulations) %>%
  cbind(data.frame(rsv.fit=statePred$fit, rsv.fit.se=statePred$se.fit)) %>%
  rename(rsv.cum.fit.lower=rsv.cumrel.lower, rsv.cum.fit.upper=rsv.cumrel.upper, rsv.cum.fit=rsv.cumrel.median)

stateThresholds = stateModel %>%
  pspline.estimate.scalars(
    modelTime,
    pspline.outbreak.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold),
    samples=simulations
  )

# *** Regional all-years analysis

obsByCounty = dataset %>%
  rename(time=epiweek) %>%
  group_by(county, time) %>%
  summarize(rsv=sum(rsv)) %>%
  left_join(rsvCountyGroups, by="county") %>%
  group_by(time) %>%
  do(summarizeCountyGroups(.)) %>%
  group_by(county) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / sum(rsv)) %>%
  as.data.frame() %>%
  mutate(county=factor(county, levels(county)[c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10)]))

predByCounty = data.frame()
thresholdsByCounty = data.frame()
fixedStratUnprotectedSimByCountyRounding = data.frame()
fixedStratDescByCountyRounding = data.frame()
fixedStratUnprotectedByCounty = data.frame()

for (c in levels(obsByCounty$county)) {
  message(paste("Regional all-years:", c))
  countyObs = obsByCounty %>% filter(county==c)
  countyModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=countyObs)

  countyPred = countyModel %>%
    predict(type="response", newdata=modelTime, se.fit=TRUE)

  countyPred = countyModel %>%
    pspline.estimate.timeseries(
      modelTime,
      pspline.outbreak.cumcases.relative,
      samples=simulations
    ) %>%
    cbind(data.frame(rsv.fit=countyPred$fit, rsv.fit.se=countyPred$se.fit)) %>%
    rename(rsv.cum.fit.lower=rsv.cumrel.lower, rsv.cum.fit.upper=rsv.cumrel.upper, rsv.cum.fit=rsv.cumrel.median) %>%
    mutate(county=factor(c, levels=levels(obsByCounty$county)))

  predByCounty = predByCounty %>% rbind(countyPred)

  countyThresholds = countyModel %>%
    pspline.estimate.scalars(
      modelTime,
      pspline.outbreak.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold),
      samples=simulations
    ) %>%
    mutate(county=factor(c, levels=levels(obsByCounty$county)))

  thresholdsByCounty = thresholdsByCounty %>% rbind(countyThresholds)

  for (rounding in ppxRounding) {
    message(paste("Regional all-years with rounding:", as.character(rounding)))
    # Separate simulation from confidence intervals in order to use simulation data to get statewide estimates
    countyUnprotectedSim = countyModel %>%
      pspline.sample.scalars(
        modelTime,
        pspline.outbreak.unprotected(ppxFixedStrategies(c, rounding)),
        samples=simulations
      )

    countyUnprotected = countyUnprotectedSim %>%
      pspline.confints.scalars(countyModel) %>%
      mutate(county=factor(c, levels=levels(obsByCounty$county)), rounding=rounding)

    fixedStratUnprotectedByCounty = fixedStratUnprotectedByCounty %>%
      rbind(countyUnprotected)

    fixedStratUnprotectedSimByCountyRounding = fixedStratUnprotectedSimByCountyRounding %>%
      rbind(
        countyUnprotectedSim %>%
        mutate(county=factor(c, levels=levels(obsByCounty$county)), rounding=rounding)
      )

    fixedStratDescByCountyRounding = fixedStratDescByCountyRounding %>%
      rbind(
        data.frame(ppxFixedStrategyStart(c, rounding)) %>%
        mutate(county=factor(c, levels=levels(obsByCounty$county)), rounding=rounding)
      )
  }
}

fixedStratDescStatewide = fixedStratDescByCountyRounding %>%
  filter(county %in% highIncidenceCounties | county == "lowIncidence") %>%
  melt(c("county", "rounding")) %>%
  rename(strat=variable, startWeek=value) %>%
  group_by(rounding, strat) %>%
  do((function(df) {
    df = df %>% arrange(county)
    name = paste0(sprintf("%s=%s", df$county, df$startWeek), collapse=";")
    df = df %>% select(-county, -startWeek)
    df = df[1,]
    df$variable = "desc"
    df$value = name
    df
  })(.)) %>%
  ungroup() %>%
  cast(rounding + strat ~ variable) %>%
  mutate(
    sliding=FALSE,
    county="all",
    desc=as.character(desc),
    county=factor(county)
  )

fixedStratUnprotectedStatewide = fixedStratUnprotectedSimByCountyRounding %>%
  filter(county %in% highIncidenceCounties | county == "lowIncidence") %>%
  group_by(rounding, pspline.sample) %>%
  summarize_at(vars(contains("total"), contains(".count")), sum) %>%
  group_by(rounding, pspline.sample) %>%
  do((function(df) {
    stratNames = names(ppxFixedStrategies())
    for (stratName in stratNames) {
      countName = sprintf("%s.count", stratName)
      fracName = sprintf("%s.frac", stratName)
      df[[fracName]] = df[[countName]] / df$total
    }
    df
  }) (.)) %>%
  ungroup() %>%
  group_by(rounding) %>%
  do((function(df) {
    df %>%
      select(-rounding, -total, -matches(".count")) %>%
      pspline.confints.scalars (stateModel) %>%
      mutate(rounding=unique(df$rounding))
  })(.)) %>%
  ungroup() %>%
  mutate(county="all") %>%
  as.data.frame() %>%
  mutate(sliding=FALSE)

# *** State-level sliding window analysis

stateObsByWindow = dataset %>%
  mutate(time=weeki) %>%
  group_by(time, epiyear, epiweek) %>%
  summarize(rsv=sum(rsv)) %>%
  as.data.frame()

total = data.frame(time=stateObsByWindow$time, rsv=0)

for (idx in seq(1, slidingWindowDuration)) {
  total = total %>%
    inner_join(stateObsByWindow %>%
                 mutate(time=time + 52 * idx) %>%
                 select(time, rsv),
               by="time"
    ) %>%
    mutate(rsv=rsv.x + rsv.y) %>%
    select(time, rsv)
}

stateObsByWindow = stateObsByWindow %>%
  select(-rsv) %>%
  inner_join(total, by="time") %>%
  group_by(epiyear) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / sum(rsv)) %>%
  select(-time) %>%
  rename(time=epiweek) %>%
  filter(epiyear >= min(rsvEpiYears) + slidingWindowDuration) %>%
  as.data.frame()

rsvWindowYears = unique(stateObsByWindow$epiyear)

statePredByWindow = data.frame()
stateThresholdsByWindow = data.frame()

for (y in rsvWindowYears) {
  message(paste("State-wide sliding window:", y))

  stateYearObs = stateObsByWindow %>% filter(epiyear==y)
  stateYearModel = gam(rsv ~ s(time, k=5, bs="cp", m=3), family=poisson, data=stateYearObs)

  stateYearPred = stateYearModel %>%
    predict(type="response", newdata=data.frame(time=modelTime), se.fit=TRUE)

  stateYearPred = stateYearModel %>%
    pspline.estimate.timeseries(modelTime, pspline.outbreak.cumcases.relative, samples=simulations) %>%
    cbind(data.frame(rsv.fit=stateYearPred$fit, rsv.fit.se=stateYearPred$se.fit)) %>%
    rename(rsv.cum.fit.lower=rsv.cumrel.lower, rsv.cum.fit.upper=rsv.cumrel.upper, rsv.cum.fit=rsv.cumrel.median) %>%
    mutate(epiyear=y)

  statePredByWindow = statePredByWindow %>% rbind(stateYearPred)

  stateYearThresholds = stateYearModel %>%
    pspline.estimate.scalars(
      modelTime,
      pspline.outbreak.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold),
      samples=simulations
    ) %>%
    mutate(epiyear=y)

  stateThresholdsByWindow = stateThresholdsByWindow %>% rbind(stateYearThresholds)
}

# *** Season drift analysis

onsetByYear = lm(onset.median ~ epiyear, data=stateThresholdsByWindow)
offsetByYear = lm(offset.median ~ epiyear, data=stateThresholdsByWindow)

# *** State-level sliding window regimen analysis

obsByCountyYear = dataset %>%
  rename(time=epiweek) %>%
  group_by(county, weeki, epiyear, time) %>%
  summarize(rsv=sum(rsv)) %>%
  left_join(rsvCountyGroups, by="county") %>%
  group_by(weeki, epiyear, time) %>%
  do(summarizeCountyGroups(.)) %>%
  group_by(county, epiyear) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / if_else(sum(rsv) > 0, sum(rsv), as.integer(1))) %>%
  select(-weeki) %>%
  as.data.frame()

predByCountyYear = data.frame()
thresholdsByCountyYear = data.frame()
slidingStratUnprotectedSimByCountyYear = data.frame()

for (c in levels(obsByCountyYear$county)) {
  message(paste("Regional sliding window:", c))
  for (y in rsvEpiYears) {
    message(paste("Regional sliding window:", y))
    countyYearObs = obsByCountyYear %>% filter(county==c, epiyear==y)
    countyYearModel = gam(rsv ~ s(time, k=4, bs="cp", m=3), family=poisson, data=countyYearObs)

    countyYearPred = countyYearModel %>%
      predict(type="response", newdata=data.frame(time=modelTime), se.fit=TRUE)

    countyYearPred = countyYearModel %>%
      pspline.estimate.timeseries(modelTime, pspline.outbreak.cumcases.relative, samples=simulations) %>%
      cbind(data.frame(rsv.fit=countyYearPred$fit, rsv.fit.se=countyYearPred$se.fit)) %>%
      rename(rsv.cum.fit.lower=rsv.cumrel.lower, rsv.cum.fit.upper=rsv.cumrel.upper, rsv.cum.fit=rsv.cumrel.median) %>%
      mutate(county=factor(c, levels=levels(obsByCountyYear$county)), epiyear=y)

    predByCountyYear = predByCountyYear %>% rbind(countyYearPred)

    countyYearThresholds = countyYearModel %>%
      pspline.estimate.scalars(
        modelTime,
        pspline.outbreak.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold), 
        samples=simulations
      ) %>%
      mutate(county=factor(c, levels=levels(obsByCountyYear$county)), epiyear=y)

    thresholdsByCountyYear = thresholdsByCountyYear %>% rbind(countyYearThresholds)

    if (y %in% rsvWindowYears) {
      countyYearUnprotectedSim = countyYearModel %>%
        pspline.sample.scalars(
          modelTime,
          pspline.outbreak.unprotected(ppxSlidingStrategies(c, y)), 
          samples=simulations
        ) %>%
        mutate(county=factor(c, levels=levels(obsByCountyYear$county)), epiyear=y)

      slidingStratUnprotectedSimByCountyYear = slidingStratUnprotectedSimByCountyYear %>% rbind(countyYearUnprotectedSim)
    }
  }
}

# Aggregate sliding strategies over the entire time span

slidingStratUnprotectedByCounty = slidingStratUnprotectedSimByCountyYear %>%
  group_by(county, pspline.sample) %>%
  summarize_at(vars(contains("total"), contains(".count")), sum) %>%
  group_by(county, pspline.sample) %>%
  do((function(df) {
    stratNames = names(ppxSlidingStrategies())
    for (stratName in stratNames) {
      countName = sprintf("%s.count", stratName)
      fracName = sprintf("%s.frac", stratName)
      df[[fracName]] = df[[countName]] / df$total
    }
    df
  }) (.)) %>%
  group_by(county) %>%
  do((function(df) {
    df %>%
      select(-county, -total, -matches(".count")) %>%
      pspline.confints.scalars(countyModel) %>%
      mutate(county=unique(df$county))
  })(.)) %>%
  ungroup() %>%
  mutate(sliding=TRUE, rounding=0)

# Merge sliding and fixed strategies into one data frame

unprotectedByCounty = fixedStratUnprotectedByCounty %>%
  mutate(sliding=FALSE) %>%
  filter(county != "all") %>%
  select(-matches(".count"), -matches("total")) %>%
  rbind(fixedStratUnprotectedStatewide) %>%
  select(county, rounding, sliding, contains("frac")) %>%
  melt(c("county", "rounding", "sliding")) %>%
  rbind(
    slidingStratUnprotectedByCounty %>%
      as.data.frame() %>%
      melt(c("county", "rounding", "sliding"))
  ) %>%
  cSplit("variable", ".") %>%
  mutate(strat=variable_1, variable=sprintf("%s.%s", variable_2, variable_3)) %>%
  select(-variable_1, -variable_2, -variable_3) %>%
  mutate(strat=as.factor(strat)) %>%
  cast(county + strat + rounding + sliding ~ variable) %>%
  left_join(fixedStratDescStatewide) %>%
  mutate(
    frac.temp = frac.lower,
    frac.lower = 1 - frac.upper,
    frac.upper = 1 - frac.temp,
    frac.median = 1 - frac.median,
    strat = factor(strat, c(
      names(ppxSlidingStrategies(NULL, NULL)),
      names(ppxFixedStrategies(NULL))
    )[c(10, 4, 5, 6, 7, 8, 9, 1, 2, 3)]),
    rounding = factor(rounding),
    county = factor(county)
  ) %>%
  select(-frac.temp)

bestCaseCoverageByRounding = unprotectedByCounty %>%
  filter(county == "all") %>%
  select(-desc) %>%
  group_by(rounding) %>%
  filter(frac.median == max(frac.median)) %>%
  group_by(rounding) %>%
  filter(row_number() == 1)
