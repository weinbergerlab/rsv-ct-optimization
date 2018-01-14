# ---- external ----
library(reshape)
library(dplyr)
library(mgcv)
library(outbreakpredict)
library(zipcode)
library(ggplot2)
library(easyGgplot2)
library(data.table)
library(splitstackshape)

data(zipcode)
set.seed(0)

simulations = 20

# *** Data munging helpers

# RSV week-of-year is -26..25, with week 0 being first week of the year
rsvWeek = function(weeki) {
  (weeki + 26) %% 52 - 26
}

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

# *** Load / munge data

# Load data
dataset = fread("../ct rsv time series.csv", sep=",", header=TRUE, colClasses=c("patzip"="character"))

# Add week-of-year, with zero = first week of January, drop first 5.5 years because of coding differences
dataset = dataset %>%
  mutate(
    weekiadj=round(dataset$weeki * 364 / 365.25),
    weekofyear=rsvWeek(weekiadj),
    year=1990 + (weekiadj + 26) %/% 52,
    adate=as.Date(adate, "%d%b%Y")
  ) %>%
  filter(weeki > 5.5*52) %>%
  filter(weeki <= 22.5*52)
# Add state based on zipcode and drop everything not in CT
zipState = zipcode %>% select(zip, state)
dataset = dataset %>%
  left_join(zipcode %>% select(zip, state), by=c("patzip"="zip")) %>%
  filter(state=="CT")
# Add county based on zipcode
countyByZip = fread("../ct counties.csv", sep=",", header=TRUE, colClasses=c("zip"="character"))
counties = countyByZip %>% select(county) %>% distinct()
dataset = dataset %>% inner_join(countyByZip, by=c("patzip"="zip"))

# Sort counties by total RSV
totalRSVByCounty = dataset %>%
  group_by(county) %>%
  summarize(total_rsv=sum(rsv)) %>%
  arrange(total_rsv) %>%
  mutate(county=factor(county, county))

rsvCounties = totalRSVByCounty$county
rsvYears = sort(unique(dataset$year))
rsvWeekRange = sort(unique(dataset$weeki))
rsvWeekOfYearRange = sort(unique(dataset$weekofyear))

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

rsvTime = dataset$weekofyear
modelTime = seq(min(rsvTime) - 1 + eps, max(rsvTime), eps)

# *** Prophyaxis assessment helpers

evalStrategy = function(start, end, time) {
  as.numeric((time > rsvWeek(start)) & (time < rsvWeek(end)))
}

evalOnsetStrategy = function(start, time) {
  as.numeric((time > rsvWeek(start)) & (time < rsvWeek(start + ppxDuration)))
}

evalOffsetStrategy = function(end, time) {
  as.numeric((time > rsvWeek(end - ppxDuration)) & (time < rsvWeek(end)))
}

# *** Prophylaxis regimen definitions

# Return a list of prophylaxis strategies for a county. If c is NULL, then it returns a list of prophylaxis strategy names
ppxFixedStrategies = function(c=NULL) {
  stateOnset = stateThresholds$onset.median
  stateOffset = stateThresholds$offset.median

  if (!is.null(c)) {
    countyOnset = (thresholdsByCounty %>% filter(county==c))$onset.median
    countyOffset = (thresholdsByCounty %>% filter(county==c))$offset.median
  }

  list(
    # never=function(time) { 0 },
    # always=function(time) { 1 },
    aap=function(time) {
      evalOnsetStrategy(strptime("11-15", format="%m-%d")$yday / 7, time)
    },
    stateOnset=function(time) {
      evalOnsetStrategy(stateOnset, time)
    },
    stateMiddle=function(time) {
      evalOnsetStrategy((stateOnset + stateOffset - ppxDuration) / 2, time)
    },
    stateOffset=function(time) {
      evalOffsetStrategy(stateOffset, time)
    },
    countyOnset=function(time) {
      evalOnsetStrategy(countyOnset, time)
    },
    countyMiddle=function(time) {
      evalOnsetStrategy((countyOnset + countyOffset - ppxDuration) / 2, time)
    },
    countyOffset=function(time) {
      evalOffsetStrategy(countyOffset, time)
    }
  )
}

ppxSlidingStrategies = function(c=NULL, y=NULL) {
  if (!is.null(y) && !is.null(c)) {
    stateSlidingOnset = (stateThresholdsByWindow %>% filter(year==y))$onset.median
    stateSlidingOffset = (stateThresholdsByWindow %>% filter(year==y))$offset.median
  }

  list(
    stateSlidingOnset=function(time) {
      evalOnsetStrategy(stateSlidingOnset, time)
    },
    stateSlidingOffset=function(time) {
      evalOffsetStrategy(stateSlidingOffset, time)
    }
  )
}

# *** Prophylaxis coverage estimation

# Estimate % left unprotected by prophylaxis strategy
outbreak.calc.unprotected = function(strategies) {
  function(model, params, time) {
    # Get model predictions for given (randomized) param values
    time = time + 0.5
    predictors = model %>% predict(data.frame(time=time), type="lpmatrix")
    fit = predictors %*% params

    # Map spline fit back to data
    fit = fit %>% model$family$linkinv()

    # Calculate total # of unprotected cases for each strategy
    unprotected = as.data.frame(lapply(strategies, function(strat) {
      sum(fit * (1 - sapply(time, strat)))
    }))

    # Calculate total # of cases
    total = sum(fit)

    unprotected %>%
      rename_all(funs(
        sprintf("%s.count", .)
      )) %>%
      cbind(
        (unprotected / total) %>%
          rename_all(funs(
            sprintf("%s.frac", .)
          ))
      ) %>%
      mutate(total=total)
  }
}

# *** State-level all-years analysis

stateObs = dataset %>%
  rename(time=weekofyear) %>%
  group_by(time) %>%
  summarize(rsv=sum(rsv)) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / sum(rsv)) %>%
  as.data.frame()

stateModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=stateObs)

statePred = stateModel %>%
  predict(type="response", newdata=data.frame(time=modelTime), se.fit=TRUE)

statePred = modelTime %>%
  cbind(data.frame(rsv.fit=statePred$fit, rsv.fit.se=statePred$se.fit)) %>%
  cbind(stateModel %>%
          outbreak.predict.timeseries(modelTime, outbreak.calc.cum(), nsim=simulations)
  ) %>%
  rename(rsv.cum.fit.lower=lower, rsv.cum.fit.upper=upper, rsv.cum.fit=median)

stateThresholds = stateModel %>%
  outbreak.predict.scalars(
    modelTime,
    outbreak.calc.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold),
    nsim=simulations
  )

# *** Regional all-years analysis

obsByCounty = dataset %>%
  rename(time=weekofyear) %>%
  group_by(county, time) %>%
  summarize(rsv=sum(rsv)) %>%
  left_join(rsvCountyGroups, by="county") %>%
  group_by(time) %>%
  do(summarizeCountyGroups(.)) %>%
  group_by(county) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / sum(rsv)) %>%
  as.data.frame()

predByCounty = data.frame()
thresholdsByCounty = data.frame()
fixedStratUnprotectedByCounty = data.frame()

for (c in levels(obsByCounty$county)) {
  countyObs = obsByCounty %>% filter(county==c)
  countyModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=countyObs)

  countyPred = countyModel %>%
    predict(type="response", newdata=data.frame(time=modelTime), se.fit=TRUE)

  countyPred = data.frame(time=modelTime) %>%
    cbind(data.frame(rsv.fit=countyPred$fit, rsv.fit.se=countyPred$se.fit)) %>%
    cbind(countyModel %>%
            outbreak.predict.timeseries(
              modelTime,
              outbreak.calc.cum(),
              nsim=simulations
            )
    ) %>%
    rename(rsv.cum.fit.lower=lower, rsv.cum.fit.upper=upper, rsv.cum.fit=median) %>%
    mutate(county=factor(c, levels=levels(obsByCounty$county)))

  predByCounty = predByCounty %>% rbind(countyPred)

  countyThresholds = countyModel %>%
    outbreak.predict.scalars(
      modelTime,
      outbreak.calc.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold),
      nsim=simulations
    ) %>%
    mutate(county=factor(c, levels=levels(obsByCounty$county)))

  thresholdsByCounty = thresholdsByCounty %>% rbind(countyThresholds)

  countyUnprotected = countyModel %>%
    outbreak.predict.scalars(
      modelTime,
      outbreak.calc.unprotected(ppxFixedStrategies(c)),
      nsim=simulations
    ) %>%
    mutate(county=factor(c, levels=levels(obsByCounty$county)))

  fixedStratUnprotectedByCounty = fixedStratUnprotectedByCounty %>% rbind(countyUnprotected)
}

unprotectedByCounty = fixedStratUnprotectedByCounty %>%
  select(county, contains("frac")) %>%
  melt("county") %>%
  mutate(
    strat=gsub("([^.]*)\\.(.*)", "\\1", variable),
    variable=gsub("([^.]*)\\.(.*)", "\\2", variable)
  ) %>%
  filter(!(county=="all" & grepl("county", strat))) %>%
  mutate(strat=as.factor(strat)) %>%
  cast(county + strat ~ variable) %>%
  mutate(
    frac.lower=1 - frac.lower,
    frac.upper=1 - frac.upper,
    frac.median=1 - frac.median,
    strat = factor(strat, names(ppxFixedStrategies(NULL)))
  )

# *** State-level sliding window analysis

stateObsByWindow = dataset %>%
  mutate(time=weeki) %>%
  group_by(time, year, weekofyear) %>%
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
  group_by(year) %>%
  mutate(rsv.cum=cumsum(rsv), rsv.cum.frac=rsv.cum / sum(rsv)) %>%
  select(-time) %>%
  rename(time=weekofyear) %>%
  filter(year >= min(rsvYears) + slidingWindowDuration) %>%
  as.data.frame()

rsvWindowYears = unique(stateObsByWindow$year)

statePredByWindow = data.frame()
stateThresholdsByWindow = data.frame()

for (y in rsvWindowYears) {

  stateYearObs = stateObsByWindow %>% filter(year==y)
  stateYearModel = gam(rsv ~ s(time, k=5, bs="cp", m=3), family=poisson, data=stateYearObs)

  stateYearPred = stateYearModel %>%
    predict(type="response", newdata=data.frame(time=modelTime), se.fit=TRUE)

  stateYearPred = modelTime %>%
    cbind(data.frame(rsv.fit=stateYearPred$fit, rsv.fit.se=stateYearPred$se.fit)) %>%
    cbind(stateYearModel %>% outbreak.predict.timeseries(modelTime, outbreak.calc.cum())) %>%
    rename(rsv.cum.fit.lower=lower, rsv.cum.fit.upper=upper, rsv.cum.fit=median) %>%
    mutate(year=y)

  statePredByWindow = statePredByWindow %>% rbind(stateYearPred)

  stateYearThresholds = stateYearModel %>%
    outbreak.predict.scalars(
      modelTime,
      outbreak.calc.thresholds(onset=seasonThreshold, offset=1 - seasonThreshold)
    ) %>%
    mutate(year=y)

  stateThresholdsByWindow = stateThresholdsByWindow %>% rbind(stateYearThresholds)
}

# *** Season drift analysis

onsetByYear = lm(onset.median ~ year, data=stateThresholdsByWindow)
offsetByYear = lm(onset.median ~ year, data=stateThresholdsByWindow)
