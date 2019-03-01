# ---- supplement_libraries ----
library(ggstance)

# ---- supplement ----
tollandObs = obsByCounty %>% filter(county=="Tolland")
tollandModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=tollandObs)

tollandNsimMini = 5

randomMVN = function(mu, sig, nsim) {
  L = mroot(sig)
  m = ncol(L)
  t(mu + L %*% matrix(rnorm(m * nsim), m, nsim))
}

tollandParamsMini = randomMVN(coef(tollandModel), tollandModel$Vp, tollandNsimMini)

tollandPredMini = tollandParamsMini %>%
  apply(1, function(params) { outbreak.calc.cum(1)(tollandModel, params, modelTime) } ) %>%
  data.frame() %>%
  cbind(time=modelTime) %>%
  melt(c("time")) %>%
  mutate(variable=as.numeric(substr(variable, 2, 2))) %>%
  rename(sim=variable, rsv.cum.frac=value)

tollandOnsetMini = tollandParamsMini %>%
  apply(1, function(params) { outbreak.calc.thresholds(seasonThreshold, 1-seasonThreshold)(tollandModel, params, modelTime) } ) %>%
  bind_rows() %>%
  select(onset)

zoomedStartWeek = min(monthBoundaries$min) + 5
zoomedEndWeek = zoomedStartWeek + 21

tollandNsimFull = simulations
tollandParamsFull = randomMVN(coef(tollandModel), tollandModel$Vp, tollandNsimFull)

tollandPredFull = tollandParamsFull %>%
  apply(1, function(params) { outbreak.calc.cum(1)(tollandModel, params, modelTime) } ) %>%
  data.frame() %>%
  cbind(time=modelTime) %>%
  melt(c("time")) %>%
  mutate(variable=as.numeric(substr(variable, 2, 2))) %>%
  rename(sim=variable, rsv.cum.frac=value) %>%
  group_by(time) %>%
  summarize(
    min=min(rsv.cum.frac), 
    max=max(rsv.cum.frac)
  )

tollandOnsetFull = tollandParamsFull %>%
  apply(1, function(params) { outbreak.calc.thresholds(seasonThreshold, 1-seasonThreshold)(tollandModel, params, modelTime) } ) %>%
  bind_rows() %>%
  select(onset)

zoomedStartWeekFull = min(monthBoundaries$min) + 5
zoomedEndWeekFull = zoomedStartWeekFull + 21

