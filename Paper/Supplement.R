# ---- supplement_libraries ----
library(ggstance)
library(gridExtra)

# ---- supplement ----
tollandObs = obsByCounty %>% filter(county=="Tolland")
tollandModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=tollandObs)

randomMVN = function(mu, sig, nsim) {
  L = mroot(sig)
  m = ncol(L)
  t(mu + L %*% matrix(rnorm(m * nsim), m, nsim))
}

tollandCalcPred = function(tollandParams, tollandNsim) {
  tollandParams %>%
    apply(1, function(params) { outbreak.calc.cum(1)(tollandModel, params, modelTime) } ) %>%
    data.frame() %>%
    setnames(as.character(seq(1:tollandNsim))) %>%
    cbind(time=modelTime) %>%
    melt(c("time")) %>%
    rename(sim=variable, rsv.cum.frac=value) %>%
    mutate(sim=as.numeric(sim))  
}

tollandCalcOnset = function(tollandParams) {
  tollandParams %>% 
    apply(1, function(params) { outbreak.calc.thresholds(seasonThreshold, 1-seasonThreshold)(tollandModel, params, modelTime) } ) %>%
    bind_rows() %>%
    select(onset)
}

tollandStrat = ppxFixedStrategies("all")[["aap"]]

tollandCalcFraction = function(tollandPred) {
  tollandPred %>%
    mutate(ppx=tollandStrat(time)) %>%
    filter(ppx > 0) %>%
    group_by(sim) %>%
    do((function(df) {
      df = df %>% arrange(time)
      data.frame(
        ppx.start=min(df$time),
        ppx.end=max(df$time),
        unprotected.start=first(df$rsv.cum.frac),
        unprotected.end=last(df$rsv.cum.frac)
      ) %>% mutate(
        unprotected=unprotected.end-unprotected.start
      )
    })(.)) %>%
    ungroup() %>%
    as.data.frame()  
}

tollandParamsSingle = randomMVN(coef(tollandModel), tollandModel$Vp, 1)
tollandPredSingle = tollandParamsSingle %>% tollandCalcPred(1)
tollandFractionSingle = tollandPredSingle %>% tollandCalcFraction()

tollandNsimMini = 5
tollandParamsMini = randomMVN(coef(tollandModel), tollandModel$Vp, tollandNsimMini)
tollandPredMini = tollandParamsMini %>% tollandCalcPred(tollandNsimMini)
tollandOnsetMini = tollandParamsMini %>% tollandCalcOnset()

zoomedStartWeek = min(monthBoundaries$min) + 5
zoomedEndWeek = zoomedStartWeek + 21

tollandNsimFull = simulations
tollandParamsFull = randomMVN(coef(tollandModel), tollandModel$Vp, tollandNsimFull)
tollandPredFull = tollandParamsFull %>% tollandCalcPred(tollandNsimFull)
tollandOnsetFull = tollandParamsFull %>% tollandCalcOnset()
tollandFractionFull = tollandPredFull %>% tollandCalcFraction()

tollandDisplayNsim = 100