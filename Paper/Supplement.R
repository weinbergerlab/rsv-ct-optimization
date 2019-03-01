tollandObs = obsByCounty %>% filter(county=="Tolland")
tollandModel = gam(rsv ~ s(time, k=20, bs="cp", m=3), family=poisson, data=tollandObs)

ggplot(tollandObs) +
    theme_light(base_size=plotTextBaseSize) +
    geom_point(aes(x=time, y=rsv.cum)) +
    scale_x_continuous(breaks=monthBoundaries$mid, labels=epiWeekLabels, limits=range(c(monthBoundaries$min, monthBoundaries$max)), expand=c(0, 0)) + 
    theme(
        panel.grid.major.x=element_blank(),
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_text(angle=90, hjust=0.5, vjust=0.5),
        legend.position="bottom"
    )

tollandNsim = 5

randomMVN = function(mu, sig, nsim) {
  L = mroot(sig)
  m = ncol(L)
  t(mu + L %*% matrix(rnorm(m * nsim), m, nsim))
}

tollandParams = randomMVN(coef(tollandModel), tollandModel$Vp, tollandNsim)

tollandPred = tollandParams %>%
  apply(1, function(params) { outbreak.calc.cum(1)(tollandModel, params, modelTime) } ) %>%
  data.frame() %>%
  cbind(time=modelTime) %>%
  melt(c("time")) %>%
  mutate(variable=as.numeric(substr(variable, 2, 2))) %>%
  rename(sim=variable, rsv.cum.frac=value)

ggplot(tollandPred) +
  theme_light(base_size=plotTextBaseSize) +
  geom_point(data=tollandObs, aes(x=time, y=rsv.cum.frac)) +
  geom_line(aes(x=time, y=rsv.cum.frac, group=sim)) +
  geom_segment(aes(x=-Inf, y=seasonThreshold, xend=+Inf, yend=seasonThreshold), linetype="11", data=data.frame(), size=.375) + 
  scale_x_continuous(breaks=monthBoundaries$mid, labels=epiWeekLabels, limits=range(c(monthBoundaries$min, monthBoundaries$max)), expand=c(0, 0)) + 
  theme(
    panel.grid.major.x=element_blank(),
    legend.title=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_text(angle=90, hjust=0.5, vjust=0.5),
    legend.position="bottom"
  )

tollandOnset = tollandParams %>%
  apply(1, function(params) { outbreak.calc.thresholds(seasonThreshold, 1-seasonThreshold)(tollandModel, params, modelTime) } ) %>%
  bind_rows() %>%
  select(onset)

zoomedStartWeek = min(monthBoundaries$min) + 11
zoomedEndWeek = zoomedStartWeek + 13

ggplot(tollandPred) +
  theme_light(base_size=plotTextBaseSize) +
  geom_line(aes(x=time, y=rsv.cum.frac, group=sim), color="gray") +
  geom_segment(aes(x=-Inf, y=seasonThreshold, xend=+Inf, yend=seasonThreshold), linetype="11", data=data.frame(), size=.375) + 
  scale_y_continuous(limits=c(0, 2*seasonThreshold)) + 
  scale_x_continuous(breaks=monthBoundaries$mid, labels=epiWeekLabels, limits=c(zoomedStartWeek, zoomedEndWeek), expand=c(0, 0)) + 
  geom_point(data=tollandOnset, aes(x=onset, y=seasonThreshold)) + 
  geom_segment(data=tollandOnset, aes(x=onset, xend=onset, y=seasonThreshold, yend=0), linetype="11") + 
  theme(
    panel.grid.major.x=element_blank(),
    legend.title=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_text(angle=90, hjust=0.5, vjust=0.5),
    legend.position="bottom"
  )

tollandNsimMany = 5000
tollandParamsMany = randomMVN(coef(tollandModel), tollandModel$Vp, tollandNsimMany)

tollandPredMany = tollandParamsMany %>%
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

tollandOnsetMany = tollandParamsMany %>%
  apply(1, function(params) { outbreak.calc.thresholds(seasonThreshold, 1-seasonThreshold)(tollandModel, params, modelTime) } ) %>%
  bind_rows() %>%
  select(onset)

ggplot(tollandPredMany) +
  theme_light(base_size=plotTextBaseSize) +
  geom_ribbon(aes(x=time, ymin=min, ymax=max), fill="gray", na.rm=FALSE) +
  geom_segment(aes(x=-Inf, y=seasonThreshold, xend=+Inf, yend=seasonThreshold), linetype="11", data=data.frame(), size=.375) + 
  # geom_jitter(data=tollandOnsetMany, aes(y=onset, x=seasonThreshold), width=0.005) + 
  geom_violinh(
    data=tollandOnsetMany,
    aes(x=onset, y=seasonThreshold),
    width=.01,
    fill="gray", color="black",
    trim=FALSE, draw_quantiles=c(0.025, 0.5, 0.975)
  ) +
  scale_x_continuous(breaks=monthBoundaries$mid, labels=epiWeekLabels, expand=c(0, 0)) +
  scale_y_continuous() +
  coord_cartesian(xlim=c(zoomedStartWeek, zoomedEndWeek), ylim=c(0, 4*seasonThreshold)) +
  theme(
    panel.grid.major.x=element_blank(),
    legend.title=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_text(angle=90, hjust=0.5, vjust=0.5),
    legend.position="bottom"
  )

