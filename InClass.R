library(brms)
library(bayesplot)
library(ggplot2)
library(marginaleffects)
library(dplyr)


sensorinfo <- read.csv("sensorinfo.csv")

recordings <- read.csv("recordings.csv")

combined_dat <- merge(recordings,sensorinfo, by="sensorid", all=TRUE)

#good to have two varying intercepts to account for days where maybe no boats were out because it was too windy
#or sensors

##activity model for tot songs: Q 2 part 1
activity.mod <- brm(
  totsongs ~ boatactivity + waterdepth + distshore + (1 | sensorid) + (1 | dayid),
  data = combined_dat,
  family = negbinomial()
)
summary(activity.mod)
plot(activity.mod)
pp_check(activity.mod)
bayes_R2(activity.mod)
mcmc_plot(activity.mod)
conditional_effects(activity.mod)

###activity model for song length: Q 2 part 2
activity.mod2 <- brm(
  songlength ~ boatactivity + waterdepth + distshore + (1 | sensorid) + (1 | dayid),
  data = combined_dat,
  family = "Gamma"(link = "log")
)
summary(activity.mod2)
plot(activity.mod2)
pp_check(activity.mod2)
bayes_R2(activity.mod2)
mcmc_plot(activity.mod2)
conditional_effects(activity.mod2)


##noise model for tot songs: Q1 part 1
noise.mod <- brm(
  totsongs ~ boatnoise + waterdepth + distshore + (1 | sensorid) + (1 | dayid),
  data = combined_dat,
  family = negbinomial()
)
summary(noise.mod)
plot(noise.mod)
pp_check(noise.mod)
bayes_R2(noise.mod)
mcmc_plot(noise.mod)
conditional_effects(noise.mod)

##noise model for song duration: Q1 part 2
noise.mod2 <- brm(
  songlength ~ boatnoise + waterdepth + distshore + (1 | sensorid) + (1 | dayid),
  data = combined_dat,
  family = "Gamma"(link = "log")
)
summary(noise.mod2)
plot(noise.mod2)
pp_check(noise.mod2)
bayes_R2(noise.mod2)
mcmc_plot(noise.mod2)
conditional_effects(noise.mod2)