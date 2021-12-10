library(redist)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

data("fl25_enum")
data("fl25")

map = redist_map(fl25, ndists = 3, pop_tol=0.1, total_pop = pop)
redist.plot.map(map, fill = obama / (obama + mccain)) + 
  scale_fill_gradient2(midpoint=mid, mid="white" , low="red", high="blue", space ="Lab") +
  labs(color="Democract vote share", title = "Democrat vote per precinct")

plans.smc = redist_smc(map, nsims=5000, verbose=TRUE)
plans.mergesplit = redist_mergesplit(map, nsims=5000, verbose=TRUE)
plans.flip = redist_flip(map, nsims=5000, verbose=TRUE)
plans.enum <- fl25_enum$plans[, fl25_enum$pop_dev < 0.1]

metrics.enum <- redist.metrics(plans.enum,
                              c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                              map$mccain,
                              map$obama)
metrics.enum$method <- "Enum"
metrics.enum$draw <- unlist(lapply(metrics.enum$draw, factor))
metrics.smc <- redist.metrics(plans.smc,
                              c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                              map$mccain,
                              map$obama)
metrics.smc$method <- "SMC"
metrics.mergesplit <- redist.metrics(plans.mergesplit,
                                     c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                                     map$mccain,
                                     map$obama)
metrics.mergesplit$method = "MergeSplit"
metrics.flip <- redist.metrics(plans.flip,
                               c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                               map$mccain,
                               map$obama)
metrics.flip$method <- "Flip"

metrics <- bind_rows(metrics.flip, metrics.mergesplit, metrics.smc, metrics.enum)

ggplot(metrics, aes(x = EffGap, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Efficiency Gap distribution on the sampling space",
       subtitle = "Big anomaly in MergeSplit out of the plotting range") +
  xlab("Efficiency Gap") + xlim(-0.1, 0.2) +
  ylab("Density") 

ggplot(metrics, aes(x = DVS, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Democratic Vote Share distribution on the sampling space") +
  xlab("DVS") + xlim(0.3, 0.6)
  ylab("Density")

ggplot(metrics, aes(x = Declination, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Declination distribution on the sampling space") +
  xlab("Declination") + xlim(-0.2, 0.07) +
  ylab("Density")


