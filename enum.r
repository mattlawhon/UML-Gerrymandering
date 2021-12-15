library(redist)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

data("fl25")

pop_tol <- 0.25
n <- 4
mid <- 0.5
map = redist_map(fl25, ndists = n, pop_tol=pop_tol, total_pop = pop)
redist.plot.map(map, fill = obama / (obama + mccain)) + 
  scale_fill_gradient2(midpoint=mid, mid="white" , low="red", high="blue", space ="Lab") +
  labs(color="Democract vote share", title = "Democrat vote per precinct")

plans.smc = redist_smc(map, nsims=10000, verbose=TRUE)
plans.mergesplit = redist_mergesplit(map, nsims=10000, verbose=TRUE)
plans.flip = redist_flip(map, nsims=10000, verbose=TRUE)
fl25_enum <- redist.enumpart(map$adj,
                              "data/unordered",
                              "data/ordered",
                              "data/fl25_enum",
                              weight_path = "data/iowa_enum_pop",
                              ndists = n,
                              all = TRUE,
                              lower = (1 - pop_tol) * sum(map$pop) / n,
                              upper = (1 + pop_tol) * sum(map$pop) / n,
                              total_pop = map$pop)
plans.enum = fl25_enum$plans[, fl25_enum$parity < pop_tol]

plans.clustering <- as.matrix(read.table("data/FLORIDA_25_FIXED")) + 1
deviation <- function(col) {
  max(
    abs(sum(map$pop[col == 1]) * n / sum(map$pop) - 1),
    abs(sum(map$pop[col == 2]) * n / sum(map$pop) - 1),
    abs(sum(map$pop[col == 3]) * n / sum(map$pop) - 1),
    abs(sum(map$pop[col == 4]) * n / sum(map$pop) - 1)
  )
}
plans.clustering <- plans.clustering[, apply(plans.clustering, 2, deviation) < pop_tol]
metrics.clustering <- redist.metrics(plans.clustering,
                               c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                               map$mccain,
                               map$obama)
metrics.clustering$method <- "Ours"
metrics.clustering$draw <- unlist(lapply(metrics.clustering$draw, factor))
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

metrics <- bind_rows(metrics.clustering, metrics.flip, metrics.mergesplit, metrics.smc, metrics.enum)

ggplot(metrics, aes(x = EffGap, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1,"Ours"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Efficiency Gap distribution on the sampling space") +
  xlab("Efficiency Gap") + xlim(-0.1, 0.45) +
  ylab("Density") 

ggplot(metrics, aes(x = DVS, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1,"Ours"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Democratic Vote Share distribution on the sampling space") +
  xlab("DVS") + xlim(0.3, 0.65) +
  ylab("Density")

ggplot(metrics, aes(x = Declination, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1,"Ours"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Declination distribution on the sampling space") +
  xlab("Declination") + xlim(-0.2, 0.45)
  ylab("Density")


