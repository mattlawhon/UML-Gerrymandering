library(redist)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)


data(iowa)
dataset <- iowa
map = redist_map(dataset, existing_plan = "cd_2010", pop_tol=0.01, total_pop = pop)

mid=0.5
redist.plot.map(map, fill = dem_08 / (dem_08 + rep_08)) + 
  scale_fill_gradient2(midpoint=mid, mid="white" , low="red", high="blue", space ="Lab") +
  labs(color="Democract vote share", title = "Democrat vote per precinct")

plans.smc = redist_smc(map, nsims=10000, verbose=TRUE)
plans.mergesplit = redist_mergesplit(map, nsims=10000, verbose=TRUE)
plans.flip = redist_flip(map, nsims=10000, verbose=TRUE)
iowa_enum = redist.enumpart(map$adj,
                            "data/unordered",
                            "data/ordered",
                            "data/iowa_enum",
                            weight_path = "data/iowa_enum_pop",
                            ndists = 4,
                            all = FALSE,
                            n = 1000000,
                            total_pop = map$pop)
plans.enum = iowa_enum$plans[, abs(iowa_enum$parity - 1) < 0.01]

redist.plot.plans(plans.smc, draws=c("1", "2"),
                  geom=map)
redist.plot.plans(plans.mergesplit, draws=c("1", "2"),
                  geom=map)
redist.plot.plans(plans.flip, draws=c("1", "2"),
                  geom=map)

metrics.enum <- redist.metrics(plans.enum,
                               c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                               map$rep_08,
                               map$dem_08)
metrics.enum$method <- "Enum"
metrics.enum$draw <- unlist(lapply(metrics.enum$draw, factor))
metrics.smc <- redist.metrics(plans.smc,
                          c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                          map$rep_08,
                          map$dem_08)
metrics.smc$method = "SMC"
metrics.mergesplit <- redist.metrics(plans.mergesplit,
                              c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                              map$rep_08,
                              map$dem_08)
metrics.mergesplit$method = "MergeSplit"
metrics.flip <- redist.metrics(plans.flip,
                              c("EffGap", "DSeats", "Bias", "Responsiveness", "DVS", "Declination"),
                              map$rep_08,
                              map$dem_08)
metrics.flip$method = "Flip"

metrics <- bind_rows(metrics.flip, metrics.mergesplit, metrics.smc, metrics.enum)


ggplot(metrics, aes(x = EffGap, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Efficiency Gap distribution on the sampling space") +
  geom_vline(aes(xintercept=EffGap[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  xlab("Efficiency Gap") +
  ylab("Density") + ylim(0,100)

ggplot(metrics, aes(x = DVS, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Democratic Vote Share distribution on the sampling space") +
  geom_vline(aes(xintercept=DVS[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  xlab("DVS") +
  ylab("Density")

ggplot(metrics, aes(x = Declination, linetype=factor(method), alpha=factor(method), fill=factor(method))) + 
  geom_density() +
  scale_alpha_manual(values = c("Enum"=0.8, "Flip"=0.1, "MergeSplit"=0.1, "SMC"=0.1) ) +
  labs(alpha="Algorithm", linetype="Algorithm", fill="Algorithm",
       title="Declination distribution on the sampling space") +
  geom_vline(aes(xintercept=Declination[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  xlab("Declination") + 
  ylab("Density") + ylim(0, 30)

ggplot(metrics, aes(x = Bias, fill=method)) + 
  geom_histogram() +
  geom_vline(aes(xintercept=Bias[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  labs(title="Bias distribution on the sampling space") +
  xlab("Bias") +
  ylab("number of plans")

ggplot(metrics, aes(x = DSeats, fill=method)) + 
  geom_histogram() +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  labs(title="Democract seats distribution on the sampling space") +
  xlab("Democrat seats") + scale_x_continuous(breaks= pretty_breaks(n=1)) +
  ylab("number of plans")


