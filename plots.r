library(redist)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)


data(fl250)
dataset <- iowa
map = redist_map(dataset, existing_plan = "cd_2010", pop_tol=0.01, total_pop = pop)

redist.plot.map(map, fill = dem_08 / (dem_08 + rep_08)) + 
  scale_fill_gradient2(midpoint=mid, mid="white" , low="red", high="blue", space ="Lab") +
  labs(color="Democract vote share", title = "Democrat vote per precinct")

plans.smc = redist_smc(map, nsims=1000, verbose=TRUE)
plans.mergesplit = redist_mergesplit(map, nsims=1000, verbose=TRUE)
plans.flip = redist_flip(map, nsims=1000, verbose=TRUE)
plans.enum = redist.enumpart()


redist.plot.plans(plans.smc, draws=c("1", "2"),
                  geom=map)
redist.plot.plans(plans.mergesplit, draws=c("1", "2"),
                  geom=map)
redist.plot.plans(plans.smc, draws=c("1", "2"),
                  geom=map)

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

metrics <- bind_rows(metrics.flip, metrics.mergesplit, metrics.smc)


ggplot(metrics, aes(x = EffGap, fill=method)) + 
  geom_density(alpha=0.4) +
  geom_vline(aes(xintercept=EffGap[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  labs(title="Efficiency Gap distribution on the sampling space") +
  xlab("Efficiency Gap") + xlim(-0.17, -0.15) +
  ylab("Density") 

ggplot(metrics, aes(x = DVS, fill=method)) + 
  geom_density(alpha=0.4) +
  geom_vline(aes(xintercept=DVS[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  labs(title="Democratic Vote Share distribution on the sampling space") +
  xlab("DVS") +
  ylab("Density")

ggplot(metrics, aes(x = Declination, fill=method)) + 
  geom_density(alpha=0.4) +
  geom_vline(aes(xintercept=Declination[Reduce("&", list(draw == "cd_2010", district==1, method=="Flip"))], color="cd_2010"), linetype="solid") +
  scale_color_manual(name = "Plans", values=c(cd_2010="red")) +
  labs(title="Declination distribution on the sampling space") +
  xlab("Declination") +
  ylab("Density")

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


extract.from.map.fl <- function(plans, map){
  df <- plans %>%
    rename(pop=total_pop) %>%
    mutate(dem = group_frac(map, obama * obama, obama)) %>%
    mutate(dem = group_frac(map, mccain * mccain, mccain))
}
