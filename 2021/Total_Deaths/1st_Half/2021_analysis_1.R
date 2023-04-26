devtools::install_github('ImperialCollegeLondon/epidemia', ref='exponential_new')
library(here)
library(epidemia)
library(tidyverse)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(scales)
library(EnvStats)
library(gridExtra)
source("plot_with_eta.R")
library(dplyr)
library(arm)

### death data

data<-readRDS("dat_21_1_15Nov.rds")
dat<- readRDS("dat_21_1_15Nov.rds")
##
o2d<-function(){
  i2o <- EuropeCovid$obs$deaths$i2o
  shape1 <- 62.22; scale1 <- 0.06; # infection to onset https://www.acpjournals.org/doi/10.7326/M20-0504
  shape2 <- 1.45 ; scale2 <- 10.43 # using estimated of onset to death from chess data
  x1 <- rgamma(1e6,shape=shape1,scale=scale1) # infection-to-onset distribution
  x2 <- rgamma(1e6,shape=shape2,scale=scale2) # infection-to-onset distribution
  
  f_cached <- ecdf(x1+x2) # empirical cumulative distribtion function
  convolution = function(u) f_cached(u)
  f = rep(NA,length(EuropeCovid$obs$deaths$i2o)) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0)) # first entry
  for(i in 2:length(EuropeCovid$obs$deaths$i2o)) { # all other entries
    f[i] = (convolution(i+.5) - convolution(i-.5))
  }
  return(f)
}


dat <- pivot_longer(dat, cols = -Date, names_to = 'country', values_to = 'deaths')
dat$deaths[is.na(dat$deaths)]=0 # to have all time series starting on the same day 30th Jan 2020
dat <- drop_na(dat) # to drop NAs just incase
dat <- mutate(dat, Date = as.Date(Date, format='%d/%m/%Y'))
dat <- rename(dat, date=Date)
dat <- filter(dat, date <= as.Date("2021-12-31")) %>% arrange(country, date) # only use data to July
i2o <- o2d()

deaths <- epiobs(
  formula = deaths(country, date) ~ 1,
  prior_intercept = rstanarm::normal(location=0.39,scale = 0.225),
  prior_aux = rstanarm::normal(location=10, scale=2),
  link="identity",
  i2o=i2o * 0.01,
)
args<-NULL
args$data <- dat
args$algorithm <- "sampling"
args$obs <- list(deaths=deaths)
args$sampling_args <- list(iter=1000, seed=12345, control=list(adapt_delta=0.95,max_treedepth=15))
# for rt just using a weekly random walk for each country with a separate R0
args$rt <- epirt(
  formula = R(country, date) ~ 0 + country + rw(time = week, gr=country, prior_scale = 0.1),
  prior = rstanarm::normal(log(2), 0.1)
)
args$prior_tau = rstanarm::exponential(rate = 1)
# make sure for peridod before week of 13th March has same weekly index (same Rt)
w=rep(c(1:(round(nrow(data))/7+1)),each=7)
w=w[c(1:nrow(data))]
args$data$week <- as.character(rep(w,times=5))

nrow(data)
# start random walk on a given date
#args$data <- mutate(
#args$data,
#week = replace(week, which(week <= 11), NA)
#)
country=c("Bangladesh","India","Pakistan","Nepal","Sri_Lanka")
pop=c(163e6,1366e6,216.6e6,29.14e6,21.92e6)
popu=cbind.data.frame(country,pop)
args$pop=popu


args$pop_adjust <- F
args$si=EuropeCovid$si


fit=readRDS("fit_2021_1_urfd_15Nov.rds")
# original fit
fit$pop_adjust <- TRUE
fit_orig <- fit
plot_obs(fit,type = "deaths")

# date from which we will change Rt
changeDate <- as.Date('2021-01-01')
# get index for random walks
nms <- colnames(as.matrix(fit_orig))
idx_ban <- grep("^R\\|rw.*Bangladesh", nms)
idx_ind <- grep("^R\\|rw.*India", nms)
idx_pak <- grep("^R\\|rw.*Pakistan", nms)
idx_nep <- grep("^R\\|rw.*Nepal", nms)
idx_sri <- grep("^R\\|rw.*Sri_Lanka", nms)
#get index for R0
idx_ban_R0 <- grep("^R\\|countryBangladesh", nms)
idx_ind_R0 <- grep("^R\\|countryIndia", nms)
idx_pak_R0 <- grep("^R\\|countryPakistan", nms)
idx_nep_R0 <- grep("^R\\|countryNepal", nms)
idx_sri_R0 <- grep("^R\\|countrySri_Lanka", nms)
#get index for seeds
idx_ban_seeds <- grep("seeds\\[Bangladesh\\]", nms)
idx_ind_seeds <- grep("seeds\\[India\\]", nms)
idx_pak_seeds <- grep("seeds\\[Pakistan\\]", nms)
idx_nep_seeds <- grep("seeds\\[Nepal\\]", nms)
idx_sri_seeds <- grep("seeds\\[Sri_Lanka\\]", nms)
nchains <- length(fit_orig$stanfit@sim$samples)

mat <- as.matrix(fit)
# compute orderings for the draws
order_ban <- order(mat[, idx_ban_R0])
order_ind <- order(mat[, idx_ind_R0])
order_pak <- order(mat[, idx_pak_R0])
order_nep <- order(mat[, idx_nep_R0])
order_sri <- order(mat[, idx_sri_R0])

banSeeds = mat[,idx_ban_seeds]
indSeeds = mat[,idx_ind_seeds]
pakSeeds = mat[,idx_pak_seeds]
nepSeeds = mat[,idx_nep_seeds]
sriSeeds = mat[,idx_sri_seeds]

banR=exp(mat[,idx_ban_R0])
indR=exp(mat[,idx_ind_R0])
pakR=exp(mat[,idx_pak_R0])
nepR=exp(mat[,idx_nep_R0])
sriR=exp(mat[,idx_sri_R0])

#pdf(here('whatif/figures/R0_ratios.pdf'))
#par(mfrow=c(3,3))
#hist(indR,main="R0 India")
# hist(PakR,main="R0 Pak")
# hist(dnkR,main="R0 India")
# hist(swnR/PakR,main="R0 ratio Bangladesh/Pak")
# hist(PakR/swnR,main="R0 ratio Pak/Bangladesh")
# hist(dnkR/swnR,main="R0 ratio India/Bangladesh")
# hist(swnR/dnkR,main="R0 ratio Bangladesh/India")
# hist(PakR/dnkR,main="R0 ratio Pak/India")
# hist(dnkR/PakR,main="R0 ratio India/Pak")
# dev.off()
#
# pdf(here('whatif/figures/PairPlot_R0_seeds.pdf'))
# par(mfrow=c(1,3))
# plot(dnkR,dnkSeeds,pch=16,main="India",xlab="R0",ylab="Seeds")
# plot(PakR,PakSeeds,pch=16,main="Pak",xlab="R0",ylab="Seeds")
# plot(swnR,swnSeeds,pch=16,main="Bangladesh",xlab="R0",ylab="Seeds")
# dev.off()
#
#
# pdf(here('whatif/figures/R0_prior.pdf'))
# hist(exp(rnorm(1e6,log(3.5),0.1)),100,main="Prior distribution on R0",xlab="R0",ylab="Density",col='red')
# dev.off()

# generate counterfactuals
e_orig <- posterior_linpred(fit)
e_orig$group
## absolute
e1 <- e_orig
w <- e_orig$time >= changeDate

e1$draws[order_ind, w & (e1$group == "India")] <- e_orig$draws[order_ban, w & (e_orig$group == "Bangladesh")]
e1$draws[order_ban, w & (e1$group == "Bangladesh")] <- e_orig$draws[order_pak, w & (e_orig$group == "Pakistan")]
e1$draws[order_pak, w & (e1$group == "Pakistan")] <- e_orig$draws[order_nep, w & (e_orig$group == "Nepal")]
e1$draws[order_nep, w & (e1$group == "Nepal")] <- e_orig$draws[order_sri, w & (e_orig$group == "Sri_Lanka")]
e1$draws[order_sri, w & (e1$group == "Sri_Lanka")] <- e_orig$draws[order_ind, w & (e_orig$group == "India")]



## absolute
e2 <- e_orig
w <- e_orig$time >= changeDate

e2$draws[order_ban, w & (e2$group == "Bangladesh")] <- e_orig$draws[order_ind, w & (e_orig$group == "India")]
e2$draws[order_pak, w & (e2$group == "Pakistan")] <- e_orig$draws[order_ban, w & (e_orig$group == "Bangladesh")]
e2$draws[order_nep, w & (e2$group == "Nepal")] <- e_orig$draws[order_pak, w & (e_orig$group == "Pakistan")]
e2$draws[order_sri, w & (e2$group == "Sri_Lanka")] <- e_orig$draws[order_nep, w & (e_orig$group == "Nepal")]
e2$draws[order_ind, w & (e2$group == "India")] <- e_orig$draws[order_sri, w & (e_orig$group == "Sri_Lanka")]

## absolute
e3 <- e_orig
w <- e_orig$time >= changeDate

e3$draws[order_ind, w & (e3$group == "India")] <- e_orig$draws[order_pak, w & (e_orig$group == "Pakistan")]
e3$draws[order_pak, w & (e3$group == "Pakistan")] <- e_orig$draws[order_sri, w & (e_orig$group == "Sri_Lanka")]
e3$draws[order_sri, w & (e3$group == "Sri_Lanka")] <- e_orig$draws[order_ban, w & (e_orig$group == "Bangladesh")]
e3$draws[order_ban, w & (e3$group == "Bangladesh")] <- e_orig$draws[order_nep, w & (e_orig$group == "Nepal")]
e3$draws[order_nep, w & (e3$group == "Nepal")] <- e_orig$draws[order_ind, w & (e_orig$group == "India")]

## absolute
e4 <- e_orig
w <- e_orig$time >= changeDate

e4$draws[order_pak, w & (e4$group == "Pakistan")] <- e_orig$draws[order_ind, w & (e_orig$group == "India")]
e4$draws[order_ind, w & (e4$group == "India")] <- e_orig$draws[order_nep, w & (e_orig$group == "Nepal")]
e4$draws[order_nep, w & (e4$group == "Nepal")] <- e_orig$draws[order_ban, w & (e_orig$group == "Bangladesh")]
e4$draws[order_ban, w & (e4$group == "Bangladesh")] <- e_orig$draws[order_sri, w & (e_orig$group == "Sri_Lanka")]
e4$draws[order_sri, w & (e4$group == "Sri_Lanka")] <- e_orig$draws[order_pak, w & (e_orig$group == "Pakistan")]




## relative
fit5=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_ind <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ind_R0]][-(1:warmup)])
  order_ban<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ban_R0]][-(1:warmup)])
  order_pak <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_pak_R0]][-(1:warmup)])
  order_nep<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nep_R0]][-(1:warmup)])
  order_sri <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_sri_R0]][-(1:warmup)])
  for (i in 1:length(idx_ind)) {
    fit5$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban]
    fit5$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban] <- fit_orig$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak]
    fit5$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep]
    fit5$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep] <- fit_orig$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri]
    fit5$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind]
  }
}


#par(mfrow=c(2,2))
#plot(indR,banR,pch=16,xlab='R0 India', ylab='R0 Bangladesh', main="Posterior India R0 against Bangladesh R0")
#temp_samples=as.matrix(fit_orig)
#plot(temp_samples[,idx_ind_R0],temp_samples[,idx_ind[1]],pch=16,xlab="R0 India",ylab="India first week random walk",main='Original samples')
#temp_samples=as.matrix(fit_corr_plot)
#plot(temp_samples[,idx_ind_R0],temp_samples[,idx_ind[1]],pch=16,xlab="R0 India",ylab="India first week random walk",main='Naive relative approach')
#temp_samples=as.matrix(fit3)
#plot(temp_samples[,idx_ind_R0],temp_samples[,idx_ind[1]],pch=16,xlab="R0 India",ylab="India first week random walk",main='Ordered relative approach')
#dev.off()

#relative
fit6=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_ind <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ind_R0]][-(1:warmup)])
  order_ban<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ban_R0]][-(1:warmup)])
  order_pak <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_pak_R0]][-(1:warmup)])
  order_nep<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nep_R0]][-(1:warmup)])
  order_sri <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_sri_R0]][-(1:warmup)])
  for (i in 1:length(idx_ind)) {
    fit6$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind] <- fit_orig$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri]
    fit6$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep]
    fit6$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind]
    fit6$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban]
    fit6$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep] <- fit_orig$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak]
  }
}

#relative
fit7=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_ind <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ind_R0]][-(1:warmup)])
  order_ban<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ban_R0]][-(1:warmup)])
  order_pak <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_pak_R0]][-(1:warmup)])
  order_nep<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nep_R0]][-(1:warmup)])
  order_sri <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_sri_R0]][-(1:warmup)])
  for (i in 1:length(idx_ind)) {
    fit7$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind] <- fit_orig$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak]
    fit7$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak] <- fit_orig$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri]
    fit7$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban]
    fit7$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep]
    fit7$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind]
  }
}

#relative
fit8=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_ind <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ind_R0]][-(1:warmup)])
  order_ban<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_ban_R0]][-(1:warmup)])
  order_pak <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_pak_R0]][-(1:warmup)])
  order_nep<- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nep_R0]][-(1:warmup)])
  order_sri <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_sri_R0]][-(1:warmup)])
  for (i in 1:length(idx_ind)) {
    fit8$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind]
    fit8$stanfit@sim$samples[[chain]][[idx_ind[i]]][order_ind] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep]
    fit8$stanfit@sim$samples[[chain]][[idx_nep[i]]][order_nep] <- fit_orig$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban]
    fit8$stanfit@sim$samples[[chain]][[idx_ban[i]]][order_ban] <- fit_orig$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri]
    fit8$stanfit@sim$samples[[chain]][[idx_sri[i]]][order_sri] <- fit_orig$stanfit@sim$samples[[chain]][[idx_pak[i]]][order_pak]
  }
}

## plotting rt
# get posterior infections
seed = 100
rt <- posterior_rt(fit_orig,seed=seed)
rt_cf1 <- posterior_rt_(fit_orig, eta=e1$draws, seed=seed)
rt_cf2 <- posterior_rt_(fit_orig, eta=e2$draws, seed=seed)
rt_cf3 <- posterior_rt_(fit_orig, eta=e3$draws, seed=seed)
rt_cf4 <- posterior_rt_(fit_orig, eta=e4$draws, seed=seed)
rt_cf5 <- posterior_rt(fit5, seed=seed)
rt_cf6 <- posterior_rt(fit6, seed=seed)
rt_cf7 <- posterior_rt(fit7, seed=seed)
rt_cf8 <- posterior_rt(fit8, seed=seed)

deaths_obs <-args$data$deaths
deaths_fit <- posterior_predict(fit_orig, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf1 <- posterior_predict_(fit_orig, eta=e1$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf2 <- posterior_predict_(fit_orig, eta=e2$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf3 <- posterior_predict_(fit_orig, eta=e3$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf4 <- posterior_predict_(fit_orig, eta=e4$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf5 <- posterior_predict(fit5, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf6 <- posterior_predict(fit6, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf7 <- posterior_predict(fit7, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf8 <- posterior_predict(fit8, type="deaths",posterior_mean=TRUE, seed=seed)

infections <- posterior_infections(fit_orig,poster_mean=TRUE, seed=seed)
infections_cf1 <- posterior_infections_(fit_orig, eta=e1$draws, posterior_mean=TRUE, seed=seed)
infections_cf2 <- posterior_infections_(fit_orig, eta=e2$draws, posterior_mean=TRUE, seed=seed)
infections_cf3 <- posterior_infections_(fit_orig, eta=e3$draws, posterior_mean=TRUE, seed=seed)
infections_cf4 <- posterior_infections_(fit_orig, eta=e4$draws, posterior_mean=TRUE, seed=seed)
infections_cf5 <- posterior_infections(fit5, posterior_mean=TRUE, seed=seed)
infections_cf6 <- posterior_infections(fit6,posterior_mean=TRUE, seed=seed)
infections_cf7 <- posterior_infections(fit7, posterior_mean=TRUE, seed=seed)
infections_cf8 <- posterior_infections(fit8,posterior_mean=TRUE, seed=seed)

rt$group <- as.character(rt$group)
rt_cf1$group <- as.character(rt_cf1$group)
rt_cf2$group <- as.character(rt_cf2$group)
rt_cf3$group <- as.character(rt_cf3$group)
rt_cf4$group <- as.character(rt_cf4$group)
rt_cf5$group <- as.character(rt_cf5$group)
rt_cf6$group <- as.character(rt_cf6$group)
rt_cf7$group <- as.character(rt_cf7$group)
rt_cf8$group <- as.character(rt_cf8$group)

df<- data.frame(
  date = rt$time,
  median = apply(rt$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections$draws, 2, function(x) quantile(x, 0.975)),
  group = rt$group
)
df_cf1 <- data.frame(
  date = rt_cf1$time,
  median = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf1$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf1$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf1$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf1$group
)
df_cf2 <- data.frame(
  date = rt_cf2$time,
  median = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf2$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf2$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf2$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf2$group
)
df_cf3 <- data.frame(
  date = rt_cf3$time,
  median = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf3$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf3$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf3$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf3$group
)
df_cf4 <- data.frame(
  date = rt_cf4$time,
  median = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf4$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf4$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf4$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf4$group
)
df_cf5 <- data.frame(
  date = rt_cf5$time,
  median = apply(rt_cf5$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf5$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf5$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf5$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf5$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf5$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf5$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf5$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf5$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf5$group
)

df_cf6 <- data.frame(
  date = rt_cf6$time,
  median = apply(rt_cf6$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf6$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf6$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf6$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf6$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf6$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf6$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf6$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf6$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf6$group
)
df_cf7 <- data.frame(
  date = rt_cf7$time,
  median = apply(rt_cf7$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf7$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf7$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf7$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf7$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf7$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf7$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf7$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf7$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf7$group
)
df_cf8 <- data.frame(
  date = rt_cf8$time,
  median = apply(rt_cf8$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf8$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf8$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf8$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf8$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf8$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf8$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf8$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf8$draws, 2, function(x) quantile(x, 0.975)),
  group = rt_cf8$group
)


df_b=cumsum(df[(df$group=="Nepal" & df$date>=changeDate),]$deaths_median)/29.14
df5_b=cumsum(df_cf5[(df_cf5$group=="Nepal" & df_cf5$date>=changeDate),]$deaths_median)/29.14
df6_b=cumsum(df_cf6[(df_cf6$group=="Nepal" & df_cf6$date>=changeDate),]$deaths_median)/29.14
df7_b=cumsum(df_cf7[(df_cf8$group=="India" & df_cf7$date>=changeDate),]$deaths_median)/1366
df8_b=cumsum(df_cf8[(df_cf8$group=="Nepal" & df_cf8$date>=changeDate),]$deaths_median)/29.14

tail(df_b,1)
tail(df5_b,1)
tail(df6_b,1)
tail(df7_b,1)
tail(df8_b,1)

df_b=cumsum(df[(df$group=="Nepal" & df$date>=changeDate),]$deaths_li)/29.14
df5_b=cumsum(df_cf5[(df_cf5$group=="Nepal" & df_cf5$date>=changeDate),]$deaths_li)/29.14
df6_b=cumsum(df_cf6[(df_cf6$group=="Nepal" & df_cf6$date>=changeDate),]$deaths_li)/29.14
df7_b=cumsum(df_cf7[(df_cf8$group=="India" & df_cf7$date>=changeDate),]$deaths_li)/1366
df8_b=cumsum(df_cf8[(df_cf8$group=="Nepal" & df_cf8$date>=changeDate),]$deaths_li)/29.14

tail(df_b,1)
tail(df5_b,1)
tail(df6_b,1)
tail(df7_b,1)
tail(df8_b,1)

df_b=cumsum(df[(df$group=="Nepal" & df$date>=changeDate),]$deaths_ui)/29.14
df5_b=cumsum(df_cf5[(df_cf5$group=="Nepal" & df_cf5$date>=changeDate),]$deaths_ui)/29.14
df6_b=cumsum(df_cf6[(df_cf6$group=="Nepal" & df_cf6$date>=changeDate),]$deaths_ui)/29.14
df7_b=cumsum(df_cf7[(df_cf8$group=="India" & df_cf7$date>=changeDate),]$deaths_ui)/1366
df8_b=cumsum(df_cf8[(df_cf8$group=="Nepal" & df_cf8$date>=changeDate),]$deaths_ui)/29.14

tail(df_b,1)
tail(df5_b,1)
tail(df6_b,1)
tail(df7_b,1)
tail(df8_b,1)




df$colour <- "Original"
df_cf1$colour <- "Scenario 1"
df_cf2$colour <- "Scenario 2"
df_cf3$colour <- "Scenario 3"
df_cf4$colour <- "Scenario 4"
df_cf5$colour <- "Scenario 5"
df_cf6$colour <- "Scenario 6"
df_cf7$colour <- "Scenario 7"
df_cf8$colour <- "Scenario 8"
df <- rbind(df, df_cf1, df_cf2, df_cf3, df_cf4,df_cf5, df_cf6, df_cf7, df_cf8)
df=df[df$date>=changeDate,]
# making sure we have renamed everything as per scenarios in papers, which is
# abs/real: donot->recpient
source("transform_df_plotting_combined.R")




date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%B")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right")
p1 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour),
    data = df[df$country%in%c("India")& !df$group%in%c("Fit Ind"),],
    size = 1,color="red",
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median),
    data = df_originals[df_originals$country%in%c("India"),],
    size = 1,color='black'
  ) +
  xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p1 <- p1 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Effective R trajectory for India (Recipient)")

ggsave("scenario-rt_Ind_1.pdf",p1,width=9,height=6)

p2 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour),
    data = df[df$country%in%c("Bangladesh")& !df$group%in%c("Fit Bang"),],
    size = 1,color="red",
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median),
    data = df_originals[df_originals$country%in%c("Bangladesh") ,],
    size = 1,color='black') + xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p2 <- p2 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group,,scales="free_y") + scale_color_brewer(palette="Paired")  + ggtitle("Effective R trajectory for Bangladesh (Recipient)")
ggsave("scenario-rt_Bang_1.pdf",p2,width=9,height=6)


p3 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour),
    data = df[df$country%in%c("Pakistan") & !df$group%in%c("Fit Pak"),],
    size = 1,color="red",
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median),
    data = df_originals[df_originals$country%in%c("Pakistan"),],
    size = 1,color='black'
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p3 <- p3 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group,,scales="free_y") + scale_color_brewer(palette="Paired")  + ggtitle("Effective R trajectory for Pakistan (Recipient)")
ggsave("scenario-rt_Pak_1.pdf",p3,width=9,height=6)


p4 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour),
    data = df[df$country%in%c("Nepal") & !df$group%in%c("Fit Nep"),],
    size = 1,color="red",
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median),
    data = df_originals[df_originals$country%in%c("Nepal"),],
    size = 1,color='black'
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p4 <- p4 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group,,scales="free_y") + scale_color_brewer(palette="Paired")  + ggtitle("Effective R trajectory for Nepal (Recipient)")
ggsave("scenario-rt_nep_1.pdf",p4,width=9,height=6)



p5 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour),
    data = df[df$country%in%c("Sri_Lanka") & !df$group%in%c("Fit Sri"),],
    size = 1,color="red",
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median),
    data = df_originals[df_originals$country%in%c("Sri_Lanka"),],
    size = 1,color='black'
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p5 <- p5 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group,,scales="free_y") + scale_color_brewer(palette="Paired")  + ggtitle("Effective R trajectory for Sri_Lanka (Recipient)")
ggsave("scenario-rt_sri_1.pdf",p5,width=9,height=6)




margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
p1=p1+margin
p2=p2+margin
p3=p3+margin
p4=p4+margin
p5=p5+margin

g_rt <- grid.arrange(p1,p2,p3,p4,p5,ncol=1)  # default settings
ggsave("scenario-rt_1.pdf",g_rt,width=11,height=11)


######################################################################################################################################################
# Infections
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right")

p1 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour),
    data = df[df$country%in%c("India"),],
    size = 1,color=mn,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))+
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui),
    data = df[df$country%in%c("India"),],
    size = 1,fill=ci,alpha=0.5,
  ) 

p1 <- p1 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("India")



p2 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour),
    data = df[df$country%in%c("Bangladesh"),],
    size = 1,color=mn,
  )+ xlim(as.Date('2021-01-01'), as.Date('2021-12-31')) #+
ggplot2::geom_ribbon(
  mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui),
  data = df[df$country%in%c("Bangladesh"),],
  size = 1,fill=ci,alpha=0.5,
) 

p2 <- p2 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Bangladesh")

p3 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour),
    data = df[df$country%in%c("Pakistan"),],
    size = 1,color=mn,
  )+ xlim(as.Date('2021-01-01'), as.Date('2021-12-31')) #+
ggplot2::geom_ribbon(
  mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui),
  data = df[df$country%in%c("Pakistan"),],
  size = 1,fill=ci,alpha=0.5,
) 

p3 <- p3 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Pakistan")
ggsave("scenario-rt_Pak.pdf",p1,width=9,height=6)
p4 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour),
    data = df[df$country%in%c("Nepal"),],
    size = 1,color=mn,
  )+ xlim(as.Date('2021-01-01'), as.Date('2021-12-31')) #+
ggplot2::geom_ribbon(
  mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui),
  data = df[df$country%in%c("Nepal"),],
  size = 1,fill=ci,alpha=0.5,
) 

p4 <- p4 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Nepal")
ggsave("scenario-rt_Nep.pdf",p1,width=9,height=6)


p5 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour),
    data = df[df$country%in%c("Sri_Lanka"),],
    size = 1,color=mn,
  )+ xlim(as.Date('2021-01-01'), as.Date('2021-12-31')) #+
ggplot2::geom_ribbon(
  mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui),
  data = df[df$country%in%c("Sri_Lanka"),],
  size = 1,fill=ci,alpha=0.5,
) 

p5 <- p5 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Sri_Lanka")
ggsave("scenario-rt_Sri.pdf",p1,width=9,height=6)


margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
p1=p1+margin
p2=p2+margin
p3=p3+margin
p4=p4+margin
p5=p5+margin
g_infections <- grid.arrange(p1,p2,p3,ncol=1)  # default settings
ggsave("scenario-infections_1.pdf",g_infections,width=11,height=11)


######################################################################################################################################################
# URF_1
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")
data_cases=data_cases[data_cases$Date>=changeDate,]
data_india=rep(cumsum(data_cases$India),9)
data_bang=rep(cumsum(data_cases$Bangladesh),9)
data_pak=rep(cumsum(data_cases$Pakistan),9)
data_nep=rep(cumsum(data_cases$Nepal),9)
data_sri=rep(cumsum(data_cases$Sri_Lanka),9)





dim_ind=nrow(df[df$country%in%c("India"),])
dim_bang=nrow(df[df$country%in%c("Bangladesh"),])
dim_pak=nrow(df[df$country%in%c("Pakistan"),])
dim_nep=nrow(df[df$country%in%c("Bangladesh"),])
dim_sri=nrow(df[df$country%in%c("Sri_Lanka"),])
df$cum_infec_med=rep(0,times=nrow(df))
df$cum_infec_li=rep(0,times=nrow(df))
df$cum_infec_ui=rep(0,times=nrow(df))

df[df$country%in%c("India"),]$cum_infec_med=
  c(cumsum(df[df$country%in%c("India"),]$infections_median[1:(dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(dim_ind/9+1):(2*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(2*dim_ind/9+1):(3*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(3*dim_ind/9+1):(4*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(4*dim_ind/9+1):(5*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(5*dim_ind/9+1):(6*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(6*dim_ind/9+1):(7*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(7*dim_ind/9+1):(8*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_median[(8*dim_ind/9+1):(dim_ind)]))

df[df$country%in%c("Bangladesh"),]$cum_infec_med=
  c(cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[1:(dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(dim_bang/9+1):(2*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(2*dim_bang/9+1):(3*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(3*dim_bang/9+1):(4*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(4*dim_bang/9+1):(5*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(5*dim_bang/9+1):(6*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(6*dim_bang/9+1):(7*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(7*dim_bang/9+1):(8*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(8*dim_bang/9+1):(dim_bang)]))

df[df$country%in%c("Pakistan"),]$cum_infec_med=
  c(cumsum(df[df$country%in%c("Pakistan"),]$infections_median[1:(dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(dim_pak/9+1):(2*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(2*dim_pak/9+1):(3*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(3*dim_pak/9+1):(4*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(4*dim_pak/9+1):(5*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(5*dim_pak/9+1):(6*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(6*dim_pak/9+1):(7*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(7*dim_pak/9+1):(8*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(8*dim_pak/9+1):(dim_pak)]))

df[df$country%in%c("India"),]$cum_infec_li=
  c(cumsum(df[df$country%in%c("India"),]$infections_li[1:(dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(dim_ind/9+1):(2*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(2*dim_ind/9+1):(3*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(3*dim_ind/9+1):(4*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(4*dim_ind/9+1):(5*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(5*dim_ind/9+1):(6*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(6*dim_ind/9+1):(7*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(7*dim_ind/9+1):(8*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_li[(8*dim_ind/9+1):(dim_ind)])
  )

df[df$country%in%c("Bangladesh"),]$cum_infec_li=
  c(cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[1:(dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(dim_bang/9+1):(2*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(2*dim_bang/9+1):(3*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(3*dim_bang/9+1):(4*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(4*dim_bang/9+1):(5*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(5*dim_bang/9+1):(6*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(6*dim_bang/9+1):(7*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(7*dim_bang/9+1):(8*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(8*dim_bang/9+1):(dim_bang)]))



df[df$country%in%c("Pakistan"),]$cum_infec_li=
  c(cumsum(df[df$country%in%c("Pakistan"),]$infections_li[1:(dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(dim_pak/9+1):(2*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(2*dim_pak/9+1):(3*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(3*dim_pak/9+1):(4*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(4*dim_pak/9+1):(5*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(5*dim_pak/9+1):(6*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(6*dim_pak/9+1):(7*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(7*dim_pak/9+1):(8*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(8*dim_pak/9+1):(dim_pak)]))

df[df$country%in%c("India"),]$cum_infec_ui=
  c(cumsum(df[df$country%in%c("India"),]$infections_ui[1:(dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(dim_ind/9+1):(2*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(2*dim_ind/9+1):(3*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(3*dim_ind/9+1):(4*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(4*dim_ind/9+1):(5*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(5*dim_ind/9+1):(6*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(6*dim_ind/9+1):(7*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(7*dim_ind/9+1):(8*dim_ind/9)]),
    cumsum(df[df$country%in%c("India"),]$infections_ui[(8*dim_ind/9+1):(dim_ind)]))

df[df$country%in%c("Bangladesh"),]$cum_infec_ui=
  c(cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[1:(dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(dim_bang/9+1):(2*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(2*dim_bang/9+1):(3*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(3*dim_bang/9+1):(4*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(4*dim_bang/9+1):(5*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(5*dim_bang/9+1):(6*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(6*dim_bang/9+1):(7*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(7*dim_bang/9+1):(8*dim_bang/9)]),
    cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(8*dim_bang/9+1):(dim_bang)]))



df[df$country%in%c("Pakistan"),]$cum_infec_ui=
  c(cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[1:(dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(dim_pak/9+1):(2*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(2*dim_pak/9+1):(3*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(3*dim_pak/9+1):(4*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(4*dim_pak/9+1):(5*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(5*dim_pak/9+1):(6*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(6*dim_pak/9+1):(7*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(7*dim_pak/9+1):(8*dim_pak/9)]),
    cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(8*dim_pak/9+1):(dim_pak)]))


df[df$country%in%c("Nepal"),]$cum_infec_med=
  c(cumsum(df[df$country%in%c("Nepal"),]$infections_median[1:(dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(dim_nep/9+1):(2*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(2*dim_nep/9+1):(3*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(3*dim_nep/9+1):(4*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(4*dim_nep/9+1):(5*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(5*dim_nep/9+1):(6*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(6*dim_nep/9+1):(7*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(7*dim_nep/9+1):(8*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_median[(8*dim_nep/9+1):(dim_nep)]))



df[df$country%in%c("Nepal"),]$cum_infec_li=
  c(cumsum(df[df$country%in%c("Nepal"),]$infections_li[1:(dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(dim_nep/9+1):(2*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(2*dim_nep/9+1):(3*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(3*dim_nep/9+1):(4*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(4*dim_nep/9+1):(5*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(5*dim_nep/9+1):(6*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(6*dim_nep/9+1):(7*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(7*dim_nep/9+1):(8*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_li[(8*dim_nep/9+1):(dim_nep)])
  )

df[df$country%in%c("Nepal"),]$cum_infec_ui=
  c(cumsum(df[df$country%in%c("Nepal"),]$infections_ui[1:(dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(dim_nep/9+1):(2*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(2*dim_nep/9+1):(3*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(3*dim_nep/9+1):(4*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(4*dim_nep/9+1):(5*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(5*dim_nep/9+1):(6*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(6*dim_nep/9+1):(7*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(7*dim_nep/9+1):(8*dim_nep/9)]),
    cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(8*dim_nep/9+1):(dim_nep)]))

df[df$country%in%c("Sri_Lanka"),]$cum_infec_med=
  c(cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[1:(dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(dim_sri/9+1):(2*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(2*dim_sri/9+1):(3*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(3*dim_sri/9+1):(4*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(4*dim_sri/9+1):(5*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(5*dim_sri/9+1):(6*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(6*dim_sri/9+1):(7*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(7*dim_sri/9+1):(8*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(8*dim_sri/9+1):(dim_sri)]))



df[df$country%in%c("Sri_Lanka"),]$cum_infec_li=
  c(cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[1:(dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(dim_sri/9+1):(2*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(2*dim_sri/9+1):(3*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(3*dim_sri/9+1):(4*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(4*dim_sri/9+1):(5*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(5*dim_sri/9+1):(6*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(6*dim_sri/9+1):(7*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(7*dim_sri/9+1):(8*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(8*dim_sri/9+1):(dim_sri)])
  )

df[df$country%in%c("Sri_Lanka"),]$cum_infec_ui=
  c(cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[1:(dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(dim_sri/9+1):(2*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(2*dim_sri/9+1):(3*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(3*dim_sri/9+1):(4*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(4*dim_sri/9+1):(5*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(5*dim_sri/9+1):(6*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(6*dim_sri/9+1):(7*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(7*dim_sri/9+1):(8*dim_sri/9)]),
    cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(8*dim_sri/9+1):(dim_sri)]))






date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right")

p1 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (cum_infec_med/data_india), color = colour),
    data = df[df$country%in%c("India"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = (cum_infec_li/data_india) ,
                           ymax=(cum_infec_ui/data_india)),
    data = df[df$country%in%c("India"),],
    size = 1,fill=ci,alpha=0.5,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p1 <- p1 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("India")



p2 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (cum_infec_med/data_bang), color = colour),
    data = df[df$country%in%c("Bangladesh"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = (cum_infec_li/data_bang),ymax=(cum_infec_ui/data_bang)),
    data = df[df$country%in%c("Bangladesh"),],
    size = 1,fill=ci,alpha=0.5,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p2 <- p2 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Bangladesh")

p3 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (cum_infec_med/data_pak), color = colour),
    data = df[df$country%in%c("Pakistan"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = (cum_infec_li/data_pak),ymax=(cum_infec_ui/data_pak)),
    data = df[df$country%in%c("Pakistan"),],
    size = 1,fill=ci,alpha=0.5,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p3 <- p3 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Pakistan")


p4 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (cum_infec_med/data_nep), color = colour),
    data = df[df$country%in%c("Nepal"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = (cum_infec_li/data_nep),ymax=(cum_infec_ui/data_nep)),
    data = df[df$country%in%c("Nepal"),],
    size = 1,fill=ci,alpha=0.5,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p4 <- p4 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Nepal")

p5 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (cum_infec_med/data_sri), color = colour),
    data = df[df$country%in%c("Sri_Lanka"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = (cum_infec_li/data_sri),ymax=(cum_infec_ui/data_sri)),
    data = df[df$country%in%c("Sri_Lanka"),],
    size = 1,fill=ci,alpha=0.5,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p5 <- p5 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Sri_Lanka")




margin = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p1=p1+margin
ggsave("scenario-URF_Ind.pdf",p1,width=11,height=8)
p2=p2+margin
ggsave("scenario-URF_Bang.pdf",p2,width=11,height=8)
p3=p3+margin
ggsave("scenario-URF_Pak.pdf",p3,width=11,height=8)
p4=p4+margin
ggsave("scenario-URF_Nep.pdf",p4,width=11,height=8)
p5=p5+margin
ggsave("scenario-URF_Sri.pdf",p5,width=11,height=8)
g_URF <- grid.arrange(p1,p2,p3,p4,p5,ncol=1)  # default settings
ggsave("scenario-URF_1.pdf",g_URF,width=11,height=11)
######################################################################################################################################################
# Deaths
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right")

p1 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour),
    data = df[df$country%in%c("India"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("India"),],
    width = 0.5,fill='royalblue4',alpha=0.7,
  )  + xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p1 <- p1 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("India")
ggsave("scenario-death_Ind.pdf",p1,width=9,height=6)

p2 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour),
    data = df[df$country%in%c("Bangladesh"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("Bangladesh"),],
    width = 0.5,fill='royalblue4',alpha=0.7,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-06-30'))
p2 <- p2 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Bangladesh")
ggsave("scenario-death_Bang.pdf",p2,width=9,height=6)

p3 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour),
    data = df[df$country%in%c("Pakistan"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("Pakistan"),],
    width = 0.5,fill='royalblue4',alpha=0.7,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p3 <- p3 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Pakistan")
ggsave("scenario-death_Pak.pdf",p3,width=9,height=6)


p4 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour),
    data = df[df$country%in%c("Nepal"),],
    size = 1,color=mn,
  )  +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("Nepal"),],
    width = 0.5,fill='royalblue4',alpha=0.7,
  )+ xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p4<- p4 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Nepal")
ggsave("scenario-death_Nep.pdf",p4,width=9,height=6)

p5 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour),
    data = df[df$country%in%c("Sri_Lanka"),],
    size = 1,color=mn,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("Sri_Lanka"),],
    width = 0.5,fill='royalblue4',alpha=0.7,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p5<- p5 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Sri_Lanka")
ggsave("scenario-death_Sri.pdf",p5,width=9,height=6)


margin = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p1=p1+margin
ggsave("scenario-deaths_Ind.pdf",p1,width=11,height=8)
p2=p2+margin
ggsave("scenario-deaths_Bang.pdf",p2,width=11,height=8)
p3=p3+margin
ggsave("scenario-deaths_Pak.pdf",p3,width=11,height=8)
p4=p4+margin
ggsave("scenario-deaths_Nep.pdf",p4,width=11,height=8)
p5=p5+margin
ggsave("scenario-deaths_Sri.pdf",p5,width=11,height=8)
g_deaths <- grid.arrange(p1,p2,p3,p4,p5,ncol=1)  # default settings
ggsave("scenario-deaths_1.pdf",g_deaths,width=11,height=11)

######################################################################################################################################################
# URF_2
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
UFR=25

x=seq(as.Date("2020-06-01"), as.Date("2021-12-31"), by = 1)

tot_cases_311220_ind=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"India"]
tot_cases_311220_bang=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Bangladesh"]
tot_cases_311220_pak=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Pakistan"]
tot_cases_311220_sri=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Sri_Lanka"]
tot_cases_311220_nep=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Nepal"]
ci <- c("#C79999")
mn <- c("#7C0000")
data_cases=data_cases[data_cases$Date>=changeDate,]
data_india=rep(cumsum(data_cases$India),9)+tot_cases_311220_ind
data_bang=rep(cumsum(data_cases$Bangladesh),9)+tot_cases_311220_bang
data_pak=rep(cumsum(data_cases$Pakistan),9)+tot_cases_311220_pak
data_nep=rep(cumsum(data_cases$Nepal),9)+tot_cases_311220_nep
data_sri=rep(cumsum(data_cases$Sri_Lanka),9)+tot_cases_311220_sri

tot_tot_cases_311220_ind=(data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"India"])*UFR
tot_tot_cases_311220_bang=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Bangladesh"]*UFR
tot_tot_cases_311220_pak=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Pakistan"]*UFR
tot_tot_cases_311220_sri=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Sri_Lanka"]*UFR
tot_tot_cases_311220_nep=data_total_cases[data_total_cases$Date==as.Date("2020-12-31"),"Nepal"]*UFR

dim_ind=nrow(df[df$country%in%c("India"),])
dim_bang=nrow(df[df$country%in%c("Bangladesh"),])
dim_pak=nrow(df[df$country%in%c("Pakistan"),])
dim_nep=nrow(df[df$country%in%c("Bangladesh"),])
dim_sri=nrow(df[df$country%in%c("Sri_Lanka"),])
df$UFR_med=rep(0,times=nrow(df))
df$UFR_li=rep(0,times=nrow(df))
df$UFR_ui=rep(0,times=nrow(df))



UUFR=UFR/((tot_tot_cases_311220_ind+cumsum(df[df$country%in%c("India"), ]$infections_median[1:(dim_ind/9)]))/data_india[1:(dim_ind/9)])
UUFR=rep(UUFR,9)





df[df$country%in%c("India"),]$UFR_med=(tot_tot_cases_311220_ind+
                                         c(cumsum(df[df$country%in%c("India"),]$infections_median[1:(dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(dim_ind/9+1):(2*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(2*dim_ind/9+1):(3*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(3*dim_ind/9+1):(4*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(4*dim_ind/9+1):(5*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(5*dim_ind/9+1):(6*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(6*dim_ind/9+1):(7*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(7*dim_ind/9+1):(8*dim_ind/9)]),
                                           cumsum(df[df$country%in%c("India"),]$infections_median[(8*dim_ind/9+1):(dim_ind)])))/data_india

df[df$country%in%c("Bangladesh"),]$UFR_med=(tot_tot_cases_311220_bang+
                                              c(cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[1:(dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(dim_bang/9+1):(2*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(2*dim_bang/9+1):(3*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(3*dim_bang/9+1):(4*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(4*dim_bang/9+1):(5*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(5*dim_bang/9+1):(6*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(6*dim_bang/9+1):(7*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(7*dim_bang/9+1):(8*dim_bang/9)]),
                                                cumsum(df[df$country%in%c("Bangladesh"),]$infections_median[(8*dim_bang/9+1):(dim_bang)])))/data_bang

df[df$country%in%c("Pakistan"),]$UFR_med=(tot_tot_cases_311220_pak+
                                            c(cumsum(df[df$country%in%c("Pakistan"),]$infections_median[1:(dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(dim_pak/9+1):(2*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(2*dim_pak/9+1):(3*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(3*dim_pak/9+1):(4*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(4*dim_pak/9+1):(5*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(5*dim_pak/9+1):(6*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(6*dim_pak/9+1):(7*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(7*dim_pak/9+1):(8*dim_pak/9)]),
                                              cumsum(df[df$country%in%c("Pakistan"),]$infections_median[(8*dim_pak/9+1):(dim_pak)])))/data_pak

df[df$country%in%c("India"),]$UFR_li=(tot_tot_cases_311220_pak+
                                        c(cumsum(df[df$country%in%c("India"),]$infections_li[1:(dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(dim_ind/9+1):(2*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(2*dim_ind/9+1):(3*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(3*dim_ind/9+1):(4*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(4*dim_ind/9+1):(5*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(5*dim_ind/9+1):(6*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(6*dim_ind/9+1):(7*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(7*dim_ind/9+1):(8*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_li[(8*dim_ind/9+1):(dim_ind)])))/data_india

df[df$country%in%c("Bangladesh"),]$UFR_li=(tot_tot_cases_311220_bang+
                                             c(cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[1:(dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(dim_bang/9+1):(2*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(2*dim_bang/9+1):(3*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(3*dim_bang/9+1):(4*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(4*dim_bang/9+1):(5*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(5*dim_bang/9+1):(6*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(6*dim_bang/9+1):(7*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(7*dim_bang/9+1):(8*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_li[(8*dim_bang/9+1):(dim_bang)])))/data_bang



df[df$country%in%c("Pakistan"),]$UFR_li=(tot_tot_cases_311220_pak+
                                           c(cumsum(df[df$country%in%c("Pakistan"),]$infections_li[1:(dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(dim_pak/9+1):(2*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(2*dim_pak/9+1):(3*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(3*dim_pak/9+1):(4*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(4*dim_pak/9+1):(5*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(5*dim_pak/9+1):(6*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(6*dim_pak/9+1):(7*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(7*dim_pak/9+1):(8*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_li[(8*dim_pak/9+1):(dim_pak)])))/data_pak

df[df$country%in%c("India"),]$UFR_ui=(tot_tot_cases_311220_ind+
                                        c(cumsum(df[df$country%in%c("India"),]$infections_ui[1:(dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(dim_ind/9+1):(2*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(2*dim_ind/9+1):(3*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(3*dim_ind/9+1):(4*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(4*dim_ind/9+1):(5*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(5*dim_ind/9+1):(6*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(6*dim_ind/9+1):(7*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(7*dim_ind/9+1):(8*dim_ind/9)]),
                                          cumsum(df[df$country%in%c("India"),]$infections_ui[(8*dim_ind/9+1):(dim_ind)])))/data_india

df[df$country%in%c("Bangladesh"),]$UFR_ui=(tot_tot_cases_311220_bang+
                                             c(cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[1:(dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(dim_bang/9+1):(2*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(2*dim_bang/9+1):(3*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(3*dim_bang/9+1):(4*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(4*dim_bang/9+1):(5*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(5*dim_bang/9+1):(6*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(6*dim_bang/9+1):(7*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(7*dim_bang/9+1):(8*dim_bang/9)]),
                                               cumsum(df[df$country%in%c("Bangladesh"),]$infections_ui[(8*dim_bang/9+1):(dim_bang)])))/data_bang



df[df$country%in%c("Pakistan"),]$UFR_ui=(tot_tot_cases_311220_pak+
                                           c(cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[1:(dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(dim_pak/9+1):(2*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(2*dim_pak/9+1):(3*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(3*dim_pak/9+1):(4*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(4*dim_pak/9+1):(5*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(5*dim_pak/9+1):(6*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(6*dim_pak/9+1):(7*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(7*dim_pak/9+1):(8*dim_pak/9)]),
                                             cumsum(df[df$country%in%c("Pakistan"),]$infections_ui[(8*dim_pak/9+1):(dim_pak)])))/data_pak


df[df$country%in%c("Nepal"),]$UFR_med=(tot_tot_cases_311220_nep+
                                         c(cumsum(df[df$country%in%c("Nepal"),]$infections_median[1:(dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(dim_nep/9+1):(2*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(2*dim_nep/9+1):(3*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(3*dim_nep/9+1):(4*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(4*dim_nep/9+1):(5*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(5*dim_nep/9+1):(6*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(6*dim_nep/9+1):(7*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(7*dim_nep/9+1):(8*dim_nep/9)]),
                                           cumsum(df[df$country%in%c("Nepal"),]$infections_median[(8*dim_nep/9+1):(dim_nep)])))/data_nep



df[df$country%in%c("Nepal"),]$UFR_li=(tot_tot_cases_311220_nep+
                                        c(cumsum(df[df$country%in%c("Nepal"),]$infections_li[1:(dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(dim_nep/9+1):(2*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(2*dim_nep/9+1):(3*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(3*dim_nep/9+1):(4*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(4*dim_nep/9+1):(5*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(5*dim_nep/9+1):(6*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(6*dim_nep/9+1):(7*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(7*dim_nep/9+1):(8*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_li[(8*dim_nep/9+1):(dim_nep)])))/data_nep


df[df$country%in%c("Nepal"),]$UFR_ui=(tot_tot_cases_311220_nep+
                                        c(cumsum(df[df$country%in%c("Nepal"),]$infections_ui[1:(dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(dim_nep/9+1):(2*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(2*dim_nep/9+1):(3*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(3*dim_nep/9+1):(4*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(4*dim_nep/9+1):(5*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(5*dim_nep/9+1):(6*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(6*dim_nep/9+1):(7*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(7*dim_nep/9+1):(8*dim_nep/9)]),
                                          cumsum(df[df$country%in%c("Nepal"),]$infections_ui[(8*dim_nep/9+1):(dim_nep)])))/data_nep

df[df$country%in%c("Sri_Lanka"),]$UFR_med=(tot_tot_cases_311220_sri+
                                             c(cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[1:(dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(dim_sri/9+1):(2*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(2*dim_sri/9+1):(3*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(3*dim_sri/9+1):(4*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(4*dim_sri/9+1):(5*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(5*dim_sri/9+1):(6*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(6*dim_sri/9+1):(7*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(7*dim_sri/9+1):(8*dim_sri/9)]),
                                               cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_median[(8*dim_sri/9+1):(dim_sri)])))/data_sri



df[df$country%in%c("Sri_Lanka"),]$UFR_li=(tot_tot_cases_311220_sri+
                                            c(cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[1:(dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(dim_sri/9+1):(2*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(2*dim_sri/9+1):(3*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(3*dim_sri/9+1):(4*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(4*dim_sri/9+1):(5*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(5*dim_sri/9+1):(6*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(6*dim_sri/9+1):(7*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(7*dim_sri/9+1):(8*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_li[(8*dim_sri/9+1):(dim_sri)])))/data_sri

df[df$country%in%c("Sri_Lanka"),]$UFR_ui=(tot_tot_cases_311220_sri+
                                            c(cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[1:(dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(dim_sri/9+1):(2*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(2*dim_sri/9+1):(3*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(3*dim_sri/9+1):(4*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(4*dim_sri/9+1):(5*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(5*dim_sri/9+1):(6*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(6*dim_sri/9+1):(7*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(7*dim_sri/9+1):(8*dim_sri/9)]),
                                              cumsum(df[df$country%in%c("Sri_Lanka"),]$infections_ui[(8*dim_sri/9+1):(dim_sri)])))/data_sri

df$UFR_med=df$UFR_med*UUFR
df$UFR_li=df$UFR_li*UUFR
df$UFR_ui=df$UFR_ui*UUFR
x1=seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = 1)
date=which(x1==as.Date("2021-12-31"))

df_b=df[(df$colour=="Original" & df$country=="India"),]$UFR_med
df5_b=df[(df$colour=="Original" & df$country=="Bangladesh"),]$UFR_med
df6_b=df[(df$colour=="Original" & df$country=="Pakistan"),]$UFR_med
df7_b=df[(df$colour=="Original" & df$country=="Sri_Lanka"),]$UFR_med
df8_b=df[(df$colour=="Original" & df$country=="Nepal"),]$UFR_med


mean(df_b[1:date])
mean(df5_b[1:date])
mean(df6_b[1:date])
mean(df7_b[1:date])
mean(df8_b[1:date])


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right")

p1 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (UFR_med), color = colour),
    data = df[df$country%in%c("India"),],
    size = 1,color=mn,
  )+ xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p1 <- p1 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("India")
ggsave("scenario-URF_Ind.pdf",p1,width=9,height=6)


p2 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (UFR_med), color = colour),
    data = df[df$country%in%c("Bangladesh"),],
    size = 1,color=mn,
  )+
  xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p2 <- p2 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Bangladesh")
ggsave("scenario-URF_Bang.pdf",p2,width=9,height=6)



p3 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (UFR_med), color = colour),
    data = df[df$country%in%c("Pakistan"),],
    size = 1,color=mn,
  )  + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p3 <- p3 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Pakistan")
ggsave("scenario-URF_Pak.pdf",p3,width=9,height=6)

p4 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (UFR_med), color = colour),
    data = df[df$country%in%c("Nepal"),],
    size = 1,color=mn,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p4 <- p4 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Nepal")
ggsave("scenario-URF_Nep.pdf",p4,width=9,height=6)



p5 <- base +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = (UFR_med), color = colour),
    data = df[df$country%in%c("Sri_Lanka"),],
    size = 1,color=mn,
  ) + xlim(as.Date('2021-01-01'), as.Date('2021-12-31'))
p5 <- p5 + ggplot2::labs(y = "URF") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Sri_Lanka")
ggsave("scenario-URF_Sri.pdf",p5,width=9,height=6)



margin = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p1=p1+margin
ggsave("scenario-URF_Ind.pdf",p1,width=11,height=8)
p2=p2+margin
ggsave("scenario-URF_Bang.pdf",p2,width=11,height=8)
p3=p3+margin
ggsave("scenario-URF_Pak.pdf",p3,width=11,height=8)
p4=p4+margin
ggsave("scenario-URF_Nep.pdf",p4,width=11,height=8)
p5=p5+margin
ggsave("scenario-URF_Sri.pdf",p5,width=11,height=8)
g_URF <- grid.arrange(p1,p2,p3,p4,p5,ncol=1)  # default settings
ggsave("scenario-URF_1.pdf",g_URF,width=11,height=11)


