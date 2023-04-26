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
data_world_confirmed = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
data_world_recovered = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
data_world_deaths = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
name_countries = unique(data_world_confirmed$Country.Region)
flag1=which(name_countries=="Bangladesh")
flag2=which(name_countries=="India")
flag3=which(name_countries=="Pakistan")
flag4=which(name_countries=="Nepal")
flag5=which(name_countries=="Sri Lanka")

flag_vec=c(flag1,flag2,flag3,flag4,flag5)
country_name = unique(data_world_deaths$Country.Region)[flag1]
data_country_deaths = colSums((data_world_deaths %>% filter(Country.Region == country_name))[, 5:ncol(data_world_deaths)])
dates = seq(as.Date("2020-01-22"), as.Date("2020-01-22") + ncol(data_world_deaths)-4, by = 1) ## yyyy-mm-dd
date_initial = as.Date("2020-11-15")
date_final = as.Date("2021-06-30")
I = abs(diff(data_country_deaths[(which(dates == date_initial)-1):(which(dates == date_final))]))
dates=as.Date(dates[(which(dates == date_initial)):(which(dates == date_final))])
data=data.frame(dates)
data_cases=data.frame(dates)
data_total_cases=data.frame(dates)
for(flag in flag_vec){
  country_name = unique(data_world_deaths$Country.Region)[flag]
  data_country_deaths = colSums((data_world_deaths %>% filter(Country.Region == country_name))[, 5:ncol(data_world_deaths)])
  data_country_cases = colSums((data_world_confirmed %>% filter(Country.Region == country_name))[, 5:ncol(data_world_confirmed)])
  dates = seq(as.Date("2020-01-22"), as.Date("2020-01-22") + ncol(data_world_deaths)-4, by = 1) ## yyyy-mm-dd
  date_initial = as.Date("2020-11-15")
  date_final = as.Date("2021-06-30")
  TC=data_country_cases[(which(dates == date_initial)):(which(dates == date_final))]
  I = abs(diff(data_country_deaths[(which(dates == date_initial)-1):(which(dates == date_final))]))
  C = abs(diff(data_country_cases[(which(dates == date_initial)-1):(which(dates == date_final))]))
  dates=as.Date(dates[(which(dates == date_initial)):(which(dates == date_final))])
  data=cbind.data.frame(data,I)
  data_cases=cbind.data.frame(data_cases,C)
  data_total_cases=cbind.data.frame(data_total_cases,TC)
}
colnames(data)=c("Date","Bangladesh","India","Pakistan","Nepal","Sri_Lanka")
rownames(data)=c(1:nrow(data))
colnames(data_cases)=c("Date","Bangladesh","India","Pakistan","Nepal","Sri_Lanka")
rownames(data_cases)=c(1:nrow(data_cases))
colnames(data_total_cases)=c("Date","Bangladesh","India","Pakistan","Nepal","Sri_Lanka")
rownames(data_total_cases)=c(1:nrow(data_total_cases))
dat<-data
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
  prior_intercept = rstanarm::normal(location=0.08,scale = 0.05),
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


fit <- do.call(epim, args)
saveRDS(fit,"fit_2021_1.rds")

#fit<-readRDS("fit_2021_2_urfd.rds")
#fit$pop_adjust=TRUE
#plot_obs(fit,type="deaths")
#plot_rt(fit)
