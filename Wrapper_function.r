### wrapper function

#####################
#####################

#set working directory
setwd("E://SECURE//Data//")

#read in raw daily moth count data available from EIDC
mth_dat <- read.csv("ECN_IM1.csv")
#read in sampling data, 
smp_dat <- read.csv("sample_data.csv")

st="T06"
sp=285
wdth=20
sim_def=200 # this is the number of days that species are declared to be similar based on their median Day of year. Only similar species are used to calculate list length

source("SortData_YearlyCount.r")

source("Match_and_Append_Covariates.r")

source("resid_weather_plot.r")
#source("Estimate_Weather_Mod.r")
#source("Estimate_Weather_Mod_Const.r")

dt <- cbind(obs_yr_cnt[yr.id,],exp_cnt[,-1])
dt

write.csv(dt,file=paste(st,sp,"ObsCount.OffsetIncWeather.csv",sep="_"),row.names=FALSE)



