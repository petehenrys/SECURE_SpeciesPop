### wrapper function

#####################
#####################

#set working directory
setwd("E://SECURE//Data//")

#read in raw daily moth count data available from EIDC
mth_dat <- read.csv("ECN_IM1.csv")
#read in sampling data, 
smp_dat <- read.csv("sample_data.csv")


#tb=table(mth_dat$SITECODE,mth_dat$FIELDNAME)
#mn=apply(tb,2,mean)
#tb[,which(mn>10)]


fn <- function(){

	source("SortData_YearlyCount_Multi.r")

	source("Match_and_Append_Covariates_multi.r")

	return(list(obs_yr_cnt=obs_yr_cnt,red_dat=red_dat,all_weather=all_weather,full_covs=full_covs,full_grid=full_grid))

}
	
sp=504
wdth=20
sim_def=200 # this is the number of days that species are declared to be similar based on their median Day of year. Only similar species are used to calculate list length

init_cntr=1

st_tab <- table(mth_dat$SITECODE[mth_dat$FIELDNAME==sp])

for (st in names(st_tab[st_tab>10])){

#for (st in c("T06","T08")){

	out_fn <- try(fn())
	red_dat <- out_fn$red_dat
	all_weather <- out_fn$all_weather
	full_covs <- out_fn$full_covs
	full_grid <- out_fn$full_grid
	obs_yr_cnt <- out_fn$obs_yr_cnt
	
	if(class(red_dat)!="try-error"){
	
		if(init_cntr==1){
			all_dat=red_dat
			cum_weather=all_weather
			f_covs=full_covs
			f_grid=full_grid
			obscnt=obs_yr_cnt
			
		}else{
			all_dat <- rbind(all_dat,red_dat)
			cum_weather <- rbind(cum_weather,all_weather)
			f_covs <- rbind(f_covs,full_covs)
			f_grid <- rbind(f_grid,full_grid)
			obscnt=rbind(obscnt,obs_yr_cnt)
		}
	
		init_cntr <- init_cntr+1
		print(st)
	}
}


#create dummy variables for contrasts
all_dat$YEAR <- as.factor(all_dat$YEAR)
dummy <- model.matrix( ~ YEAR - 1, data = all_dat)
#add into the full data frame
red_dat <- cbind(all_dat,dummy)

red_dat$Weather=log(red_dat$Weather+1)
f_covs$Weather=log(f_covs$Weather+1)
f_grid$Weather=log(f_grid$Weather+1)

#source("Estimate_Weather_Mod_multi.r")
source("Estimate_Weather_Mod_multi_Const.r")

obs <- tapply(obscnt$Observed_Annual_Total,obscnt$YEAR,mean,na.rm=TRUE)
yr.id <- match(1993:2012,names(obs))

dt <- cbind(Year=names(obs)[yr.id],Observed_Annual_Total=obs[yr.id],exp_cnt[,-1])

write.csv(dt,file=paste("AllSites.Dusk",sp,"ObsCount.OffsetIncWeather.csv",sep="_"),row.names=FALSE)

dt


write.csv(data.frame(quantile(red_dat$DOY,c(0.5,0.6,0.7,0.75,0.8,0.9,0.95,0.99,1),na.rm=TRUE)),file=paste("AllSites.Dusk",sp,"DOY_Quantiles.csv",sep="_"))


