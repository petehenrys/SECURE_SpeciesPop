#########################
#########################

### Link to Covariates

#match up the dates between the species dataset to model and the full list length data
mti=c()
for(k in 1:length(red_dat[,1])){
mti[k] <- which(list_length$DATE==red_dat$POSIXdate[k])
}
#add in list length for corresponding dates into species data
red_dat$LL <- list_length$LL[mti]
red_dat$Active<-active_doys$Active[match(red_dat$DOY,active_doys$DOY)]


#load in the observed weather data evaluated based on differing night lengths etc from Mark's script
load(paste("Weather//ECN_rainfall_",st,".RData",sep=""))
#match up the dates between the species dataset to model and the weather data
mti=c()
for(k in 1:length(red_dat[,1])){
mti[k] <- which(as.Date(out.ecn.data$Date)==as.Date(red_dat$POSIXdate[k]))
}
#add in observed weather data for corresponding dates into species data
red_dat$ObsWeather <- out.ecn.data$duskrainfallintensity[mti]

#load in the modelled weather data for missing dates evaluated based on average for similar time periods over the series
load(paste("Weather//ECN_estimatedmissingrainfall_",st,".RData",sep=""))
#match up the dates between the species dataset to model and the weather data
mti=c()
for(k in 1:length(red_dat[,1])){
mti[k] <- which(as.Date(out.ecn.data.missing$Date)==as.Date(red_dat$POSIXdate[k]))
}

#add in modelled weather data for corresponding dates into species data
red_dat$EstWeather <- out.ecn.data.missing$duskrainfallintensity[mti]

#create an overall weather column that takes the observed weather where possible, but if not uses the modelled weather
red_dat$Weather=apply(matrix(c(red_dat$ObsWeather,red_dat$EstWeather),nrow=length(red_dat[,1]),ncol=2),1,sum,na.rm=TRUE)
#red_dat$Weather=sqrt(red_dat$Weather)

#combine the observed and modelled weather data over the whole period for a ful weather data set.
all_weather = data.frame(Date=out.ecn.data$Date,ObsRain=out.ecn.data$nightrainfallintensity,EstRain=out.ecn.data.missing$nightrainfallintensity)
all_weather$Weather=apply(matrix(c(all_weather$ObsRain,all_weather$EstRain),nrow=length(all_weather[,1]),ncol=2),1,sum,na.rm=TRUE)
all_weather$SiteCode=st

#####################
#####################


#create a dataframe with all covariates in 
full_covs <- list_length
dts <- strptime(full_covs$DATE,"%Y-%m-%d")
full_covs$YEAR <- 1900 + dts$year
full_covs$MONTH <- dts$mon + 1
full_covs$DAY <- dts$mday
full_covs$DOY <- dts$yday + 1 
full_covs$SG_id <- as.integer(cut(full_covs$YEAR,4))
full_covs$Active<-active_doys$Active[match(full_covs$DOY,active_doys$DOY)]
full_covs$LLActive<-full_covs$Active*full_covs$LL
full_covs$Weather<-all_weather$Weather[match(as.Date(full_covs$DATE),as.Date(all_weather$Date))]
full_covs$Dry=0
full_covs$SiteCode=st
##if any mismatch between covariate data and sample data, remove the row so that this is only based on the sample times
if(any(!is.element(full_covs$YEAR,red_dat$YEAR))){
  full_covs <- full_covs[-which(!is.element(full_covs$YEAR,red_dat$YEAR)),]
}

#######################
#######################

### create a full data frame of covariate covering the whole period, not just sampling occurrences
full_grid=data.frame(DATE=seq(as.Date(min(full_covs$DATE)),as.Date("31/12/2012","%d/%m/%Y"),by=1))
tmp=strptime(full_grid$DATE,"%Y-%m-%d")
full_grid$DOY=tmp$yday+1
full_grid$YEAR=tmp$year+1900
full_grid$SG_id <- as.integer(cut(full_grid$YEAR,4))

estLL=tapply(full_covs$LL,full_covs$DOY,mean)
full_grid$LL=NA
full_grid$LL[match(as.Date(full_covs$DATE),as.Date(full_grid$DATE))]=full_covs$LL
full_grid$estLL = estLL[match(full_grid$DOY,names(estLL))]
full_grid$LL[is.na(full_grid$LL)]=full_grid$estLL[is.na(full_grid$LL)]
full_grid$Active=active_doys$Active[match(full_grid$DOY,active_doys$DOY)]

full_grid$LLActive=full_grid$LL*full_grid$Active

full_grid$Weather=all_weather$Weather[match(as.Date(full_grid$DATE),as.Date(all_weather$Date))]
full_grid$Dry=0
full_grid$SiteCode=st


#######################
#######################


