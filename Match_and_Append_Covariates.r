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
red_dat$ObsWeather <- out.ecn.data$nightrainfallintensity[mti]

#load in the modelled weather data for missing dates evaluated based on average for similar time periods over the series
load(paste("Weather//ECN_estimatedmissingrainfall_",st,".RData",sep=""))
#match up the dates between the species dataset to model and the weather data
mti=c()
for(k in 1:length(red_dat[,1])){
mti[k] <- which(as.Date(out.ecn.data.missing$Date)==as.Date(red_dat$POSIXdate[k]))
}

#add in modelled weather data for corresponding dates into species data
red_dat$EstWeather <- out.ecn.data.missing$nightrainfallintensity[mti]

#create an overall weather column that takes the observed weather where possible, but if not uses the modelled weather
red_dat$Weather=apply(matrix(c(red_dat$ObsWeather,red_dat$EstWeather),nrow=length(red_dat[,1]),ncol=2),1,sum,na.rm=TRUE)
#red_dat$Weather=sqrt(red_dat$Weather)

#combine the observed and modelled weather data over the whole period for a ful weather data set.
all_weather = data.frame(Date=out.ecn.data$Date,ObsRain=out.ecn.data$nightrainfallintensity,EstRain=out.ecn.data.missing$nightrainfallintensity)
all_weather$Weather=apply(matrix(c(all_weather$ObsRain,all_weather$EstRain),nrow=length(all_weather[,1]),ncol=2),1,sum,na.rm=TRUE)

#####################
#####################

#create dummy variables for contrasts
red_dat$YEAR <- as.factor(red_dat$YEAR)
dummy <- model.matrix( ~ YEAR - 1, data = red_dat)
#add into the full data frame
red_dat <- cbind(red_dat,dummy)



