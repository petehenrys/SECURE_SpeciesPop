# weather v3.R (SECURE)
#
# Program to read in CEH ECN weather data (one file per site) and create summaries for analysis in the SECURE project:
# 1) Total night rainfall
# 2) Intensity of night rainfall
# 3) Amount and intensity of rainfall during "civil twilight" hours (dusk and dawn) and some others
# This version is the complement to v2, as we output only the "missing" values by filling in corresponding means, and make the actual data values blank

# Get file names, site codes and names
data.dir <- "Data/ECN/"
datafile.names <- dir(data.dir)[-1]
datafile.sitecodes <- substr(datafile.names,1,3)
datafile.sitenames <- substr(datafile.names,5,30)
datafile.sitenames <- matrix(unlist(strsplit(datafile.sitenames,"[.]")),nrow=2)[1,]
n.files <- length(datafile.names)
# Need lat/long for the time functions
locations <- read.csv(paste(data.dir,"ECNlocs.csv",sep=""))
location.x <- locations[locations$ECN.SITE.CODE%in%datafile.sitecodes,"LONGITUDE..WGS84."]
location.y <- locations[locations$ECN.SITE.CODE%in%datafile.sitecodes,"LATITUDE..WGS84."]
longlat.mat <- cbind(x=location.x,y=location.y)

# Need maptools for the "sunriset" and "crepuscule" functions for times of interest
library(maptools)

# Process each data file one at time
for(i.file in 1:n.files){
    # Load and fix data
    ecn.data <- read.csv(paste(data.dir,datafile.names[i.file],sep=""))
    longlat <- matrix(longlat.mat[i.file,],nrow=1) # NB x,y, i.e. long then lat
    ecn.data <- ecn.data[ecn.data$FIELDNAME=="RAIN",]
    # Want rows in chron. order
    ecn.data <- ecn.data[order(ecn.data$SHOUR),]
    ecn.data <- ecn.data[order(ecn.data$SDATE),]
    ecn.data$SDATE <- as.character(format(ecn.data$SDATE,scientific=FALSE))
    ecn.data$Year <- substr(ecn.data$SDATE,1,4)
    ecn.data$Month <- substr(ecn.data$SDATE,5,6)
    ecn.data$Day <- substr(ecn.data$SDATE,7,8)
    # NB be careful with timezone - specifying GMT everywhere avoids problems with DST
    ecn.data$Date <- strptime(substr(ecn.data$SDATE,1,8),"%Y%m%d",tz="GMT")
    ecn.data$DateAndHour <- strptime(paste(substr(ecn.data$SDATE,1,8),ecn.data$SHOUR),"%Y%m%d %H",tz="GMT")
    ecn.data$DayOfYear <- as.numeric(strftime(ecn.data$Date, format = "%j",tz="GMT"))
    # Remove any rows prior to 1/1/93
    ecn.data <- ecn.data[!(ecn.data$Date <= strptime("31dec1992","%d%b%Y",tz="GMT")),]

    # Some sites have AWS [auto weather system] overlap - plot shows which
    # pdf(paste("AWS overlap",datafile.sitecodes[i.file],"_",datafile.sitenames[i.file],".pdf",sep=""))
    # with(ecn.data,plot(DateAndHour,AWSNO,type="l"))
    # dev.off()
    # Advice from Pete Henrys is to use only later AWS given data for two
    repeat.dates <- ecn.data$DateAndHour%in%ecn.data$DateAndHour[duplicated(ecn.data$DateAndHour)]
    repeat.dates.and.aws.1 <- repeat.dates & ecn.data$AWSNO==1
    ecn.data <- ecn.data[!repeat.dates.and.aws.1,]
    print(dim(ecn.data)) # sanity check and progress report

    # Create "complete" time frame from start to finish (so gaps in the time series will be present but NA)
    test.date <- rep(seq.Date(as.Date(strptime("31dec1992","%d%b%Y",tz="GMT")),as.Date(strptime("1jan2013","%d%b%Y",tz="GMT")),by=1),each=24)
    test.date.and.hour <- strptime(paste(test.date,1:24),"%Y-%m-%d %H",tz="GMT")
    ecn.data2 <- data.frame(Date=as.POSIXct(as.POSIXlt(test.date),tz="GMT"),DateAndHour=test.date.and.hour)
    len.ecn.data <- nrow(ecn.data)
    len.ecn.data2 <- nrow(ecn.data2)
    ecn.data.date.and.hour <- ecn.data$DateAndHour
    ecn.data.value <- ecn.data$VALUE
    ecn.data2.value <- rep(NA,len.ecn.data2)
    # Quick check to see if dates in the expanded data set occur exactly once each in the original data set
    # Previously this failed due to repeated dates owing to AWS overlap (see code above)
    if(length(ecn.data2.value[which(test.date.and.hour%in%ecn.data.date.and.hour)])!=length(ecn.data.value)){
        print(length(ecn.data2.value[which(test.date.and.hour%in%ecn.data.date.and.hour)]))
        print(length(ecn.data.value))
        stop("Problem with differing vector lengths.\n")
    }
    # Assign original data values [rainfall] to expanded data values column but only for the dates in the new data frame which are also in the old data frame
    # This is why the data set needs to be in chronological order, so that these will definitely match
    ecn.data2.value[which(test.date.and.hour%in%ecn.data.date.and.hour)] <- ecn.data.value
    ecn.data2$VALUE <- ecn.data2.value

    # Data set to be output
    out.ecn.data <- data.frame(Date=unique(ecn.data2$Date))
    out.ecn.data$DayOfYear <- as.numeric(strftime(out.ecn.data$Date, format = "%j",tz="GMT"))
    one.day <- 24*3600 # seconds
    one.hour <- 3600 # seconds
    # Important time points
    out.ecn.data$sunrise <- sunriset(longlat,out.ecn.data$Date,POSIXct.out=TRUE,direction="sunrise")$time
    out.ecn.data$sunset <- sunriset(longlat,out.ecn.data$Date-one.day,POSIXct.out=TRUE,direction="sunset")$time # THE DAY BEFORE
    out.ecn.data$dawn <- crepuscule(longlat,out.ecn.data$Date,POSIXct.out=TRUE,direction="dawn",solarDep=6)$time # civil twilight -> solarDep = 6
    out.ecn.data$dusk <- crepuscule(longlat,out.ecn.data$Date-one.day,POSIXct.out=TRUE,direction="dusk",solarDep=6)$time # THE DAY BEFORE
    out.ecn.data$solarnoon <- solarnoon(longlat,out.ecn.data$Date-one.day,POSIXct.out=TRUE)$time # THE DAY BEFORE
    out.ecn.data$sunriseyesterday <- sunriset(longlat,out.ecn.data$Date-1,POSIXct.out=TRUE,direction="sunrise")$time
    out.ecn.data$houraftersunset <- out.ecn.data$sunset+one.hour
    out.ecn.data$hourbeforesunrise <- out.ecn.data$sunrise-one.hour
    # Corresponding time range lengths - need to use difftime to ensure calculations always reported in hours
    out.ecn.data$nightlength <- difftime(out.ecn.data$sunrise,out.ecn.data$sunset,tz="GMT",units="hours")
    out.ecn.data$dusktodawn <- difftime(out.ecn.data$dawn,out.ecn.data$dusk,tz="GMT",units="hours")
    out.ecn.data$sunsettodusk <- difftime(out.ecn.data$dusk,out.ecn.data$sunset,tz="GMT",units="hours")
    out.ecn.data$dawntosunrise <- difftime(out.ecn.data$sunrise,out.ecn.data$dawn,tz="GMT",units="hours")
    out.ecn.data$morninglength <- difftime(out.ecn.data$solarnoon,out.ecn.data$sunriseyesterday,tz="GMT",units="hours")
    out.ecn.data$afternoonlength <- difftime(out.ecn.data$sunset,out.ecn.data$solarnoon,tz="GMT",units="hours")
    # Rainfall calculation for the different time ranges
    len.out.ecn.data <- nrow(out.ecn.data)
    hourbeforesunriserainfall <- houraftersunsetrainfall <- afternoonrainfall <- morningrainfall <- dawnrainfall <- duskrainfall <- nightrainfall <- numeric(len.out.ecn.data)
    # Full night (sunset to sunrise)
    for(i in 1:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$sunrise[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$sunset[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        # need to downweight the first and last hour to match sunset and sunrise times
        value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60)
        value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$sunrise[i],"%M"))+as.numeric(format(out.ecn.data$sunrise[i],"%S"))/60)/60)
        nightrainfall[i] <- sum(value.subset)
    }
    # Dawn (civil twilight to sunrise)
    for(i in 1:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$sunrise[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$dawn[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        if(len.subset==1){
            value.subset[1] <- value.subset[1]*((1-(as.numeric(format(out.ecn.data$dawn[i],"%M"))+as.numeric(format(out.ecn.data$dawn[i],"%S"))/60)/60)+((as.numeric(format(out.ecn.data$sunrise[i],"%M"))+as.numeric(format(out.ecn.data$sunrise[i],"%S"))/60)/60))
        }else{
            value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$dawn[i],"%M"))+as.numeric(format(out.ecn.data$dawn[i],"%S"))/60)/60)
            value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$sunrise[i],"%M"))+as.numeric(format(out.ecn.data$sunrise[i],"%S"))/60)/60)
        }
        dawnrainfall[i] <- sum(value.subset)
    }
    # Dusk (sunset to end of civil twilight)
    duskrainfall[1] <- NA # This is entirely before the data set starts
    for(i in 2:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$dusk[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$sunset[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        if(len.subset==1){
            value.subset[1] <- value.subset[1]*((1-(as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60)+((as.numeric(format(out.ecn.data$dusk[i],"%M"))+as.numeric(format(out.ecn.data$dusk[i],"%S"))/60)/60))
        }else{
            value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60)
            value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$dusk[i],"%M"))+as.numeric(format(out.ecn.data$dusk[i],"%S"))/60)/60)
        }
        duskrainfall[i] <- sum(value.subset)
    }
    # Morning (day before - sunrise to solar noon)
    morningrainfall[1] <- NA # This is entirely before the data set starts
    for(i in 2:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$solarnoon[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$sunriseyesterday[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        if(len.subset==1){
            value.subset[1] <- value.subset[1]*((1-(as.numeric(format(out.ecn.data$sunriseyesterday[i],"%M"))+as.numeric(format(out.ecn.data$sunriseyesterday[i],"%S"))/60)/60)+((as.numeric(format(out.ecn.data$solarnoon[i],"%M"))+as.numeric(format(out.ecn.data$solarnoon[i],"%S"))/60)/60))
        }else{
            value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$sunriseyesterday[i],"%M"))+as.numeric(format(out.ecn.data$sunriseyesterday[i],"%S"))/60)/60)
            value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$solarnoon[i],"%M"))+as.numeric(format(out.ecn.data$solarnoon[i],"%S"))/60)/60)
        }
        morningrainfall[i] <- sum(value.subset)
    }
    # Afternoon (day before - solar noon to sunset)
    afternoonrainfall[1] <- NA # This is entirely before the data set starts
    for(i in 2:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$sunset[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$solarnoon[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        if(len.subset==1){
            value.subset[1] <- value.subset[1]*((1-(as.numeric(format(out.ecn.data$solarnoon[i],"%M"))+as.numeric(format(out.ecn.data$solarnoon[i],"%S"))/60)/60)+((as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60))
        }else{
            value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$solarnoon[i],"%M"))+as.numeric(format(out.ecn.data$solarnoon[i],"%S"))/60)/60)
            value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60)
        }
        afternoonrainfall[i] <- sum(value.subset)
    }
    # First hour after sunset (day before)
    houraftersunsetrainfall[1] <- NA # This is entirely before the data set starts
    for(i in 2:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$houraftersunset[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$sunset[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        if(len.subset==1){
            value.subset[1] <- value.subset[1]*((1-(as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60)+((as.numeric(format(out.ecn.data$houraftersunset[i],"%M"))+as.numeric(format(out.ecn.data$houraftersunset[i],"%S"))/60)/60))
        }else{
            value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$sunset[i],"%M"))+as.numeric(format(out.ecn.data$sunset[i],"%S"))/60)/60)
            value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$houraftersunset[i],"%M"))+as.numeric(format(out.ecn.data$houraftersunset[i],"%S"))/60)/60)
        }
        houraftersunsetrainfall[i] <- sum(value.subset)
    }
    #  Hour before sunrise
    for(i in 1:len.out.ecn.data){
        data.subset <- ecn.data2[ecn.data2$DateAndHour <= out.ecn.data$sunrise[i]+one.hour & ecn.data2$DateAndHour >= out.ecn.data$hourbeforesunrise[i]-one.hour,]
        value.subset <- data.subset$VALUE
        len.subset <- length(value.subset)
        if(len.subset==1){
            value.subset[1] <- value.subset[1]*((1-(as.numeric(format(out.ecn.data$hourbeforesunrise[i],"%M"))+as.numeric(format(out.ecn.data$hourbeforesunrise[i],"%S"))/60)/60)+((as.numeric(format(out.ecn.data$sunrise[i],"%M"))+as.numeric(format(out.ecn.data$sunrise[i],"%S"))/60)/60))
        }else{
            value.subset[1] <- value.subset[1]*(1-(as.numeric(format(out.ecn.data$hourbeforesunrise[i],"%M"))+as.numeric(format(out.ecn.data$hourbeforesunrise[i],"%S"))/60)/60)
            value.subset[len.subset] <- value.subset[len.subset]*((as.numeric(format(out.ecn.data$sunrise[i],"%M"))+as.numeric(format(out.ecn.data$sunrise[i],"%S"))/60)/60)
        }
        hourbeforesunriserainfall[i] <- sum(value.subset)
    }
    # Enter into output data set, with intensities
    out.ecn.data$nightrainfall <- nightrainfall
    out.ecn.data$nightrainfallintensity <- nightrainfall/as.numeric(out.ecn.data$nightlength)
    out.ecn.data$dawnrainfall <- dawnrainfall
    out.ecn.data$dawnrainfallintensity <- dawnrainfall/as.numeric(out.ecn.data$dawntosunrise)
    out.ecn.data$duskrainfall <- duskrainfall
    out.ecn.data$duskrainfallintensity <- duskrainfall/as.numeric(out.ecn.data$sunsettodusk)
    out.ecn.data$morningrainfall <- morningrainfall
    out.ecn.data$morningrainfallintensity <- morningrainfall/as.numeric(out.ecn.data$morninglength)
    out.ecn.data$afternoonrainfall <- afternoonrainfall
    out.ecn.data$afternoonrainfallintensity <- afternoonrainfall/as.numeric(out.ecn.data$afternoonlength)
    out.ecn.data$houraftersunsetrainfall <- houraftersunsetrainfall
    out.ecn.data$hourbeforesunriserainfall <- hourbeforesunriserainfall

    # Now create a second data frame, where we:
    # (a) replace NAs with estimates formed by means of the data from the same dates from years with data available; and
    # (b) replace data points with NAs
    variable.list <- names(out.ecn.data)[17:28]
    out.ecn.data.missing <- out.ecn.data
    for(i.list in 1:length(variable.list)){
        tempcol <- tapply(out.ecn.data[,variable.list[i.list]],out.ecn.data$DayOfYear,mean,na.rm=TRUE)
        out.ecn.data.missing[is.na(out.ecn.data[,variable.list[i.list]]),variable.list[i.list]] <- tempcol[out.ecn.data$DayOfYear[is.na(out.ecn.data[,variable.list[i.list]])]]
        is.na(out.ecn.data.missing[!is.na(out.ecn.data[,variable.list[i.list]]),variable.list[i.list]]) <- TRUE
    }

    # Write out computed rainfall data summaries
    write.csv(out.ecn.data,paste("ECN_rainfall_",datafile.sitecodes[i.file],"_",datafile.sitenames[i.file],".csv",sep=""),row.names = FALSE)
    save(out.ecn.data,file=paste("ECN_rainfall_",datafile.sitecodes[i.file],"_",datafile.sitenames[i.file],".RData",sep=""))

    write.csv(out.ecn.data.missing,paste("ECN_estimatedmissingrainfall_",datafile.sitecodes[i.file],"_",datafile.sitenames[i.file],".csv",sep=""),row.names = FALSE)
    save(out.ecn.data.missing,file=paste("ECN_estimatedmissingrainfall_",datafile.sitecodes[i.file],"_",datafile.sitenames[i.file],".RData",sep=""))

} # end i.file loop
