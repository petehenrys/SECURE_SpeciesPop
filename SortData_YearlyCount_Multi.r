

#####################
#####################

#set working directory
#setwd("F://SECURE//Data//")

#read in raw daily moth count data available from EIDC
#mth_dat <- read.csv("ECN_IM1.csv")
#read in sampling data, 
#smp_dat <- read.csv("sample_data.csv")
#
#st="T08"
#sp=180
#wdth=20
#sim_def=200 # this is the number of days that species are declared to be similar based on their median Day of year. Only similar species are used to calculate list length

#####################
#####################


#dataset for site and species of interest
stsp_dat <- mth_dat[mth_dat$SITECODE==st & is.element(mth_dat$FIELDNAME,c("XX",as.character(sp))),]

##full moth dataset for site of interest
st_dat <- mth_dat[mth_dat$SITECODE==st,]
##full sampling record of site of interest
st_smp <- smp_dat[smp_dat$SITECODE==st,]

##assign date and day of year to the moth data from site in question
st_dat$SDATE=as.character(st_dat$SDATE)
st_dat$DOY=strptime(st_dat$SDATE,"%d-%b-%y")$yday + 1

## work out the median day of year for each species
med.doys=tapply(st_dat$DOY,st_dat$FIELDNAME,median,na.rm=TRUE)
obs_med.doy=med.doys[which(names(med.doys)==sp)]
#define similar species as those that have DOY within sim_def of observed DOY
sim_sp = names(med.doys)[which(abs(med.doys-obs_med.doy)<sim_def & !is.element(names(med.doys),c("XX","Q1")))]
#create site data that only includes similar species to the one in question
st_dat_sim = st_dat[is.element(st_dat$FIELDNAME,sim_sp),]

#calculate how many of the similar species were recorded on each day - this will form list length covariate
x <- tapply(st_dat$FIELDNAME,st_dat$SDATE,function(x){length(unique(x[is.element(x,sim_sp)]))})
#create data frame from tapply object
list_length <- data.frame(Orig_Date=names(x),LL=x,DATE=strptime(names(x),"%d-%b-%y"))
rownames(list_length) <- seq(length=nrow(list_length))
##order data frame by date
list_length <- list_length[order(list_length$DATE),]


### create full species record, including 0s defined where sample WAS taken

#find corresponding dates between sample dates and observed dates for species in question
mt_id <- match(st_smp$SDATE,stsp_dat$SDATE)
#which sample dates had no match
na.idx <- which(is.na(mt_id))
#assign those with no match as missing dates
miss_dates <- as.character(st_smp$SDATE[na.idx])

### create data frame of true 0s where sample was taken but no record for species. from miss dates. 

#first create data frame representing dates where count was 0. ie sample taken but no species record
true_zeros <- data.frame(SITECODE=st, LCODE=1, SDATE=miss_dates, FIELDNAME="XX", VALUE=0)
#combime the observed species data with the true zero observation data
full_dat <- rbind(stsp_dat,true_zeros)
#decompose the date into constituent parts and add into data frame
dts <- strptime(full_dat$SDATE,"%d-%b-%y")
full_dat$POSIXdate=dts
full_dat$YEAR <- 1900 + dts$year
full_dat$MONTH <- dts$mon + 1
full_dat$DAY <- dts$mday
full_dat$DOY <- dts$yday + 1 
#crerate a numeric running value over the full time series
full_dat$Run_DOY <- full_dat$DOY + (365*(full_dat$YEAR-1993))
#order the dataset by the running value 
full_dat <- full_dat[order(full_dat$Run_DOY),]

#plot the log species count for each day within a year giving each year a different colour. 
cntr=1
plot(full_dat$DOY,log(full_dat$VALUE+1),type="n")
for(k in unique(full_dat$YEAR)){
  lines(full_dat$DOY[full_dat$YEAR==k],log(full_dat$VALUE[full_dat$YEAR==k]+1),lwd=2,col=cntr)
  cntr=cntr+1
}

##find those sample records that represent cumulative observations over multiple days from the sample data
mult_dates <- which(st_smp$SPERIOD_D>1)
rm_mult <- which(is.element(as.character(full_dat$SDATE),as.character(st_smp$SDATE[mult_dates])))

##remove these dates from the full data frame - difficult to use at this stage
full_dat <- full_dat[-rm_mult,]

##based on the sample dates left work out the total count for each day of year across all years
dt <- data.frame(tapply(full_dat$VALUE,full_dat$DOY,sum))

### restrict the overall time window to days of year where an observation (across all years) was no more than wdth days away

#create empty vector representing days throughout the year and set a counter to 1
tot=rep(NA,366); ctr=1
#loop over days in year
for(i in as.integer(rownames(dt))){
  #find all days within wdth of the current day (i) in loop. careful of circularity in the days (eg 1 and 365) 
  sq <- (i-wdth):(i+wdth)
  sq[sq<1]=sq[sq<1]+365
  sq[sq>365]=sq[sq>365]-365
  #sum the total species count over all days within wdth of current day
  tot[ctr] <- sum(full_dat$VALUE[is.element(full_dat$DOY,sq)])
  #move counter on
  ctr <- ctr + 1

}
#record all days where the total species count across all 2*wdth window was zero
zero_doys <- as.integer(rownames(dt))[tot==0]

#convert to a data frame of days labelled 0 and 1  according to an active period for species or not.
active_doys <- data.frame(DOY=1:366,Active=1)
active_doys$Active[which(is.element(active_doys$DOY,zero_doys))]=0

#remove all non-active days from the full species data set and save seperately
red_dat <- full_dat[-which(is.element(full_dat$DOY,zero_doys)),]

### plot the day of year against log species cound for the reduced data set representing only "active days"
#x11()
cntr=1
plot(red_dat$DOY,log(red_dat$VALUE+1),type="n")
for(k in unique(red_dat$YEAR)){
  points(red_dat$DOY[red_dat$YEAR==k],log(red_dat$VALUE[red_dat$YEAR==k]+1),lwd=2,col=cntr)
  cntr=cntr+1
}

### for those species that are observed across the december/january year split, the december data should be assigned to the same year as the january data recognising it is the same "event"
### this is for the random term in the model

#create new year vector that is repeat of observed year
red_dat$RND_Yr <- red_dat$YEAR
#calculate those incidents where the difference in running day is more than 100 (ie there has been a long gap over the year)
withinYR <- red_dat$YEAR[which(diff(red_dat$Run_DOY)>100)-1]-red_dat$YEAR[which(diff(red_dat$Run_DOY)>100)+1]
#if the sum of this is zero then the gaps are within rather than across the year and the second period should be aligned with the next first period
if(sum(withinYR)==0){
red_dat$RND_Yr[(red_dat$DOY>mean(red_dat$DOY[which(diff(red_dat$Run_DOY)>100)]))]=red_dat$RND_Yr[red_dat$DOY>mean(red_dat$DOY[which(diff(red_dat$Run_DOY)>100)])]+1
}

#investigate the species count by year to check all looks ok
table(red_dat$VALUE,red_dat$YEAR)

#finally, produce data frame of the total species count in each year
obs_yr_cnt=data.frame(YEAR=rownames(tapply(red_dat$VALUE,red_dat$YEAR,sum)),Observed_Annual_Total=tapply(red_dat$VALUE,red_dat$YEAR,sum))

#write.csv(obs_yr_cnt,file=paste(st,sp,"Observed_Annual_Total.csv",sep="_"),row.names=FALSE)


#########################
#########################

