#
#FILE TO READ IN AND SUMMARISE MULTIPLE ANALYSES FOR SECURE PROJECT
#FILE NAMES BEGINNING 06 ARE ROTHAMSTED DATA
#FILENAMES BEGINNING 08 ARE WYTHM
#offset status 0 means no offest, 1 means with offset
#interest is initially in whether having the offset is beneficial (ie was it worth all the effort trying to allow for rainfall on the recording days
#

nspsites <- 2  #17 in full set #number of combinations of species and site, and for the C and null (0) models
log_Lik_C_offset_0 <- rep(0,nspsites )
log_Lik_C_offset_1 <- rep(0,nspsites )
log_Lik_0_offset_0 <- rep(0,nspsites )
log_Lik_0_offset_1 <- rep(0,nspsites )
detcov_C_offset_0 <- rep(0,nspsites )
detcov_C_offset_1 <- rep(0,nspsites )
detvcov_C_offset_0 <- rep(0,nspsites )
detvcov_C_offset_1 <- rep(0,nspsites )
detvcov_0_offset_0 <- rep(0,nspsites )
detvcov_0_offset_1 <- rep(0,nspsites )
sigma_sq_0_offset_0 <- rep(0,nspsites )
sigma_sq_0_offset_1 <- rep(0,nspsites )
sigma_sq_C_offset_0 <- rep(0,nspsites )
sigma_sq_C_offset_1 <- rep(0,nspsites )
AR1_corr_0_offset_0 <- rep(0,nspsites )
AR1_corr_0_offset_1 <- rep(0,nspsites )
AR1_corr_C_offset_0 <- rep(0,nspsites )
AR1_corr_C_offset_1 <- rep(0,nspsites )
fixscaleC_offset_0 <- rep(0,nspsites )
fixscaleC_offset_1 <- rep(0,nspsites )

min_offset <- rep(0,nspsites )
max_offset <- rep(0,nspsites )
min_offset_save <- rep(0,nspsites )
max_offset_save <- rep(0,nspsites )

#
#INSIDE THIS LOOP READ ALL DATA FILES SENT TO DAE
#ALSO READ EACH .Rsave file and save log-likelihoods
#

#nSECUREssp<- 17  #full set
nSECUREssp <- 2  #small selection

for (with_offset in 0:1){
for (SECUREsppick in 1:nSECUREssp) {

#
#with_offset <- 0 #use this to select an individual SECURE model fit
#SECUREsppick <- 1  #use this to select an individual SECURE dataset
#SECUREsppick <-3 

#Full set
#SECUREstems <- c("T06_180","T06_267","T06_285","T06_395","T06_456","T06_467","T06_469","T06_574","T06_717",
#                                "T08_180","T08_382","T08_431","T08_456","T08_574","T08_707","T08_717","T08_864")
#SECUREwhichmetdatafile <-c(rep(1,9),rep(2,8))

#SECUREstems <- c("T08_574","T08_717","T08_707")
SECUREstems <- c("T04_382","T04_724")
SECUREwhichmetdatafile <-c(rep(3,2))


#SECUREmetdatadir <- c("H:/contracts/EPSRC_Secure_Henrys/henrys_data_jan_2017/SitePrecipData")
#SECUREspdatadir <- c("H:/contracts/EPSRC_Secure_Henrys/henrys_data_jan_2017/MothPrecip_Results")
#SECUREresdir <- c("H:/contracts/EPSRC_Secure_Henrys/henrys_data_jan_2017/results/feb_8th")

#SECUREmetdatadir <- c("D:/dae/temp_17_apr/EPSRC_Secure_Henrys/henrys_data_jan_2017/SitePrecipData")
#SECUREspdatadir <- c("D:/dae/temp_17_apr/EPSRC_Secure_Henrys/henrys_data_jan_2017/MothPrecip_Results")
#SECUREresdir <- c("D:/dae/temp_17_apr/EPSRC_Secure_Henrys/henrys_data_jan_2017/results/feb_8th")

SECUREmetdatadir <- c("D:/dae/temp_17_apr/new_EPSRC_Secure_Henrys/data_files")
SECUREspdatadir <- c("D:/dae/temp_17_apr/new_EPSRC_Secure_Henrys/data_files")
SECUREresdir <- c("D:/dae/temp_17_apr/new_EPSRC_Secure_Henrys/data_files")

SECUREmetdatafiles<- c("Rothamsted_Monthly_Precip.csv","Wytham_Monthly_Precip.csv","Moorhouse_Monthly_Precip.csv")  #CHANGE THESE
SECUREmetdatafile <-SECUREmetdatafiles[SECUREwhichmetdatafile[SECUREsppick]]


SECUREstem <- SECUREstems[SECUREsppick]
SECUREspABUNdatafile<-paste(SECUREstem,"_ObsCount.OffsetIncWeather.csv",sep="")
SECUREspWHENdatafile<-paste(SECUREstem,"_DOY_Quantiles.csv",sep="")

SECUREspresfile<-paste(SECUREstem,"_offset_status_",as.character(with_offset),".Rsave",sep="")
print(SECUREspresfile)


setwd(SECUREspdatadir)
yABUNdataSECURE <- read.csv(SECUREspABUNdatafile,header=TRUE)
ycounts<-yABUNdataSECURE[,2]
yoffset<-yABUNdataSECURE[,3]/yABUNdataSECURE[,5]
print(cbind(ycounts,yoffset))

yWHENdataSECURE <- read.csv(SECUREspWHENdatafile,header=TRUE)
yWHENdataSECURE
ycentile <- yWHENdataSECURE[5,2] #row 5 is 80th centile
cum_mon<-c(0,31,59,90,120,151,181,212,243,273,304,334,365)
ymetmonth<-sum(as.numeric(ycentile>cum_mon))
print(c(ycentile,ymetmonth))
SECUREmetmonth<-ymetmonth


setwd(SECUREmetdatadir)
test_data<-read.csv(SECUREmetdatafile,header=TRUE)
#print(test_data)

setwd(SECUREresdir)
load(SECUREspresfile)
print(cbind(neg_logLik0,neg_log_lik_C1))



nspsites <- 17 #number of combinations of species and site, and for the C and null (0) models


if (with_offset==0){
log_Lik_C_offset_0[SECUREsppick] <-  -neg_log_lik_C1
log_Lik_0_offset_0[SECUREsppick] <-  -neg_logLik0
detcov_C_offset_0[SECUREsppick]  <- detcovC
detvcov_C_offset_0[SECUREsppick]  <- detvcovC
detvcov_0_offset_0[SECUREsppick]  <- detvcov0
sigma_sq_0_offset_0[SECUREsppick] <- sigma_sq_0
sigma_sq_C_offset_0[SECUREsppick] <- sigma_sq_C1
AR1_corr_0_offset_0[SECUREsppick] <- corr0
AR1_corr_C_offset_0[SECUREsppick] <- corrC1
#fixscaleC_offset_0[SECUREsppick] <- fixscaleC
}else{
log_Lik_C_offset_1[SECUREsppick] <-  -neg_log_lik_C1
log_Lik_0_offset_1[SECUREsppick] <-  -neg_logLik0
detcov_C_offset_1[SECUREsppick]  <- detcovC
detvcov_C_offset_1[SECUREsppick]  <- detvcovC
detvcov_0_offset_1[SECUREsppick]  <- detvcov0
sigma_sq_0_offset_1[SECUREsppick] <- sigma_sq_0
sigma_sq_C_offset_1[SECUREsppick] <- sigma_sq_C1
AR1_corr_0_offset_1[SECUREsppick] <- corr0
AR1_corr_C_offset_1[SECUREsppick] <- corrC1
#fixscaleC_offset_1[SECUREsppick] <- fixscaleC
}

if (with_offset==1){
min_offset[SECUREsppick]  <- min(log(yoffset),na.rm=TRUE)
max_offset[SECUREsppick]  <- max(log(yoffset),na.rm=TRUE)
min_offset_save[SECUREsppick]  <- min(yoffset,na.rm=TRUE)
max_offset_save[SECUREsppick]  <- max(yoffset,na.rm=TRUE)
}

}} #END OF TEST READING FROM FILES



setwd(SECUREresdir)

cbind(SECUREstems,max_offset,log_Lik_C_offset_0,log_Lik_0_offset_0,log_Lik_C_offset_1,log_Lik_0_offset_1)
write.csv(cbind(SECUREstems,max_offset,log_Lik_C_offset_0,log_Lik_0_offset_0,log_Lik_C_offset_1,log_Lik_0_offset_1),"max_liks.csv")

lrt_0C_offset_0 <- 2*(log_Lik_C_offset_0-log_Lik_0_offset_0)
lrt_0C_offset_1 <- 2*(log_Lik_C_offset_1-log_Lik_0_offset_1)
cbind(SECUREstems,max_offset,lrt_0C_offset_0,lrt_0C_offset_1,min_offset,max_offset)
write.csv(cbind(SECUREstems,lrt_0C_offset_0,lrt_0C_offset_1,min_offset,max_offset),"LRT_offset.csv")

cbind(SECUREstems,max_offset,fixscaleC_offset_0,fixscaleC_offset_1,detvcov_0_offset_0,detvcov_0_offset_1,detvcov_C_offset_0,
                             detvcov_C_offset_1,detcov_C_offset_0,detcov_C_offset_1)
write.csv(cbind(SECUREstems,max_offset,fixscaleC_offset_0,fixscaleC_offset_1,detvcov_0_offset_0,detvcov_0_offset_1,detvcov_C_offset_0,
                             detvcov_C_offset_1,detcov_C_offset_0,detcov_C_offset_1),"det_vcov_and_cov_offset.csv")

cbind(SECUREstems,max_offset,sigma_sq_0_offset_0,sigma_sq_0_offset_1,sigma_sq_C_offset_0,sigma_sq_C_offset_1,
                   AR1_corr_0_offset_0,AR1_corr_0_offset_1,AR1_corr_C_offset_0,AR1_corr_C_offset_1)
write.csv(cbind(SECUREstems,max_offset,sigma_sq_0_offset_0,sigma_sq_0_offset_1,sigma_sq_C_offset_0,sigma_sq_C_offset_1,
                   AR1_corr_0_offset_0,AR1_corr_0_offset_1,AR1_corr_C_offset_0,AR1_corr_C_offset_1),"det_sigmasq_corr_offset.csv")


#pick out entries from results to go in tables
pick <- 1
for (pick in 1:nSECUREssp){
print(SECUREstems[pick])
offset_status <- c(0,1)
max_liks <-(c(log_Lik_C_offset_0[pick],log_Lik_C_offset_1[pick]))
sigma_sqs<- c(sigma_sq_C_offset_0[pick],sigma_sq_C_offset_1[pick])
lrts<- c(lrt_0C_offset_0[pick],lrt_0C_offset_1[pick])
det_es<- c(detvcov_C_offset_0[pick],detvcov_C_offset_1[pick])
det_pars<-c(detcov_C_offset_0[pick],detcov_C_offset_1[pick])
corrs<-c(AR1_corr_C_offset_0[pick],AR1_corr_C_offset_1[pick])
print(cbind(offset_status,max_liks,lrts,sigma_sqs,corrs,det_es,det_pars))
write.csv(cbind(offset_status,max_liks,lrts,sigma_sqs,corrs,det_es,det_pars),paste(SECUREstems[pick],"_Hasselt.csv"))}

#final testing
#setwd(SECUREresdir)
#load("T04_382_offset_status_0.Rsave")
#load("T04_382_offset_status_1.Rsave")
-- 
Biomathematics and Statistics Scotland (BioSS) is formally part of The
James Hutton Institute (JHI), a registered Scottish charity No. SC041796
and a company limited by guarantee No. SC374831