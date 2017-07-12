

#########################

#only use data without any NAs in. so first identify these full rows in the data
no.na <- which(!is.na(red_dat$LL) & !is.na(red_dat$Weather) & !is.na(red_dat$VALUE))


#use nls to get parameter estimates 
(res <- nls(log(VALUE+1) ~ beta0+beta01*Weather+(beta1*YEAR1993+beta2*YEAR1994+beta3*YEAR1995+beta4*YEAR1996+beta5*YEAR1997+beta6*YEAR1998+beta7*YEAR1999+beta8*YEAR2000+beta9*YEAR2001+beta10*YEAR2002+beta11*YEAR2003+beta12*YEAR2004+beta13*YEAR2005+beta14*YEAR2006+beta15*YEAR2007+beta16*YEAR2008+beta17*YEAR2009+beta18*YEAR2010+beta19*YEAR2011+beta20*YEAR2012)*exp(-1/2*(DOY-mu)^2/((sbeta1*(YEAR1993+YEAR1994+YEAR1995+YEAR1996+YEAR1997)+sbeta2*(YEAR1998+YEAR1999+YEAR2000+YEAR2001+YEAR2002)+sbeta3*(YEAR2003+YEAR2004+YEAR2005+YEAR2006+YEAR2007)+sbeta4*(YEAR2008+YEAR2009+YEAR2010+YEAR2011+YEAR2012)))^2), 
start=c(mu=mean(red_dat$DOY),beta0=0,beta01=1,beta1=1,beta2=2,beta3=1,beta4=1,beta5=1,beta6=1,beta7=1,beta8=1,beta9=1,beta10=1,beta11=1,beta12=1,beta13=1,beta14=1,beta15=1,beta16=1,beta17=1,beta18=1,beta19=1,beta20=1,sbeta1=10,sbeta2=10,sbeta3=10,sbeta4=10) , 
data = red_dat))

#save estimated parameters to use as staring values in mixed model 
v <- summary(res)$parameters[,"Estimate"]


#####################
#####################

### Now use mixed effects model so year can be included as random effect. 
##yearly varying and LL

library(nlme)

#specify function based on gaussian basis with yearly mean in and block of standard deviations controlling width (4 different ones)
md_fn <- function(mu,beta0,beta01,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20,sbeta1,sbeta2,sbeta3,sbeta4,DOY,YEAR1993,YEAR1994,YEAR1995,YEAR1996,YEAR1997,YEAR1998,YEAR1999,YEAR2000,YEAR2001,YEAR2002,YEAR2003,YEAR2004,YEAR2005,YEAR2006,YEAR2007,YEAR2008,YEAR2009,YEAR2010,YEAR2011,YEAR2012,LL,Weather){
  lin_pred <- beta0+beta01*Weather+(beta1*YEAR1993+beta2*YEAR1994+beta3*YEAR1995+beta4*YEAR1996+beta5*YEAR1997+beta6*YEAR1998+beta7*YEAR1999+beta8*YEAR2000+beta9*YEAR2001+beta10*YEAR2002+beta11*YEAR2003+beta12*YEAR2004+beta13*YEAR2005+beta14*YEAR2006+beta15*YEAR2007+beta16*YEAR2008+beta17*YEAR2009+beta18*YEAR2010+beta19*YEAR2011+beta20*YEAR2012)*exp(-1/2*(DOY-mu)^2/((sbeta1*(YEAR1993+YEAR1994+YEAR1995+YEAR1996+YEAR1997)+sbeta2*(YEAR1998+YEAR1999+YEAR2000+YEAR2001+YEAR2002)+sbeta3*(YEAR2003+YEAR2004+YEAR2005+YEAR2006+YEAR2007)+sbeta4*(YEAR2008+YEAR2009+YEAR2010+YEAR2011+YEAR2012)))^2)
  lin_pred
}


#use function specified and starting values from nls model to estimate parameters whilst including year as random effect
detect_mod <- try(nlme(log(VALUE+1) ~ md_fn(mu,beta0,beta01,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20,sbeta1,sbeta2,sbeta3,sbeta4,DOY,YEAR1993,YEAR1994,YEAR1995,YEAR1996,YEAR1997,YEAR1998,YEAR1999,YEAR2000,YEAR2001,YEAR2002,YEAR2003,YEAR2004,YEAR2005,YEAR2006,YEAR2007,YEAR2008,YEAR2009,YEAR2010,YEAR2011,YEAR2012,LL,Weather),
        fixed=mu+beta0+beta01+beta1+beta2+beta3+beta4+beta5+beta6+beta7+beta8+beta9+beta10+beta11+beta12+beta13+beta14+beta15+beta16+beta17+beta18+beta19+beta20+sbeta1+sbeta2+sbeta3+sbeta4~1,
        random=beta0 ~ 1|RND_Yr,
        data=red_dat,
        start=v,
		control=nlmeControl(opt="nlminb"),
		method="ML"
		))
		
		
#save the estimated coefficients and their estimated standard error
coef_est <- summary(detect_mod)$tTable[,1]		
coef_std.err <- 	summary(detect_mod)$tTable[,2]		


v  <- coef_est #+ (rnorm(1)*coef_std.err)

#####################
#####################

## Predict and estimate

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

##if any mismatch between covariate data and sample data, remove the row so that this is only based on the sample times
if(any(!is.element(full_covs$YEAR,red_dat$YEAR))){
  full_covs <- full_covs[-which(!is.element(full_covs$YEAR,red_dat$YEAR)),]
}


#function to predict based on model parameters estimated in mixed model
pred_fn <- function(PAR,DOY,Weather,Yrid,sg.id){
return(PAR[2]+PAR[3]*Weather+PAR[Yrid]*exp(-1/2*(DOY-PAR[1])^2/PAR[sg.id]^2))
}

##predict the count at each sample point based on the observed covariates over the sample period
full_covs$Pred = exp(pred_fn(PAR=v,DOY=full_covs$DOY,Weather=(full_covs$Weather),Yrid=full_covs$YEAR-1989,sg.id=full_covs$SG_id+23))-1

#plot the predicted counts to check all looks ok
plot(full_covs$DOY,full_covs$Pred)


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


full_grid$Pred = exp(pred_fn(PAR=v,DOY=full_grid$DOY,Weather=full_grid$Weather,Yrid=full_grid$YEAR-1989,sg.id=full_grid$SG_id+23))-1
full_grid$PredDry = exp(pred_fn(PAR=v,DOY=full_grid$DOY,Weather=full_grid$Dry,Yrid=full_grid$YEAR-1989,sg.id=full_grid$SG_id+23))-1

yrest <- tapply(full_covs$Pred,full_covs$YEAR,sum)
yr.id <- match(1993:2012,names(yrest))
exp_cnt=data.frame(YEAR=1993:2012,Exp_Sample=yrest[yr.id],Exp_Full=tapply(full_grid$Pred,full_grid$YEAR,sum)[yr.id],Exp_Full_Dry=tapply(full_grid$PredDry,full_grid$YEAR,sum)[yr.id])

#write.csv(exp_cnt,file=paste(st,sp,"Offset_IncWeather.csv",sep="_"),row.names=FALSE)

