library("plyr")
library("YieldCurve")
library("quantmod")
library("reshape2")
library("corrplot")
library("ggplot2")



plots_NS <- list()

#####################  first download the data for US rates #####################

US1Y<-as.xts(na.omit(getSymbols("DGS1",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US2Y<-as.xts(na.omit(getSymbols("DGS2",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US3Y<-as.xts(na.omit(getSymbols("DGS3",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US5Y<-as.xts(na.omit(getSymbols("DGS5",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US7Y<-as.xts(na.omit(getSymbols("DGS7",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US10Y<-as.xts(na.omit(getSymbols("DGS10",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US20Y<-as.xts(na.omit(getSymbols("DGS20",src='FRED',from="1970-01-01",auto.assign=FALSE)))
US30Y<-as.xts(na.omit(getSymbols("DGS30",src='FRED',from="1970-01-01",auto.assign=FALSE)))

## combine the data
USR<-cbind.xts(US1Y,US2Y,US3Y,US5Y,US7Y,US10Y,US20Y,US30Y)
colnames(USR)<-c("US1Y","US2Y","US3Y","US5Y","US7Y","US10Y","US20Y","US30Y")  

USR.DF<-data.frame(index(USR),coredata(USR),stringsAsFactors=FALSE)
names(USR.DF)<-c("Date","US1Y","US2Y","US3Y","US5Y","US7Y","US10Y","US20Y","US30Y")

## from wide to long format
USRM<-melt(USR.DF,id.vars="Date",variable.name="US_Rate",value.name="Level")

###############################################################################################
## Nelson Siegel Calibration for last date
NSParameters <- Nelson.Siegel( rate = USR[nrow(USR),], maturity=c(1,2,3,5,7,10,20,30) )

## now calcualting all ratios for 2014
NSdc<-Nelson.Siegel(rate=USR['2014'], maturity=c(1,2,3,5,7,10,20,30))
NS_US_Rates<-NSrates(NSdc, maturity=seq(1,30,1))
NS_US_Rates.df<-data.frame(index(NS_US_Rates),coredata(NS_US_Rates),stringsAsFactors=FALSE)
names(NS_US_Rates.df)<-c("Date",paste0("R",seq(1,30,1)))
NSRm<-melt(NS_US_Rates.df,id="Date",variable.name="Maturity",value.name="Rate")
NSRm$Maturity <- rep(seq(1,30,1),each=250)


### correlation NS curves compared to true correlations for 2014
par(mfrow=c(1,2))
corrplot.mixed(cor(NS_US_Rates[,c(1,2,3,5,7,10,20,30)]),lower="ellipse",upper="number")
corrplot.mixed(cor(USR['2014']),lower="ellipse",upper="number")
par(mfrow=c(1,1))

## reorder for plotting
NSRmc<-NSRm[order(as.numeric(NSRm$Date),NSRm$Maturity),]
NSRmc$Maturity <-  seq(1,30,1)

## NS Curves for d # of days
d=20
print(plots_NS[[1]] <- ggplot(NSRmc[(nrow(NSRmc)-(30*d-1)):nrow(NSRmc),],
                              aes(x=Maturity,y=Rate,Group=Date))+geom_point(colour="cadetblue",size=rel(3))+
        facet_wrap(~Date,ncol=5))

### now checking all data point for the last 2 months
USR14<-USR['201411/201412']
USR14.df<-data.frame(index(USR14),coredata(USR14),stringsAsFactors=FALSE)
names(USR14.df)[1]<-"Date"
USR14M<-melt(USR14.df,id.vars="Date",variable.name="Maturity",value.name="Rate")
USR14M$Maturity<-rep(c(1,2,3,5,7,10,20,30),each=dim(USR14)[1])


## check calibration Nelson Siegel against market Data
print(plots_NS[[2]] <- ggplot(NSRmc[(nrow(NSRmc)-(30*ndays(USR14)-1)):nrow(NSRmc),],aes(x=Maturity,y=Rate))+
        geom_point(colour="cadetblue",size=rel(3))+
        geom_point(data=USR14M,aes(x=Maturity,y=Rate),colour="red",size=rel(3))+
        facet_wrap(~Date,ncol=5)+theme_bw()+
        scale_x_continuous(breaks=c(1,5,10,15,20,25,30)))

###########################################################
#### get a understanding of the different curve moves in 2014 & calibration NS
USR14a.df<-data.frame(index(USR['2014']),coredata(USR['2014']),stringsAsFactors=FALSE)
names(USR14a.df)<-c("Date",paste0("R",c(1,2,3,5,7,10,20,30)))
USR14Ma<-melt(USR14a.df,id.vars="Date",variable.name="Maturity",value.name="Rate")
USR14Ma$Maturity <- rep(as.numeric(c(1,2,3,5,7,10,20,30)),each=250)

print(plots_NS[[3]] <- ggplot(NSRm,aes(y=Rate,x=Maturity))+geom_point(size=rel(3),colour="midnightblue")+
        geom_point(data=USR14Ma,aes(y=Rate,x=Maturity),colour="red",shape=21,fill="pink"))

##########################################################################################
### Capture the errors per data and run stats over it, both for Nelson-Siegel & Svensson

USR14Mb<-melt(NS_US_Rates.df[,c(1,2,3,4,6,8,11,21,31)],id.vars="Date",variable.name="Maturity",value.name="Rate")

USR14M_diff<-USR14Ma
USR14M_diff$Calibration_error<-USR14Ma$Rate-USR14Mb$Rate

head(USR14M_diff)

## 2014 errors
NS_error<-ddply(USR14M_diff,.(Maturity),summarise,mean=mean(Calibration_error,na.rm = TRUE),
                median=median(Calibration_error,na.rm = TRUE),perc25=quantile(Calibration_error,0.25,na.rm = TRUE),
                perc75=quantile(Calibration_error,0.75,na.rm = TRUE),
                max=max(Calibration_error,na.rm = TRUE),
                min=min(Calibration_error,na.rm = TRUE),
                sum=sum(Calibration_error,na.rm = TRUE))
NS_error$Year<-"2014"

## graphically capturing the errors
print(plots_NS[[4]] <- ggplot(NS_error,aes(x=Maturity,y=mean,ymin=min,ymax=max))+geom_pointrange())


########################################################################################################
## now for the last four years
## Nelson-Siegel
NSdc13<-Nelson.Siegel(rate=USR['2010/2014'], maturity=c(1,2,3,5,7,10,20,30))
NS_US_Rates13<-NSrates(NSdc13, maturity=seq(1,30,1))
NS_US_Rates13.df<-data.frame(index(NS_US_Rates13),coredata(NS_US_Rates13),stringsAsFactors=FALSE)
names(NS_US_Rates13.df)<-c("Date",paste0("R",seq(1,30,1)))

USR13Mb<-melt(NS_US_Rates13.df[,c(1,2,3,4,6,8,11,21,31)],id.vars="Date",variable.name="Maturity",value.name="Rate")

USR13a.df<-data.frame(index(USR['2010/2014']),coredata(USR['2010/2014']),stringsAsFactors=FALSE)
names(USR13a.df)<-c("Date",paste0("R",c(1,2,3,5,7,10,20,30)))
USR13Ma<-melt(USR13a.df,id.vars="Date",variable.name="Maturity",value.name="Rate")

USR13M_diff<-USR13Ma
USR13M_diff$Calibration_error<-USR13Ma$Rate-USR13Mb$Rate
USR13M_diff$Year<-factor(format(USR13M_diff$Date,'%Y'))

### Svensson
SVdc13<-Svensson(rate=USR['2010/2014'], maturity=c(1,2,3,5,7,10,20,30))
SV_US_Rates13<-Srates(SVdc13, maturity=seq(1,30,1),"Spot")
SV_US_Rates13.df<-data.frame(index(SV_US_Rates13),coredata(SV_US_Rates13),stringsAsFactors=FALSE)
names(SV_US_Rates13.df)<-c("Date",paste0("R",seq(1,30,1)))

USR13Mc<-melt(SV_US_Rates13.df[,c(1,2,3,4,6,8,11,21,31)],id.vars="Date",variable.name="Maturity",value.name="Rate")

USR13M_diff$CalError_SV<-USR13Ma$Rate-USR13Mc$Rate


NS_error_all<-ddply(USR13M_diff,.(Maturity,Year),summarise,mean=mean(Calibration_error,na.rm = TRUE),
                    median=median(Calibration_error,na.rm = TRUE),perc25=quantile(Calibration_error,0.25,na.rm = TRUE),
                    perc75=quantile(Calibration_error,0.75,na.rm = TRUE),
                    max=max(Calibration_error,na.rm = TRUE),
                    min=min(Calibration_error,na.rm = TRUE),
                    sum=sum(Calibration_error,na.rm = TRUE))
NS_error_all$Type<-"Nelson-Siegel"

SV_error_all<-ddply(USR13M_diff,.(Maturity,Year),summarise,mean=mean(CalError_SV,na.rm = TRUE),
                    median=median(CalError_SV,na.rm = TRUE),perc25=quantile(CalError_SV,0.25,na.rm = TRUE),
                    perc75=quantile(CalError_SV,0.75,na.rm = TRUE),
                    max=max(CalError_SV,na.rm = TRUE),
                    min=min(CalError_SV,na.rm = TRUE),
                    sum=sum(CalError_SV,na.rm = TRUE))
SV_error_all$Type<-"Svensson"

NSSV_error_all<-rbind(NS_error_all,SV_error_all)


## plotting the Nelson-Siegel Errors over 2010/2014
print(plots_NS[[5]] <-ggplot(NS_error_all,aes(x=Maturity,y=mean))+
        geom_crossbar(aes(ymin=perc25,ymax=perc75),width=0.25,fatten=0.25,colour="#e31a1c",fill="grey50",alpha=0.4)+
        geom_pointrange(aes(ymin=min,ymax=max),colour="#e31a1c")+
        facet_wrap(~Year,nrow=1)+geom_hline(yintercept=0,colour="grey50",linetype=2)+
        theme_bw()+coord_flip()+ylab("Error"))


## nice red (stay away from yellow!!)
print(plots_NS[[6]] <- ggplot(NS_error_all,aes(x=Maturity,y=mean))+
        geom_crossbar(aes(ymin=perc25,ymax=perc75),width=0.25,fatten=0.25,colour="#e31a1c",fill="grey20",alpha=0.4)+
        geom_pointrange(aes(ymin=min,ymax=max),colour="#e31a1c")+
        facet_wrap(~Year,nrow=1)+
        theme_bw()+coord_flip()+ylab("Error"))


########### plotting both Nelson-Siegel Errors & Svensson
## plotting the Nelson-Siegel Errors over 2010/2014
print(plots_NS[[7]] <-ggplot(NSSV_error_all)+
        geom_pointrange(aes(x=Maturity,y=mean,ymin=min,ymax=max,colour=Type),position=position_dodge(width=c(0.6,0.4)))+
        facet_wrap(~Year,nrow=1)+geom_hline(yintercept=0,colour="grey50",linetype=2)+
        theme_bw()+coord_flip()+ylab("Error")+scale_colour_manual(values=c("#e31a1c","#3f007d"))+
        theme(legend.position="top"))


##### Printing all plots to jpeg files #####
invisible(
  lapply(
    seq_along(plots_NS), 
    function(x) ggsave(filename=paste0("NS", x, ".png"), plot=plots_NS[[x]])
  ) )





