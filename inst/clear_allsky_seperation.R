library(data.table)
library(lubridate)
library(plyr)
library(seas)
source("inst/settings.R")

#functions to preprare the data
read_ECA_info<-function(fname){
  stations<-fread(fname,
                     skip=16,select = c(1,2,3,4,5,6))
  names(stations)<-c("sou_id","name","coun_id","lat","lon","elev")
  
  stations$lat<-as.character(gsub(":"," ",stations$lat))
  stations$lon<-as.character(gsub(":"," ",stations$lon))
  stations$lat = measurements::conv_unit(stations$lat, from = 'deg_min_sec', to = 'dec_deg')
  stations$lon = measurements::conv_unit(stations$lon, from = 'deg_min_sec', to = 'dec_deg')
  return(stations)
}

read_qq_cc<-function(file_qq,file_cc){
  qq<-fread(file_qq)
  qq<-qq[which(qq$Q_QQ==0),]
  qq<-subset(qq,select = c("STAID","DATE","QQ"))
  cc<-fread(file_cc)
  cc<-cc[which(cc$Q_CC==0),]
  cc<-subset(cc,select = c("STAID","DATE","CC"))
  
  qq_cc<-merge(qq,cc,by=c("STAID","DATE"))
  qq_cc$DATE<-as.character(qq_cc$DATE)
  qq_cc$DATE<-as.Date(qq_cc$DATE,format="%Y%m%d")
  return(qq_cc)
}

allsky_monthly_qq_cc <- function(qq_cc=qq_cc_day,frac=0.8){
  qq_cc$month_year<-format(qq_cc$DATE,"%Y-%m")
  qq_cc$nr_days<-days_in_month(qq_cc$DATE)
  qq_cc_month<-ddply(qq_cc,~month_year,summarise,QQm=mean(QQ),frac=length(QQ)/unique(nr_days))
  qq_cc_month<-qq_cc_month[which(qq_cc_month$frac>frac),]
  
  #set the dates and seasons back into the df
  qq_cc_month$month_year<-as.Date(paste0(qq_cc_month$month_year,"-01"),format="%Y-%m-%d")
  qq_cc_month$season <- mkseas(x = qq_cc_month$month_year, width = "DJF")
  qq_cc_month$year <- year(qq_cc_month$month_year)
  return(qq_cc_month)
}

clearsky_monthly_qq_cc <- function(qq_cc=qq_cc_day,min_obs=2){
  qq_cc <- qq_cc[which(qq_cc$CC==0 | qq_cc$CC==1),]
  qq_cc$month_year<-format(qq_cc$DATE,"%Y-%m")
  qq_cc_month<-ddply(qq_cc,~month_year,summarise,QQm=mean(QQ),frac=length(QQ)/min_obs)
  qq_cc_month<-qq_cc_month[which(qq_cc_month$frac>=1),]
  
  #set the dates and seasons back into the df
  qq_cc_month$month_year<-as.Date(paste0(qq_cc_month$month_year,"-01"),format="%Y-%m-%d")
  qq_cc_month$season <- mkseas(x = qq_cc_month$month_year, width = "DJF")
  qq_cc_month$year <- year(qq_cc_month$month_year)
  return(qq_cc_month)
}

seasonal_trend <- function(qq_cc_month){
  qq_cc_season<-ddply(qq_cc_month,~season+year,summarise,QQs=mean(QQm),frac=length(QQm)/3)
  qq_cc_season<-qq_cc_season[which(qq_cc_season$frac==1),]
  qq_cc_season <- qq_cc_season %>% group_by(season) %>% mutate(Nor = scale(QQs)) 
  return(qq_cc_season)
}

#Matching the metadata of qq and cc
stations_qq<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$global_radiation,"/ECA_nonblend_info_qq.txt"))
stations_cc<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$cloud_cover,"/ECA_nonblend_info_cc.txt"))

stations_join<-merge(stations_qq,stations_cc,by=c("name","coun_id","lat","lon","elev"))
stations_join$fname_qq<-paste0("QQ_SOUID",stations_join$sou_id.x,".txt")
stations_join$fname_cc<-paste0("CC_SOUID",stations_join$sou_id.y,".txt")
#this leaves only 78 joined stations within Europe for qq and cc

#Combine the QQ and CC files with an inner merge
qq_cc_day <- read_qq_cc(file_qq = paste0(file_loc$main_loc,file_loc$global_radiation,"/",stations_join$fname_qq[1]),
           file_cc = paste0(file_loc$main_loc,file_loc$cloud_cover,"/",stations_join$fname_cc[1]))

#Get monthly all sky and clear sky values
qq_cc_allsky <- allsky_monthly_qq_cc(qq_cc_day,frac=0.8)
qq_cc_clearsky <- clearsky_monthly_qq_cc(qq_cc_day,min_obs = 2)

#seasonal and annual anomalies
qq_cc_allsky_season <- seasonal_trend(qq_cc_allsky)
qq_cc_clearsky_season <- seasonal_trend(qq_cc_clearsky)

#visualization of trends
library(ggplot2)
library(gridExtra)

p1<-ggplot(qq_cc_clearsky_season,aes(year,Nor))+
  facet_grid(.~season)+
  geom_point()+ggtitle("Clear sky")+ylab("norm. qq")+theme_bw()+geom_smooth(method="lm")
p2<-ggplot(qq_cc_allsky_season,aes(year,Nor))+
  facet_grid(.~season)+
  geom_point()+ggtitle("All sky")+ylab("norm. qq")+theme_bw()+geom_smooth(method="lm")
grid.arrange(p1,p2)

