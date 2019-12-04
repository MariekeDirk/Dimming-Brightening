library(data.table)
library(dplyr)
library(DimBri)
library(lubridate)
library(plyr)
library(raster)
library(seas)
library(sp)
library(tidyr)
source("inst/settings.R")

#####Compute a correlation matrix from the observations
#to add:
#optional: decorrelation length: e-1

############################Gap Filling

#There is something wrong with the ids in the QQ_SOUID file, they do not match the ones in the
#stations_qq file, try matching lat lon with souids?
qq_meta<-read_ECA_info(fname = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/stations_qq.txt"))
# qq_meta<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$global_radiation,"/ECA_nonblend_info_qq.txt"))
names(qq_meta)<-c("STAID","name","coun_id","lat","lon","elev")
qq_meta$lat<-as.numeric(qq_meta$lat)
qq_meta$lon<-as.numeric(qq_meta$lon)
staids<-unique(qq_meta$STAID)

#prepare all sky monthly data
qq_monthly<-fread(paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly.txt"))
qq_monthly<-subset(qq_monthly,select = c("month_year","QQm","STAID"))
qq_monthly$month_year<-as.Date(qq_monthly$month_year)

#define file name to which fill values should be written and remove if it already exists
# fname=paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_fill.txt")
# system(sprintf("rm %s",fname))
fill_time_series(staids,qq_meta,qq_monthly,fname)

#prepare clear sky monthly data
qq_monthly_cs<-fread(paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt"))
qq_monthly_cs<-subset(qq_monthly_cs,select = c("month_year","QQm","STAID"))
qq_monthly_cs$month_year<-as.Date(qq_monthly_cs$month_year)

fname=paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs_fill.txt")
# system(sprintf("rm %s",fname))
fill_time_series(staids,qq_meta,qq_monthly_cs,fname)
#####

#Programming wise:
#(1) create a new empty file to save the filling results (prevent the filled values to be used again)
#(2) take one sta_id: find a gap, if no gap, continue with the next
#(3) which month falls the gap in?
#(4) if there are at least 6 values in the same month corralate with other series
#(5) if the 5 highest correlated series fall within 500km take the median
#(6) write filled values to file, append=TRUE

#(7) create a new filled qq file and merge with cc file and continue with the analysis below

#Manara (2016) used the 5 highest correlated stations median value
#requirements:
#(1) five stations within 500km range
#(2) six other monthly values in common with the month of the gap (6 januari averages for example)
#####################


#Get monthly all sky and clear sky values
qq_cc_allsky <- allsky_monthly_qq_cc(qq_cc_day,frac=0.8)
qq_cc_clearsky <- clearsky_monthly_qq_cc(qq_cc_day,min_obs = 2)

#seasonal and annual anomalies
qq_cc_allsky_season <- seasonal_trend(qq_cc_allsky)
qq_cc_clearsky_season <- seasonal_trend(qq_cc_clearsky)


#fit the lm to the different seasons
qq_cc_fit_allsky <- with(qq_cc_allsky_season,
               by(qq_cc_allsky_season, season,
                  function(x) fit_trend(x)))

qq_fit<-lapply(qq_cc_fit_allsky,"[[",2)
qq_fit<-do.call("rbind",qq_fit)

qq_cc_fit_allsky <- lapply(qq_cc_fit_allsky,"[[",1)
qq_cc_fit_allsky <- do.call("rbind",qq_cc_fit_allsky)

qq_cc_fit_clearsky <- with(qq_cc_clearsky_season,
                          by(qq_cc_clearsky_season, season,
                             function(x) fit_trend(x)))
qq_cc_fit_clearsky <- do.call("rbind",qq_cc_fit_clearsky)

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

