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

############################Gap Filling

#There is something wrong with the ids in the QQ_SOUID file, they do not match the ones in the
#stations_qq file, try matching lat lon with souids?
qq_meta<-read_ECA_info(fname = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/stations_qq.txt"))
names(qq_meta)<-c("STAID","name","coun_id","lat","lon","elev")
qq_meta$lat<-as.numeric(qq_meta$lat)
qq_meta$lon<-as.numeric(qq_meta$lon)

qq_monthly<-fread(paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly.txt"))
qq_monthly_cs<-fread(paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt"))

qq_monthly<-subset(qq_monthly,select = c("month_year","QQm","STAID"))
qq_monthly$month_year<-as.Date(qq_monthly$month_year)
#start with the filling of 1 stations
staids<-unique(qq_monthly$STAID)

qq_series<-qq_monthly[which(qq_monthly$STAID==staids[4]),]
measurement_period<-seq(from=min(qq_series$month_year),
                        to=max(qq_series$month_year),by="month")
if(length(measurement_period)==length(qq_series$month_year)){
  message("no gaps in this series")
  return(NULL)
}

missing_dates <- subset(measurement_period, !(measurement_period %in% qq_series$month_year))


#####Compute a correlation matrix from the observations
#to add:
# normalize data seasonal cycle
#optional: decorrelation length: e-1

#'Correlation with other stations
#'@description calculates the correlation coefficient from the series with gaps to all the other
#'series and computes a buffer with a 500km radius.
#'@param qq_meta metadata with lat lon from all the stations
#'@param qq_series series with missing data
#'@param qq_monthly full data series which will be used to fill the gap
#'@param missing_date missing month from the qq_series
#'@param buffer_dist minimum distance to the filling stations in km. Returning 1 if the qq_monthly stations
#'falls within the buffer zone.
get_corr_stations <- function(qq_meta,qq_series,qq_monthly,
                              missing_date=missing_dates[[1]],buffer_dist=500){

fill_STAID<-unique(qq_monthly$STAID[which(qq_monthly$month_year==missing_date)])

qq_monthly_fill <- qq_monthly[qq_monthly$STAID %in% fill_STAID]

#subset the days to the common period to construct a correlation matrix
qq_monthly_fill <- qq_monthly_fill[qq_monthly_fill$month_year %in% qq_series$month_year]
###Here we can make a subset of stations which have at least 6 months in common

###
qq_monthly_fill <- unique(qq_monthly_fill)
qq_monthly_fill$month <- month(qq_monthly_fill$month_year)
qq_monthly_fill <- qq_monthly_fill[complete.cases(qq_monthly_fill),]

qq_monthly_trend <- aggregate(QQm ~ month+STAID, qq_monthly_fill, mean)
names(qq_monthly_trend) <-c("month","STAID","trend")

qq_monthly_dt <- merge(qq_monthly_fill,qq_monthly_trend,by=c("STAID","month"))
qq_monthly_dt$norm <- qq_monthly_dt$QQm/qq_monthly_dt$trend

qq_matrix<-dcast(qq_monthly_dt,
            month_year~STAID,
            value.var="norm",fun.aggregate = mean)

# qq_matrix<-spread(qq_monthly_dt,key=STAID,value=norm)


qq_series$month <- month(qq_series$month_year)
qq_series_dt <- aggregate(QQm ~ month+STAID, qq_series, mean)
names(qq_series_dt) <-c("month","STAID","trend")
qq_series_dt <- merge(qq_series,qq_series_dt,by=c("STAID","month"))
qq_series_dt$norm <- qq_series_dt$QQm/qq_series_dt$trend

matrix.corr <- cor(qq_series_dt$norm,qq_matrix[,2:ncol(qq_matrix)],use="pairwise.complete.obs")

df.corr <- data.table(t(matrix.corr))
df.corr$STAID <- as.integer(rownames(df.corr))
names(df.corr)<-c("correlation_coef","STAID")
setkey(df.corr,cols="correlation_coef")

# df.corr <- rbind(df.corr,data.frame("correlation_coef"=1,"STAID"=unique(qq_series$STAID)))

p1 <- data.frame("correlation_coef"=1,"STAID"=unique(qq_series$STAID))
p1 <- merge(p1,qq_meta,by="STAID")
coordinates(p1)<-~lon+lat
proj4string(p1)<-CRS("+init=epsg:4326")

p1.RD <- spTransform(p1,CRS("+init=epsg:28992"))#use a meter projection for the buffer
p1.b <- buffer(p1.RD,width=buffer_dist*1000)
p1.b <- spTransform(p1.b,CRS("+init=epsg:4326"))#set back to the original crs

#####Spatial subset within 500km range
df.corr <- merge(df.corr,qq_meta,by="STAID") #correlations with name,coun_id,elev and lat lon
sp.corr <- df.corr
sp.corr$lat<-as.numeric(sp.corr$lat)
sp.corr$lon<-as.numeric(sp.corr$lon)
coordinates(sp.corr) <- ~lon+lat
proj4string(sp.corr)<-CRS("+init=epsg:4326")

# mapview(p1.b,color="red")+sp.corr

sp.corr$buffer_range <- over(sp.corr,p1.b)

df.correlation <- data.frame(sp.corr)
df.correlation <- df.correlation[which(sp.corr$buffer_range==1),]

qq_fill <- median(qq_monthly$QQm[which(qq_monthly$month_year==missing_date &
                                         qq_monthly$STAID %in% df.correlation$STAID)],na.rm=TRUE)

return(list("df"=data.frame(sp.corr),"fill_value"=qq_fill))
}

df.out<-get_corr_stations(qq_meta,qq_series,qq_monthly)
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

