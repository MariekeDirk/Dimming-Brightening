
library(data.table)
library(htmltools)
library(greenbrown)
library(mapview)
library(plyr)
library(xts)
source("inst/settings.R")
#Read all sky and clear sky data
series_cs<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt"))
qq_monthly_com<-with(series_cs,by(series_cs,STAID,function(x) completeness.series(x)))
qq_monthly_com<-do.call("rbind",qq_monthly_com)
mean(qq_monthly_com$completeness)

series_cs_fill<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs_fill.txt"))
series_cs_fill<-rbind(series_cs,series_cs_fill)
qq_monthly_com_f<-with(series_cs_fill,by(series_cs_fill,STAID,function(x) completeness.series(x)))
qq_monthly_com_f<-do.call("rbind",qq_monthly_com_f)
mean(qq_monthly_com_f$completeness)

series<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly.txt"))
qq_m_c<-with(series,by(series,STAID,function(x) completeness.series(x)))
qq_m_c<-do.call("rbind",qq_m_c)
mean(qq_m_c$completeness)

series_fill<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_fill.txt"))
series_fill<-rbind(series,series_fill)
qq_m_c_f<-with(series_fill,by(series_fill,STAID,function(x) completeness.series(x)))
qq_m_c_f<-do.call("rbind",qq_m_c_f)
mean(qq_m_c_f$completeness)

#Make a plot with start years and completeness for Europe
qq_meta<-read_ECA_info(fname = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/stations_qq.txt"))
# qq_meta<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$global_radiation,"/ECA_nonblend_info_qq.txt"))
names(qq_meta)<-c("STAID","name","coun_id","lat","lon","elev")
qq_meta$lat<-as.numeric(qq_meta$lat)
qq_meta$lon<-as.numeric(qq_meta$lon)
qq_meta$STAID<-as.character(qq_meta$STAID)

qq_m_c_f$STAID<-as.character(qq_m_c_f$STAID)

#visualize the timeseries on a map with start year, completeness and stop year
qq_m_c_f$completeness[which(qq_m_c_f$completeness>1)]<-1
qq_m_c_f$start.year<-as.numeric(year(qq_m_c_f$start))
qq_m_c_f$stop.year<-as.numeric(year(qq_m_c_f$stop))
qq_m_c_f$measurement.years<-qq_m_c_f$stop.year-qq_m_c_f$start.year



#What is the completeness of stations with a start around 1965 and stop around 2018?
get_spatial_coverage<-function(meta=qq_meta,
                               stations_info=qq_m_c_f,
                               t1=1980,t2=2017){
  qq_sub<-stations_info[which(stations_info$start.year<t1 & stations_info$stop.year>t2),]

  qq_sp<-merge(meta,qq_sub,by="STAID")

  # leaflet(qq_sp) %>% addTiles() %>%
  #   addCircleMarkers(~lon, ~lat, label = ~htmlEscape(name),
  #              labelOptions = labelOptions(noHide = TRUE,textOnly = FALSE),
  #              options = markerOptions(riseOnHover = TRUE))

  coordinates(qq_sp)<-~lon+lat
  crs(qq_sp)<-file_loc$CRS.arg

  m<-mapview(qq_sp,zcol="completeness",at=seq(0,1,0.001))
  return(list("map"=m,"sp"=qq_sp))
}

long_records<-get_spatial_coverage(t1=1966)

df.long_records<-data.frame(long_records$sp)
df.long_records$name<-gsub("/","-",df.long_records$name)
#Trend analysis
#Series from de Bilt
#De Bilt 162
#Potsdam 54
#Stockholm 6930
setkey(series_fill,"month_year")
setkey(series_cs_fill,"month_year")

#to do: concider the same time period for all the series with as start t1
#to do: the number of breaks which can be detected breaks
#change in the function get_change_point to a year number:
#h.years=10, length.series=65, breaks=floor(65/10)
for(i in 1:length(df.long_records$STAID)){
nr=df.long_records$STAID[i]
nm=df.long_records$name[i]
cn=df.long_records$coun_id[i]
lng=df.long_records$measurement.years[i]
# qq_m_c_f[which(qq_m_c_f$STAID==nr),]
# qq_monthly_com_f[which(qq_monthly_com_f$STAID==nr),]
# timeseries_cs<-series_cs_fill[which(series_cs_fill$STAID==nr),]
timeseries_allsky<-series_fill[which(series_fill$STAID==nr),]
trend.allsky<-get_change_point(timeseries_allsky,station.name = paste0("coun_id= ",cn,", name= ",nm),brk.max = floor(lng/15))

trend.allsky$plot
ggsave(filename=paste0("C:/Users/marie/Pictures/",nm,".png"))
# trend.cs<-get_change_point(timeseries_cs)
}


