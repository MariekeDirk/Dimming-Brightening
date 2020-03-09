## code to prepare `qq_start_stop` dataset goes here
library(DimBri)
source("inst/settings.R")
#Read all sky and clear sky data
# series_cs<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt"))
# qq_monthly_com<-with(series_cs,by(series_cs,STAID,function(x) completeness.series(x)))
# qq_monthly_com<-do.call("rbind",qq_monthly_com)
# mean(qq_monthly_com$completeness)
#
# series_cs_fill<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs_fill.txt"))
# series_cs_fill<-rbind(series_cs,series_cs_fill)
# qq_monthly_com_f<-with(series_cs_fill,by(series_cs_fill,STAID,function(x) completeness.series(x)))
# qq_monthly_com_f<-do.call("rbind",qq_monthly_com_f)
# mean(qq_monthly_com_f$completeness)

series<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly.txt"))
qq_m_c<-with(series,by(series,STAID,function(x) completeness.series(x)))
qq_m_c<-do.call("rbind",qq_m_c)

series_fill<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_fill.txt"))
series_fill<-rbind(series,series_fill)
qq_m_c_f<-with(series_fill,by(series_fill,STAID,function(x) completeness.series(x)))
qq_m_c_f<-do.call("rbind",qq_m_c_f)

qq_m_c_f$STAID<-as.character(qq_m_c_f$STAID)

#visualize the timeseries on a map with start year, completeness and stop year
qq_m_c_f$completeness[which(qq_m_c_f$completeness>1)]<-1
qq_m_c_f$start.year<-as.numeric(year(qq_m_c_f$start))
qq_m_c_f$stop.year<-as.numeric(year(qq_m_c_f$stop))
qq_m_c_f$measurement.years<-qq_m_c_f$stop.year-qq_m_c_f$start.year
qq_start_stop <- qq_m_c_f
usethis::use_data(qq_start_stop)
