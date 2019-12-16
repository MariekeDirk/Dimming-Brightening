library(data.table)
library(DimBri)
library(ggplot2)
library(lubridate)
library(plyr)
library(seas)
source("inst/settings.R")

############################Gap Filling
#write away all the monthly values with sta_id before gap filling
# qq_files<-list.files(paste0(file_loc$main_loc,"/",file_loc$global_radiation),
#                      pattern = "QQ_SOUID*",
#                      full.names = TRUE)
qq_files<-list.files("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/SunCloud/qq/",
                     pattern = "QQ_SOUID*",
                     full.names = TRUE)
qq_data    <- lapply(qq_files,read_qq)
qq_data    <- lapply(qq_data,function(x){ x[which(x$QQ==-9999),]<-NA; x[complete.cases(x),]})
# qq_data    <- lapply(qq_data,function(x){ x[which(x$QQ<0),]<-0})

qq_monthly <- lapply(qq_data,allsky_monthly_qq)
qq_monthly <- do.call("rbind",qq_monthly)
# write.table(qq_monthly,
#             file = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly.txt"),
#             col.names = TRUE, row.names = FALSE, sep = ",",quote = FALSE)

#now for the clear sky the same
#Matching the metadata of qq and cc
# stations_qq<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$global_radiation,"/ECA_nonblend_info_qq.txt"))
# stations_cc<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$cloud_cover,"/ECA_nonblend_info_cc.txt"))

stations_qq<-read_ECA_info(fname="/net/pc150400/nobackup/users/dirksen/data/radiation_europe/SunCloud/qq/ECA_nonblend_info_qq.txt")
stations_cc<-read_ECA_info(fname="/net/pc150400/nobackup/users/dirksen/data/radiation_europe/SunCloud/cc/ECA_nonblend_info_cc.txt")

stations_join<-merge(stations_qq,stations_cc,by=c("name","coun_id","lat","lon","elev"))
stations_join$fname_qq<-paste0("QQ_SOUID",stations_join$sou_id.x,".txt")
stations_join$fname_cc<-paste0("CC_SOUID",stations_join$sou_id.y,".txt")
#this leaves 257 joined stations within Europe for qq and cc

#Combine the QQ and CC files with an inner merge
# qq_cc_day <- mapply(read_qq_cc,file_qq=paste0(file_loc$main_loc,file_loc$global_radiation,"/",stations_join$fname_qq),
#                     file_cc = paste0(file_loc$main_loc,file_loc$cloud_cover,"/",stations_join$fname_cc),
#                     SIMPLIFY = FALSE)
qq_cc_day <- mapply(read_qq_cc,file_qq=paste0("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/SunCloud/qq/",stations_join$fname_qq),
                    file_cc = paste0("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/SunCloud/cc/",stations_join$fname_cc),
                    SIMPLIFY = FALSE)

qq_monthly_cs <- lapply(qq_cc_day,clearsky_monthly_qq_cc)
qq_monthly_cs <- do.call("rbind",qq_monthly_cs)
# write.table(qq_monthly_cs,
#             file = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt"),
#             col.names = TRUE, row.names = FALSE, sep = ",",quote = FALSE)

#Cloud fraction monthly
cc_monthly <- lapply(qq_cc_day,allsky_monthly_cc)
cc_monthly <- do.call("rbind",cc_monthly)

# write.table(cc_monthly,
#             file = "/net/pc150400/nobackup/users/dirksen/data/radiation_europe/SunCloud/CC_monthly.txt",
#             col.names = TRUE, row.names = FALSE, sep = ",",quote = FALSE)

qq_monthly <- lapply(qq_cc_day,allsky_monthly_qq)
qq_monthly <- do.call("rbind",qq_monthly)

qq_cc_monthly <- merge(cc_monthly,qq_monthly,by=c("STAID","month_year","season","year"))


#Wageningen has no cloud cover observations
#Koper 16450
#Wageningen Haarweg: 6937
#Wageningen Veenkampen: 8555
plot_qq(qq_monthly[which(qq_monthly$STAID==6937),],station.name = "Haarweg")
# plot_qq(qq_monthly[which(qq_monthly$STAID==8555),],station.name = "Veenkampen")

#De Bilt 162
#Potsdam 54


plot_qq_cc(qq_cc_monthly[which(qq_cc_monthly$STAID==162),],station.name="De Bilt")
plot_qq_cc(qq_cc_monthly[which(qq_cc_monthly$STAID==54),],station.name="Potsdam")

plot_qq_cc<-function(series,station.name){
series<-as.data.table(series)
series$year<-year(series$month_year)
setkey(series)
series_y<-ddply(series, .(year), summarize,  QQy=mean(QQm),CCy=mean(CCm))

#5 year moving average
series_y$QQ5y <- frollmean(series_y$QQy,5)
series_y$CC5y <- frollmean(series_y$CCy,5)
series_y<-series_y[complete.cases(series_y),]

# series_long<-gather(series_y, "var", "val",-year)

p1<-ggplot(series_y,aes(year,QQ5y)) +
  theme_bw()+
  geom_line()+ geom_line(aes(year,QQy),color="grey")+
  ylab("Global radiation [W/m2]")+ggtitle(paste0(station.name)) + xlab("")

p2<-ggplot(series_y,aes(year,CC5y)) +
  theme_bw()+
  geom_line()+ geom_line(aes(year,CCy),color="grey")+
  ylab("Cloud cover fraction [-]") + xlab("")
grid.arrange(p1,p2)

}

plot_qq<-function(series,station.name){
  series<-as.data.table(series)
  series$year<-year(series$month_year)
  setkey(series)
  series_y<-ddply(series, .(year), summarize,  QQy=mean(QQm))

  #5 year moving average
  series_y$QQ5y <- frollmean(series_y$QQy,5)
  series_y<-series_y[complete.cases(series_y),]

  ggplot(series_y,aes(year,QQ5y)) +
    theme_bw()+
    geom_line()+ geom_line(aes(year,QQy),color="grey")+
    ylab("Global radiation [W/m2]")+ggtitle(paste0(station.name)) + xlab("")


}
