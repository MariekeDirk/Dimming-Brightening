library(data.table)
library(DimBri)
library(lubridate)
library(plyr)
library(seas)
source("inst/settings.R")

############################Gap Filling
#write away all the monthly values with sta_id before gap filling
qq_files<-list.files(paste0(file_loc$main_loc,"/",file_loc$global_radiation),
                     pattern = "QQ_SOUID*",
                     full.names = TRUE)
qq_data    <- lapply(qq_files,read_qq)
qq_data    <- lapply(qq_data,function(x){ x[which(x$QQ==-9999),]<-NA; x[complete.cases(x),]})
# qq_data    <- lapply(qq_data,function(x){ x[which(x$QQ<0),]<-0})

qq_monthly <- lapply(qq_data,allsky_monthly_qq)
qq_monthly <- do.call("rbind",qq_monthly)
write.table(qq_monthly,
            file = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly.txt"),
            col.names = TRUE, row.names = FALSE, sep = ",",quote = FALSE)
#now for the clear sky the same
#Matching the metadata of qq and cc
stations_qq<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$global_radiation,"/ECA_nonblend_info_qq.txt"))
stations_cc<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$cloud_cover,"/ECA_nonblend_info_cc.txt"))

stations_join<-merge(stations_qq,stations_cc,by=c("name","coun_id","lat","lon","elev"))
stations_join$fname_qq<-paste0("QQ_SOUID",stations_join$sou_id.x,".txt")
stations_join$fname_cc<-paste0("CC_SOUID",stations_join$sou_id.y,".txt")
#this leaves 257 joined stations within Europe for qq and cc

#Combine the QQ and CC files with an inner merge
# qq_cc_day <- read_qq_cc(file_qq = paste0(file_loc$main_loc,file_loc$global_radiation,"/",stations_join$fname_qq[1]),
# file_cc = paste0(file_loc$main_loc,file_loc$cloud_cover,"/",stations_join$fname_cc[1]))

qq_cc_day <- mapply(read_qq_cc,file_qq=paste0(file_loc$main_loc,file_loc$global_radiation,"/",stations_join$fname_qq),
                    file_cc = paste0(file_loc$main_loc,file_loc$cloud_cover,"/",stations_join$fname_cc),
                    SIMPLIFY = FALSE)

qq_monthly_cs <- lapply(qq_cc_day,clearsky_monthly_qq_cc)
qq_monthly_cs <- do.call("rbind",qq_monthly_cs)
write.table(qq_monthly_cs,
            file = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt"),
            col.names = TRUE, row.names = FALSE, sep = ",",quote = FALSE)
