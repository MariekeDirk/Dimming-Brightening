## code to prepare `qq_meta` dataset goes here
library(DimBri)
library(raster)
source("inst/settings.R")

qq_meta<-read_ECA_info(fname = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/stations_qq.txt"))
# qq_meta<-read_ECA_info(fname=paste0(file_loc$main_loc,file_loc$global_radiation,"/ECA_nonblend_info_qq.txt"))
names(qq_meta)<-c("STAID","name","coun_id","lat","lon","elev")
qq_meta$lat<-as.numeric(qq_meta$lat)
qq_meta$lon<-as.numeric(qq_meta$lon)
qq_meta$STAID<-as.character(qq_meta$STAID)

usethis::use_data("qq_meta")
