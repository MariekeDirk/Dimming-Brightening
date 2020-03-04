## code to prepare `mon_cc_qq` dataset goes here
mon_cc<-data.table::fread("D:/SunCloud/CC_monthly.txt")
mon_qq<-data.table::fread("D:/SunCloud/qq/QQ_monthly.txt") #add the filled data
#merge datasets
mon_cc_qq<-merge(mon_cc,mon_qq,by=c("month_year","year","season","STAID"))

usethis::use_data("mon_cc_qq")
