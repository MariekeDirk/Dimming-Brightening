## code to prepare `mon_qq` dataset goes here
mon_qq<-data.table::fread("D:/SunCloud/qq/QQ_monthly.txt") #add the filled data
usethis::use_data("mon_qq")
