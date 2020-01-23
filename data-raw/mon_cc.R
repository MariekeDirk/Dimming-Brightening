## code to prepare `mon_cc` dataset goes here
mon_cc<-data.table::fread("D:/SunCloud/CC_monthly.txt")
usethis::use_data("mon_cc")
