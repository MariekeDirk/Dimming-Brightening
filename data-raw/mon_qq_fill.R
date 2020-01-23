## code to prepare `mon_qq_fill` dataset goes here
library(DimBri)
source("inst/settings.R")
mon_qq_fill<-read_monthly_QQ(monthly_qq = paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_fill.txt"))

usethis::use_data("mon_qq_fill")
