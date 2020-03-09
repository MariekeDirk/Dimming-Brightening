library(data.table)
library(DimBri)
data(mon_qq)
data_dir <- "D:/SunCloud/ss2qq/"

deBilt<-fread(paste0(data_dir,"indexSS2QQ000162.txt"))
names(deBilt)<-c("STAID","month_year","SS2QQ","qc")
# deBilt$SS2QQ[which(deBilt$SS2QQ==-9999)]<-NA
deBilt$SS2QQ<-deBilt$SS2QQ*10
deBilt$month_year<-substr(deBilt$month_year,1,nchar(deBilt$month_year)-2)
deBilt$month_year<-paste0(deBilt$month_year,"01")
deBilt$month_year<-as.Date(deBilt$month_year,format="%Y%m%d")

deBilt_qq<-mon_qq[which(mon_qq$STAID==162),]
deBilt_qq$month_year<-as.Date(deBilt_qq$month_year)

mb<-merge(deBilt,deBilt_qq,by=c("STAID","month_year"))

library(ggplot2)
ggplot(mb,aes(QQm,SS2QQ))+geom_point()+xlim(0,400)+ylim(0,400)+geom_abline(color="red")
