library(data.table)
library(DimBri)
library(ggplot2)
library(gridExtra)
library(plyr)
#read both the qq and cc data

mon_cc<-fread("D:/SunCloud/CC_monthly.txt")
mon_qq<-fread("D:/SunCloud/qq/QQ_monthly.txt") #add the filled data


#merge datasets
mon_cc_qq<-merge(mon_cc,mon_qq,by=c("month_year","year","season","STAID"))

#find long records
#Koper 16450
#Wageningen Haarweg: 6937
#Wageningen Veenkampen: 8555
#De Bilt 162
#Potsdam 54
plot_qq_cc<-function(series,station.name,season="summer"){

  if(season=="summer"){
  message("subsetting summer")
  series<-series[which(series$season=="JJA"),]
  }
  if(season=="autumn"){
  message("subsetting autumn")
  series<-series[which(series$season=="SON"),]
  }
  if(season=="winter"){
  message("subsetting winter")
  series<-series[which(series$season=="DJF"),]
  }
  if(season=="spring"){
  message("subsetting spring")
  series<-series[which(series$season=="MAM"),]
  }

  series<-as.data.table(series)
  series$year<-year(series$month_year)
  setkey(series)

  #only calculate rolling mean if all months are there
  series_y<-ddply(series, .(year), summarize,  QQy=mean(QQm),CCy=mean(CCm))

  #5 year moving average -->only if all years are there
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

#subplot
#(A) qq moving average
#(B) cc moving average
save_loc <- "C:/Users/marie/OneDrive/Afbeeldingen/SunCloud/"

plot_data <- mon_cc_qq[which(mon_cc_qq$STAID==54),]
p <- plot_qq_cc(plot_data,station.name = "Potsdam")
ggsave(p,filename=paste0(save_loc,"potsdam_summer.png"))
p <- plot_qq_cc(plot_data,station.name = "Potsdam",season = "autumn")

plot_bilt <- mon_cc_qq[which(mon_cc_qq$STAID==162),]
p2 <- plot_qq_cc(plot_bilt,station.name = "De Bilt")
ggsave(p2,filename=paste0(save_loc,"deBilt_summer.png"))

#some other station
plot_stn <- mon_cc_qq[which(mon_cc_qq$STAID==665),]
plot_qq_cc(plot_koper,station.name = "stn")

