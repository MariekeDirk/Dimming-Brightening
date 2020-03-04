library(data.table)
library(DimBri)
library(ggplot2)
library(gridExtra)
library(plyr)
#read both the qq and cc data
data("qq_meta")
data("mon_cc")
data("mon_qq")
data("mon_cc_qq")

`%notin%` <- Negate(`%in%`)

#find long records
#Koper 16450
#Wageningen Haarweg: 6937
#Wageningen Veenkampen: 8555
#De Bilt 162
#Potsdam 54
plot_qq_cc<-function(series,station.name,season="summer",qq_only=TRUE){

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
  # series$year<-year(series$month_year)
  setkey(series)

  if(qq_only==FALSE){
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
  #only calculate rolling mean if all months are there
  series_y<-ddply(series, .(year), summarize,  QQy=mean(QQm))

  #5 year moving average -->only if all years are there
  series_y$QQ5y <- frollmean(series_y$QQy,5)
  series_y<-series_y[complete.cases(series_y),]

  p1<-ggplot(series_y,aes(year,QQ5y)) +
    theme_bw()+
    geom_line()+ geom_line(aes(year,QQy),color="grey")+
    ylab("Global radiation [W/m2]")+ggtitle(paste0(station.name)) + xlab("")
return(p1)

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

#normalized trends for whole Europe
library(dplyr)
#before looking into these trends an area needs to be defined with similar global radiation
#pattern, otherwise results will be bias by for example high latitude measurements

#define a reference period to normalize the global radiation set
normalized_season_trends<-function(series_qq=mon_qq,
                                   var="QQm",
                                   nr.ref.year=15,
                                   ref.start=as.Date("1980-01-01"),
                                   ref.stop=as.Date("2010-01-01")){

ref_scale_qq <- series_qq[which(series_qq$month_year>ref.start &
                                series_qq$month_year<ref.stop),]
ref_scale_qq$month<-month(ref_scale_qq$month_year)
ref_nr_obs <- ref_scale_qq %>% group_by(STAID,month) %>% summarize(count=n())


series_out<-unique(ref_nr_obs$STAID[which(ref_nr_obs$count<nr.ref.year)])
ref_scale_qq<-ref_scale_qq[ref_scale_qq$STAID %notin% series_out]

ref_scale_qq <- ref_scale_qq %>% group_by(STAID,season) %>% summarize(QQref = mean(.data[[var]]))

#add the reference values to the original dataset
series_qq <- merge(series_qq,ref_scale_qq,by=c("STAID","season"))
series_qq <- as_tibble(series_qq)
series_qq$QQnorm <- (series_qq %>% pull(.data[[var]]))/series_qq$QQref #create a normalized var

qq_scale <- series_qq %>% group_by(season,year) %>%
  summarize("QQm"=mean(QQnorm,na.rm=TRUE),
            "QQsd"=sd(QQnorm,na.rm=TRUE),
            "count"=n()/3) #summarize
return(qq_scale)
}
qq_scale<-normalized_season_trends()
# cc_scale<-normalized_season_trends(series_qq = mon_cc,var="CCm")

#fix plots for cc
plot_qq_cc(qq_scale,season = "summer",station.name = "Europe")
plot_qq_cc(qq_scale,season = "winter",station.name = "Europe")
plot_qq_cc(qq_scale,season = "spring",station.name = "Europe")
plot_qq_cc(qq_scale,season = "autumn",station.name = "Europe")

#some other station
plot_stn <- mon_cc_qq[which(mon_cc_qq$STAID==665),]
plot_qq_cc(plot_koper,station.name = "stn")

