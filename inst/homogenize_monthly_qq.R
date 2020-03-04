library(climatol)
library(data.table)
library(DimBri)
library(tidyr)
library(dplyr)
`%notin%` <- Negate(`%in%`)
main_dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/"

wd0<-setwd(paste0(main_dir,"homogen"))
#
t.start=1960
t.stop=2018

#Data input
stations_qq <- read_ECA_info(paste0(main_dir,"SunCloud/qq/stations_qq.txt"))
series_qq <- read_monthly_QQ(paste0(main_dir,"SunCloud/QQ_monthly.txt"))

stations_ss2qq<- read_ECA_info(paste0(main_dir,"SunCloud/ss2qq/stations.txt"))
series_ss2qq <-prepare_series_ss2qq(main_dir)

#find dublicate between the qq and ss2qq series and keep the qq STAIDs
qqSTAID<-as.integer(unique(series_qq$STAID))
qq2ssSTAID<-as.integer(unique(series_ss2qq$STAID))
newSTAID<-qq2ssSTAID[is.na(match(qq2ssSTAID,qqSTAID))]

series_ss2qq<-series_ss2qq[(series_ss2qq$STAID %in% newSTAID),]
series_comb<-rbind(series_qq,series_ss2qq)

#create a new metadata file from the stations qq and ss2qq files
stations_ss2qq<-stations_ss2qq[(stations_ss2qq$sou_id %in% newSTAID),]
stations_comb<-rbind(stations_qq,stations_ss2qq)

#merge series
stations_comb[grep(" [1-9]",stations_comb$name),]
#make one series of the following pairs:
#QUINTA DE AZEVEDO: 10989 10991
#QUINTA DE CARVALHAIS: 10994 10995 10996
#QUINTA DA LEDA: 10997 10998 10999
#QUINTA DO SAIRRAO: 11000 11001 11002
#QUINTA DO PESO: 11005 11006 11007
#or remove these series from the analysis
# staid_out<-c(10989,10991,10994,10995,10996,10997,10998,10999,11000,11001,11002,11005,11006,11007)
staid_out<-c(3947,3948,3839) #Madrid stations have problems with sunshine duration
stations_comb<-stations_comb[stations_comb$sou_id %notin% staid_out]
series_comb<-series_comb[series_comb$STAID %notin% staid_out]
###################################################################
###################################################################
#This is a test part to subset staids in case the output looks shifted or not ok
#CABAUW 438
#DE BILT 162
#DE KOOY 164
#stations_qq[which(stations_qq$coun_id=="NL"),]$sou_id

#subset of the nl stations
#c(161,162,164,166,168,411,438,442,454,464,478)
# series_subset<-c(161,162,164,166,168,411,438,442,454,464,478)
# series_subset<-c(161,162) #De Bilt and De Kooy
# series_subset<-stations_qq[which(stations_qq$coun_id=="NL"),]$sou_id
# stations_comb<-stations_comb[stations_comb$sou_id %in% series_subset]
# series_comb<-series_comb[series_comb$STAID %in% series_subset]
###################################################################
###################################################################

# prepare_ecad_climatol(t.start=t.start,t.stop=t.stop,stations_qq=stations_qq,series_qq=series_qq)
prepare_ecad_climatol(t.start=t.start,t.stop=t.stop,stations_qq=stations_comb,series_qq=series_comb)
#HOMOGENIZATION PROCEDURE
#climatol
# use: 'homogen('QQ',2000,2018,expl=TRUE)' for exploratory analysis

homogen('QQ-m',t.start,t.stop,
        std = 3, snht1 = 30, snht2 = 25, dz.max=10, wd=c(750,500,250))
#with the monthly break points the daily data can be corrected using metad=TRUE
#prepare daily series QQ
#homogen('QQ',t.start,t.stop,metad=TRUE)

#look at the results
break_points<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_brk.csv"))

#somehow the series with multiple names are duplicated! something wrong with the quotes?
staid<-paste0("sta_id",stations_qq$sou_id)
dahstat("QQ-m",t.start,t.stop,stat='series',cod=staid,last=TRUE) #writes series.csv and flags.csv

hom_flags<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_flags.csv"),header = TRUE) #fist column=date, others are stations
hom_series<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_series.csv"),header=TRUE)

#Visualization of the original and corrected timeseries
# load('QQ_2000-2018.rda')
# sta_id<-"2566"
# sta_id<-"4012"
sta_id<-"162"

time_series<-hom_series[,1]; names(time_series) <- "time"
time_series$time<-as.Date(time_series$time)

original_series<-data.frame(series_comb[which(series_comb$STAID==sta_id),]$month_year,
                            series_comb[which(series_comb$STAID==sta_id),]$QQm
                            )
names(original_series) <- c("time","original")
homogenized_series<-hom_series[,"sta_id162"]; names(homogenized_series) <- "homogenized"

homogenized_series <- data.frame(time_series,homogenized_series)

df<-full_join(original_series,homogenized_series,by="time")
df_long<-tidyr::gather(df,"series","measurement",-time)
df_long$series<-as.factor(df_long$series)

library(ggplot2)
library(plotly)
p<-ggplot(df_long,aes(time,measurement,color=series)) + geom_line()
ggplotly(p,dynamicTicks = TRUE)

