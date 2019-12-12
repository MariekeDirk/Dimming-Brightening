library(climatol)
library(data.table)
library(DimBri)
library(tidyr)
library(dplyr)
main_dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/"

wd0<-setwd(paste0(main_dir,"homogen"))
#
t.start=1960
t.stop=2018

#Data input
stations_qq <- read_ECA_info(paste0(main_dir,"SunCloud/qq/stations_qq.txt"))
series_qq <- read_monthly_QQ(paste0(main_dir,"SunCloud/QQ_monthly.txt"))

prepare_ecad_climatol(t.start=t.start,t.stop=t.stop,stations_qq=stations_qq,series_qq=series_qq)
#HOMOGENIZATION PROCEDURE
#climatol
# use: 'homogen('QQ',2000,2018,expl=TRUE)' for exploratory analysis

#from the pdf the following values are found
# homogen('QQ-m',t.start,t.stop,std = 3, snht1 = 25)#,wd=c(750,500,250)
homogen('QQ-m',t.start,t.stop,expl = TRUE)
homogen('QQ-m',t.start,t.stop,std = 3, snht1 = 25,wd=c(750,500,250))
#homogenizing different sub-areas has the disadvantage of data voides
# homogsplit('QQ-m',t.start,t.stop,std=3,snht1=25,
#            wd=c(750,500,250), #weights for the 3 different stages of the homogenization the default excludes too much qq data
#            xc=c(-35,5.7,40),yc=c(22,47.7,60,80), #deviding lines in lat lon
#            xo=2,yo=1) #the overlapping areas in degrees, also set larger. xo>yo because variations with longitude are generally smaller

#look at the results
break_points<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_brk.csv"))

dahstat("QQ",t.start,t.stop,stat='series') #writes series.csv and flags.csv
hom_flags<-fread(paste0("QQ_",t.start,"-",t.stop,"_flags.csv"),header = TRUE) #fist column=date, others are stations
hom_series<-fread(paste0("QQ_",t.start,"-",t.stop,"_series.csv"),header=TRUE)

#Visualization of the original and corrected timeseries
# load('QQ_2000-2018.rda')
sta_id<-"6930" #Stockholm

time_series<-hom_series[,1]; names(time_series) <- "time"
original_series<-series_qq_wide[,"6930"]; names(original_series) <- "original"
homogenized_series<-hom_series[,"6930"]; names(homogenized_series) <- "homogenized"

df <- data.frame(time_series,original_series,homogenized_series)
df_long<-gather(df,"series","measurement",-time)
df_long$series<-as.factor(df_long$series)

library(ggplot2)
ggplot(df_long,aes(time,measurement,color=series)) +geom_point()


