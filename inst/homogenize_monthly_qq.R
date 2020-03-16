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
series_ss2qq <-prepare_series_ss2qq(main_path=main_dir)

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
staid_out<-c(3947,3948,3839,16452) #Madrid stations have problems with sunshine duration
#16452 is for SARAJEVO which is named twice (I think one time for qq and one time for ss),
#leaf 1 in because climatol can only handle unique names

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
#HOMOGENIZATION PROCEDURE
#climatol
# use: 'homogen('QQ',2000,2018,expl=TRUE)' for exploratory analysis
# Monthly homogenization
prepare_ecad_climatol(t.start=t.start,t.stop=t.stop,stations_qq=stations_comb,series_qq=series_comb)

homogen('QQ-m',t.start,t.stop,
        std = 3, snht1 = 30, snht2 = 25, dz.max=8)#, wd=c(750,500,250))
#####################################################################
#####################################################################
#with the monthly break points the daily data can be corrected using metad=TRUE

#Run climatol for daily data
subset_time<-function(series){
  series$QQ[which(series$QQ==-9999)]<-NA
  series[complete.cases(series),]
  series$year<-year(series$DATE)
  series$month<-month(series$DATE)
  series$mday<-mday(series$DATE)
  t.start<-min(which(series$month==1 & series$mday==1))
  t.stop<-max(which(series$month==12 & series$mday==31))
  if(t.start==Inf | t.stop==-Inf){return(NULL)}
  return(series[t.start:t.stop])
}

#Daily QQ
qq_files <- list.files(paste0(main_dir,"SunCloud/qq/"),pattern="QQ_SOUID*",full.names=TRUE)
qq_daily <- lapply(qq_files, read_qq)
qq_daily <- lapply(qq_daily,subset_time)

#The correction of these series with the metad takes a very long time!
qq_daily <- do.call("rbind",qq_daily)
prepare_ecad_climatol_daily(t.start=t.start,t.stop=t.stop,stations_qq = stations_qq,series_qq = qq_daily)
homogen('QQ',t.start,t.stop,metad=TRUE)
#####################################################################
#####################################################################

#look at the results
staid<-paste0("sta_id",stations_qq$sou_id)

break_points<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_brk.csv"))
break_points$Date<-as.Date(break_points$Date)
dahstat("QQ-m",t.start,t.stop,stat='series',last=TRUE) #writes series.csv and flags.csv #cod=staid,
hom_flags<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_flags.csv"),header = TRUE) #fist column=date, others are stations
hom_series<-fread(paste0("QQ-m_",t.start,"-",t.stop,"_series.csv"),header=TRUE) #save the homogenized series to a file

#change format hom_series and gather
ghom_series<-gather(hom_series,key="sta_id",value="QQm",-Date)
ghom_series$sta_id<-gsub("sta_id","",ghom_series$sta_id)

#subset the QQ stations
id_in<-unique(ghom_series$sta_id)
# id_in<-id_in[match(id_in,unique(series_qq$STAID))]
# ghom_series<-ghom_series[ghom_series$sta_id %in% id_in,]
# length(unique(ghom_series$sta_id))
#

#Visualization of the original and corrected timeseries
library(ggplot2) #standard plotting
library(plotly)  #interactive interface around ggplot2

unique(ghom_series$sta_id)
plot_hom_series(id="13")
plot_hom_series(id="16420")



