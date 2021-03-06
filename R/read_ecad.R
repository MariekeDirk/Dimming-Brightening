#'Read metadata info from ECAD
#'@description reads the metadata files from ECAD with a selection of colomns
#'@param fname file name of the metadata
#'@return returns a dataframe with lat,lon converted to decimal degrees
#'@author Marieke Dirksen
#'@export
read_ECA_info<-function(fname){
  stations<-fread(fname,
                  skip=16,select = c(1,2,3,4,5,6))
  names(stations)<-c("sou_id","name","coun_id","lat","lon","elev")

  stations$lat<-as.character(gsub(":"," ",stations$lat))
  stations$lon<-as.character(gsub(":"," ",stations$lon))
  stations$lat = as.numeric(measurements::conv_unit(stations$lat, from = 'deg_min_sec', to = 'dec_deg'))
  stations$lon = as.numeric(measurements::conv_unit(stations$lon, from = 'deg_min_sec', to = 'dec_deg'))
  stations$sou_id<-as.integer(stations$sou_id)
  return(stations)
}

#'Read and combine qq and cc files
#'@description reads qq and cc and merges both files with matching STAID and DATE, used for
#'\link{clearsky_monthly_qq_cc}.
#'@param file_qq global radiation file
#'@param file_cc cloud cover file, matching with file_qq
#'@return a combined dataframe with dates converted to date format
#'@author Marieke Dirksen
#'@export
read_qq_cc<-function(file_qq,file_cc){
  qq<-fread(file_qq)
  qq<-qq[which(qq$Q_QQ==0),]
  qq<-subset(qq,select = c("STAID","DATE","QQ"))
  cc<-fread(file_cc)
  cc<-cc[which(cc$Q_CC==0),]
  cc<-subset(cc,select = c("STAID","DATE","CC"))

  qq_cc<-merge(qq,cc,by=c("STAID","DATE"))
  qq_cc$DATE<-as.character(qq_cc$DATE)
  qq_cc$DATE<-as.Date(qq_cc$DATE,format="%Y%m%d")
  return(qq_cc)
}

#'Read qq
#'@description Read the qq file which is used in \link{allsky_monthly_qq}.
#'@param file_qq global radiation file
#'@return returns a dataframe with dates converted into date format
#'@author Marieke Dirksen
#'@export
read_qq<-function(file_qq){
  qq<-fread(file_qq)
  qq<-qq[which(qq$Q_QQ==0),]
  qq<-subset(qq,select = c("STAID","DATE","QQ"))
  qq$DATE<-as.Date(as.character(qq$DATE),format="%Y%m%d")
  return(qq)
}

#'Read monthly qq
#'@description Reads the monthly qq files fields month_year, QQm and STAID calculated by \link{clearsky_monthly_qq_cc},
#'\link{clearsky_monthly_qq_cc} and \link{fill_time_series}.
#'@param monthly_qq name of the file
#'@author Marieke Dirksen
#'@export
read_monthly_QQ<-function(monthly_qq=paste0(file_loc$main_loc,"/",file_loc$global_radiation,"/QQ_monthly_cs.txt")){
  qq_monthly<-fread(monthly_qq)
  qq_monthly<-subset(qq_monthly,select = c("month_year","QQm","STAID"))
  qq_monthly<-qq_monthly[complete.cases(qq_monthly),]
  qq_monthly$month_year<-as.Date(qq_monthly$month_year)
  qq_monthly$STAID<-as.character(qq_monthly$STAID)
  return(qq_monthly)
}

#'Prepare monthly files of global radiation from sunshine duration
#'@description Reads the monthly ss2qq files
#'@param main_path main path where the SunCloud folder with all the data is
#'@author Marieke Dirksen
#'@export
prepare_series_ss2qq<-function(main_path){
  series_ss2qq.list<-list.files(path=paste0(main_path,"SunCloud/ss2qq/"),pattern="indexSS2QQ*",full.names=TRUE)
  series_ss2qq<-mapply(fread,series_ss2qq.list,MoreArgs = list(header=FALSE))
  ncolumns<-lapply(series_ss2qq,ncol)
  ncolumns<-data.frame(do.call(rbind,ncolumns))
  names(ncolumns)<-"n"
  series_in<-which(ncolumns$n==4)
  series_ss2qq<-series_ss2qq[series_in]
  series_ss2qq<-do.call(rbind,series_ss2qq)
  names(series_ss2qq)<-c("STAID","month_year","QQm","qc")
  series_ss2qq$QQm<-series_ss2qq$QQm*10
  series_ss2qq<-series_ss2qq[which(series_ss2qq$qc==0)]
  series_ss2qq$month_year<-substr(series_ss2qq$month_year,1,nchar(series_ss2qq$month_year)-2)
  series_ss2qq$month_year<-paste0(series_ss2qq$month_year,"01")
  series_ss2qq$month_year<-as.Date(series_ss2qq$month_year,format="%Y%m%d")
  series_ss2qq<-subset(series_ss2qq,select = c("month_year","QQm","STAID"))
  return(series_ss2qq)
}
