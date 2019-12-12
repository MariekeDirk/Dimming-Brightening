########################Time series and Station data preparation
#Data preparation
#'Prepare ECAD data for homogenization with climatol
#'@description Organizes the station data and time-series for homogenization
#'with the climatol package. The .dat and .est files are saved in the current work directory.
#'@param t.start starting date to be homogenized
#'@param t.stop end date to be homogenized
#'@param stations_qq station data
#'@param series_qq global radiation observations
#'@author Marieke Dirksen
#'@export
prepare_ecad_climatol<-function(t.start,t.stop,stations_qq,series_qq){
  #(1a) ECAD global radiation observations
  series_qq<-series_qq[which(series_qq$month_year > as.Date(paste0(t.start-1,"-12-31")) &
                               series_qq$month_year < as.Date(paste0(t.stop+1,"-01-01"))),]
  series_qq<-series_qq[complete.cases(series_qq),]
  series_qq<-as.data.table(series_qq)
  setkey(series_qq,"STAID")
  series_qq <- subset(series_qq,select = c("month_year","STAID","QQm"))
  series_qq$STAID<-as.character(series_qq$STAID)

  #Prepare station info
  names(stations_qq)<-c("STAID","name","coun_id","lat","lon","elev")
  stations_qq <- subset(stations_qq,select=c("lon","lat","elev","STAID","name"))
  stations_qq <-as.data.table(stations_qq)
  setkey(stations_qq,"STAID")
  stations_qq$STAID<-as.character(stations_qq$STAID)


  #make sure only stations with metadata are in there
  combined_series<-merge(series_qq,stations_qq,by="STAID")
  stations_qq <- subset(combined_series,select=c("lon","lat","elev","STAID","name"))
  stations_qq <- unique(stations_qq)

  stations_qq$STAID<-paste0("sta_id",stations_qq$STAID)
  stations_qq$lon<-as.numeric(stations_qq$lon)
  stations_qq$lat<-as.numeric(stations_qq$lat)


  series_qq <- subset(combined_series,select = c("month_year","STAID","QQm"))
  #Create wide format
  series_qq_wide <- reshape(series_qq,idvar = "month_year",timevar="STAID",direction = "wide")
  series_qq_wide <- dplyr::select(series_qq_wide,-month_year)
  series_qq_wide<-t(series_qq_wide)
  #if the correlation matrix still looks wierd try to transpose series_qq_wide

  dim(series_qq_wide)

  ######### Write output
  # series files
  series.file<-paste0("QQ-m_",t.start,"-",t.stop,".dat")
  write.table(series_qq_wide,series.file,row.names=FALSE,col.names=FALSE)

  #write station file
  stations.file<-paste0("QQ-m_",t.start,"-",t.stop,".est")
  write.table(stations_qq,stations.file,
              row.names=FALSE,col.names=FALSE)
  ########################

  return(list("series"=series.file,"stations"=stations.file))
}
