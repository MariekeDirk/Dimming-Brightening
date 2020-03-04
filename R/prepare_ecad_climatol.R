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
  series_qq$year<-year(series_qq$month_year)

  #to prevent series with less than a year of data count the months:
  cnt_years<-series_qq %>% group_by(STAID) %>% summarize(min=min(year),max=max(year))
  cnt_min<-series_qq[(series_qq$STAID %in% cnt_years$STAID & series_qq$year %in% cnt_years$min),]
  cnt_max<-series_qq[(series_qq$STAID %in% cnt_years$STAID & series_qq$year %in% cnt_years$max),]

  cnt_min<- cnt_min %>% group_by(STAID,year) %>% summarise(nr.months=n())
  out_min<-cnt_min[which(cnt_min$nr.months<12),]

  cnt_max<- cnt_max %>% group_by(STAID,year) %>% summarise(nr.months=n())
  out_max<-cnt_max[which(cnt_max$nr.months<12),]

  series_qq<-series_qq[!(series_qq$STAID %in% out_min$STAID & series_qq$year %in% out_min$year),]
  series_qq<-series_qq[!(series_qq$STAID %in% out_max$STAID & series_qq$year %in% out_max$year),]

  #end count and subset

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
  # stations_qq$name<-gsub(" ","",stations_qq$name)
  # stations_qq$name<-gsub("-","",stations_qq$name)
  # stations_qq$name<-gsub("/","",stations_qq$name)
  # stations_qq$name<-gsub("_","",stations_qq$name)
  # stations_qq$name<-gsub("\\.","",stations_qq$name)
  # stations_qq$name<-gsub("\\(","",stations_qq$name)
  # stations_qq$name<-gsub("\\)","",stations_qq$name)


  series_qq <- subset(combined_series,select = c("month_year","STAID","QQm"))
  #remove duplicated series!

  #Create wide format
  # time_vec<-data.frame(seq(from=t.start,to=t.stop,by="months"));names(time_vec)<-"month_year"

  series_qq_wide <- reshape(series_qq,idvar = "month_year",timevar="STAID",direction = "wide")
  series_qq_wide <- dplyr::select(series_qq_wide,-month_year)
  series_qq_wide<-t(series_qq_wide)


  # series_qq_wide <- reshape(series_qq,idvar = "month_year",timevar="STAID",direction = "wide")
  # series_qq_wide <- dplyr::select(series_qq_wide,-month_year)
  # series_qq_wide<-t(series_qq_wide)

  #if the correlation matrix still looks wierd try to transpose series_qq_wide

  ######### Write output
  # series files
  series.file<-paste0("QQ-m_",t.start,"-",t.stop,".dat")
  #In the manual: write(dat, 'Ttest_1981-2000.dat') #save the data file but is depreciated
  # write(series_qq_wide,series.file) #creates a .rda output file
  write.table(series_qq_wide,series.file,row.names=FALSE,col.names=FALSE,quote = FALSE,sep=" ")

  #write station file
  stations.file<-paste0("QQ-m_",t.start,"-",t.stop,".est")
  write.table(stations_qq,stations.file,
              row.names=FALSE,col.names=FALSE,quote = TRUE)
  # write(stations_qq,stations.file)
  ########################
  rm(stations_qq)
  rm(series_qq)
  return(list("series"=series.file,"stations"=stations.file))
}
