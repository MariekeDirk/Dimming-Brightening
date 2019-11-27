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
  stations$lat = measurements::conv_unit(stations$lat, from = 'deg_min_sec', to = 'dec_deg')
  stations$lon = measurements::conv_unit(stations$lon, from = 'deg_min_sec', to = 'dec_deg')
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