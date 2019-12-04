#'Correlation with other stations
#'@description calculates the correlation coefficient from the series with gaps to all the other
#'series and computes a buffer with a 500km radius. At least 5 available observations per month for
#'all the data is recommended, otherwise the normalization of the data and correlation coef. can
#'give a wrong impression.
#'@param meta metadata with lat lon from all the stations
#'@param series series with missing data
#'@param full_data_series full data series which will be used to fill the gap
#'@param missing_date missing month from the series
#'@param buffer_dist minimum distance to the filling stations in km. Returning 1 if the full_data_series stations
#'falls within the buffer zone.
#'@param n.months minimum number of months with needed to compute correlation coef.
#'@param n.stations maximum number of reference stations from which the median is calculated
#'@author Marieke Dirksen
#'@export
fill_gap <- function(meta,series,full_data_series,
                              missing_date,buffer_dist=500,
                              n.months=5,n.stations=5){
  `%notin%` <- Negate(`%in%`)
  #length of the reference series
  if(empty(meta) | empty(series) | empty(full_data_series)){
    message("One of the input series or meta data is empty, returning NULL")
    return(NULL)
  }
  series$month <- month(series$month_year)
  series_occurance <- series %>% group_by(STAID,month) %>% tally()
  qq_out <- series_occurance$month[which(series_occurance$n < n.months)]

  #check if the series is long enough
  if(length(qq_out)!=0){
    message("Series is not long enough to fill month(s) ",qq_out, " have less than ",n.months," of observations")
    message("To run anyway choose a lower value of n.months")
    return(NULL)
  }

  #select qq staids which match the metadata and the missing day
  full_data_series <- full_data_series[full_data_series$STAID %in% unique(meta$STAID)]
  fill_STAID<-unique(full_data_series$STAID[which(full_data_series$month_year==missing_date)])
  full_data_series_fill <- full_data_series[full_data_series$STAID %in% fill_STAID]

  #subset the days to the common period to construct a correlation matrix
  full_data_series_fill <- full_data_series_fill[full_data_series_fill$month_year %in% series$month_year,]
  full_data_series_fill$month <- month(full_data_series_fill$month_year)
  full_data_series_fill <- full_data_series_fill[complete.cases(full_data_series_fill),]

  ###Here we can make a subset of stations which have at least n months in common
  full_data_series_occurance <- full_data_series_fill %>% group_by(STAID,month) %>% tally()
  qq_out <- unique(full_data_series_occurance$STAID[which(full_data_series_occurance$n < n.months)])
  full_data_series_fill <- full_data_series_fill[full_data_series_fill$STAID %notin% qq_out]

  ###Check if there are enough reference series available
  if(length(unique(full_data_series_fill$STAID))<5){
    message(paste0("Series is not long enough reference series (<5) which meet n.months=",n.months))
    return(NULL)
  }
  ###

  full_data_series_trend <- aggregate(QQm ~ month+STAID, full_data_series_fill, mean)
  names(full_data_series_trend) <-c("month","STAID","trend")

  full_data_series_dt <- merge(full_data_series_fill,full_data_series_trend,by=c("STAID","month"))
  full_data_series_dt$norm <- full_data_series_dt$QQm/full_data_series_dt$trend

  #normalize series
  series_dt <- aggregate(QQm ~ month+STAID, series, mean)
  names(series_dt) <-c("month","STAID","trend")
  series_dt <- merge(series,series_dt,by=c("STAID","month"))
  series_dt$norm <- series_dt$QQm/series_dt$trend

  full_data_series_dt<-rbind(series_dt,full_data_series_dt)


  qq_matrix<-dcast(full_data_series_dt,
                   month_year~STAID,
                   value.var="norm",fun.aggregate = mean)

  qq_matrix<-data.frame(qq_matrix)
  qq_matrix<-subset(qq_matrix,select=c(-1))
  I<-which(colnames(qq_matrix)==paste0("X",unique(series_dt$STAID)))
  matrix.corr <- cor(qq_matrix[,I],qq_matrix[,-I],use="pairwise.complete.obs")

  df.names <- gsub("X","",colnames(matrix.corr))
  df.corr <- data.table(t(matrix.corr))
  df.corr$STAID <- as.integer(df.names)
  names(df.corr)<-c("correlation_coef","STAID")
  setkey(df.corr,cols="correlation_coef")

  # df.corr <- rbind(df.corr,data.frame("correlation_coef"=1,"STAID"=unique(series$STAID)))

  p1 <- data.frame("correlation_coef"=1,"STAID"=unique(series$STAID))
  p1 <- merge(p1,meta,by="STAID")
  coordinates(p1)<-~lon+lat
  proj4string(p1)<-CRS("+init=epsg:4326")

  p1.RD <- spTransform(p1,CRS("+init=epsg:28992"))#use a meter projection for the buffer
  p1.b <- buffer(p1.RD,width=buffer_dist*1000)
  p1.b <- spTransform(p1.b,CRS("+init=epsg:4326"))#set back to the original crs

  #####Spatial subset within 500km range
  df.corr <- merge(df.corr,meta,by="STAID") #correlations with name,coun_id,elev and lat lon
  sp.corr <- df.corr
  sp.corr$lat<-as.numeric(sp.corr$lat)
  sp.corr$lon<-as.numeric(sp.corr$lon)
  coordinates(sp.corr) <- ~lon+lat
  proj4string(sp.corr)<-CRS("+init=epsg:4326")

  # mapview(p1.b,color="red")+sp.corr

  sp.corr$buffer_range <- over(sp.corr,p1.b)

  df.correlation <- as.data.table(sp.corr)
  df.correlation <- df.correlation[which(sp.corr$buffer_range==1),]
  setkey(df.correlation,value="correlation_coef")
  df.correlation <- tail(df.correlation,n=n.stations)

  qq_all_fill <-full_data_series[which(full_data_series$month_year==missing_date &
                                   full_data_series$STAID %in% df.correlation$STAID)]
  qq_all_fill <- merge(df.correlation,qq_all_fill,by="STAID")
  n_val<-length(qq_all_fill$QQm)
  qq_fill <- median(qq_all_fill$QQm,na.rm=TRUE)

  return(list("df"=data.frame(sp.corr),
              "corr"=df.correlation,
              "fill_value"=qq_fill,
              "sd"=sd(qq_all_fill$QQm),
              "nval"=n_val,
              "values"=qq_all_fill))
}

#'Fill gaps in time series
#'@description Fill gaps in time series using the function \link{fill_gap}. The monthly
#'series which have the id column STAID are one by one selected, checked for gaps and
#'if possible filled. The filled values are written to a single file which is appended.
#'Additionally to the filling value the number of stations from which the median is calculated
#'and their standard deviation is included.
#'@param staids unique station ids corresponding with STAID in qq_monthly
#'@param qq_meta metadata from the stations are read by \link{read_ECAD_info}
#'@param qq_monthly monthly averaged file as calculated by either \link{allsky_monthly_qq} or \link{clearsky_monthly_qq_cc}
#'@param fname file name to which the filled gaps should be written
#'@author Marieke Dirksen
#'@export
fill_time_series<-function(staids,qq_meta,qq_monthly,fname){

  for(i in 1:length(staids)){
    message(paste0("Filling gaps for the ",i,"th staid: ",staids[i]," from the total of ",length(staids)))
    qq_series<-qq_monthly[which(qq_monthly$STAID==staids[i]),]

    if(empty(qq_series)){
      message("Series is empty")
    } else{
      measurement_period<-seq(from=min(qq_series$month_year),
                              to=max(qq_series$month_year),by="month")

      if(length(measurement_period)==length(qq_series$month_year)){
        message("no gaps in this series")

      } else{

        missing_dates <- subset(measurement_period, !(measurement_period %in% qq_series$month_year))

        for(j in 1:length(missing_dates)){
          m.d<-missing_dates[j]

          df.out<-fill_gap(meta=qq_meta,
                           series=qq_series,
                           full_data_series = qq_monthly,
                           missing_date = m.d)
          if(!is.null(df.out)){
            fill.row<-data.frame("month_year"=m.d,"QQm"=df.out$fill_value,"STAID"=unique(qq_series$STAID),
                                 "sd"=df.out$sd,"nval"=df.out$nval)

            write.table(fill.row,file=fname,
                        row.names = FALSE,col.names=!file.exists(fname),sep=",",
                        quote = FALSE,append = TRUE)
          }
        }
      }
    }
  }
  return(TRUE)
}

#'Completeness of a time series
#'@param series time series with STAID,month_year dates
#'@param start start of the time series or period
#'@param stop stop of the time series or period
#'@description Calculates the fraction of measurements within the whole period of a series or other period
#'@author Marieke Dirksen
#'@export
completeness.series<-function(series,start=min(series$month_year),stop=max(series$month_year)){

  if(empty(series)){
    message("Series is empty")
    return(NULL)
  }
  measurement_period<-seq(from=start,
                          to=stop,by="month")
  series<-series[complete.cases(series),]
  completeness<-length(series$month_year)/length(measurement_period)
  df<-data.frame("STAID"=unique(series$STAID),"completeness"=completeness,
                 "start"=start,"stop"=stop)
  return(df)
}
