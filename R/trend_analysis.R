#'Fit sens slope to the series
#'@description Fits sens slope to a time series within a period. It is adviced to run
#'\link{get_change_point} first to find a proper time period for the fitting (without change points).
#'@param series time-series to be analysed
#'@note See also \url{https://rcompanion.org/handbook/G_10.html}
#'@author Marieke Dirksen
#'@export
fit_sens_slope<-function(series){
  requireNamespace("trend")
  requireNamespace("plyr")

  setkey(series)
  series_y<-ddply(series, .(year), summarize,  QQy=mean(QQm))

  #5 year moving average
  series_y$QQ5y <- frollmean(series_y$QQy,5)
  series_y<-series_y[complete.cases(series_y),]

  series_y$year.date<-as.Date(series_y$year,format="%Y")
  series.ts<-xts(series_y$QQ5y,order.by = series_y$year.date)
  series.ts<-as.ts(series.ts)

  series.trend <- trend::sens.slope(series.ts)
  series.trend.df <- data.frame("STAID"=unique(series$STAID),
                                "slope"=series.trend$estimates,
                                "pval"=series.trend$p.value,
                                "conf5"=series.trend$conf.int[1],
                                "conf95"=series.trend$conf.int[2])
  return(series.trend.df)
}

#'Find the change point in time-series
#'@description Calculates the change points and fits linear trends before and after the change points.
#'For the trend fitting a five year moving average window function was used to exclude short term variability
#'from the change point detection.
#'@param series As calculated by \link{clearsky_monthly_qq_cc} or \link{allsky_monthly_qq}, possibly combined
#'with the gap filled series from \link{fill_time_series}.
#'@param station.name name of the station, used as title for the ggplot
#'@return Returns the fitted trend model and a plot of the time series with trend and change points.
#'@author Marieke Dirksen
#'@export
get_change_point<-function(series,
                           station.name="5 year moving average",
                           brk.max){
  requireNamespace("ggplot2")
  requireNamespace("greenbrown")
  requireNamespace("zoo")
  requireNamespace("grid")
  requireNamespace("xts")

  series$year<-year(series$month_year)
  setkey(series)
  series_y<-ddply(series, .(year), summarize,  QQy=mean(QQm))

  #5 year moving average
  series_y$QQ5y <- frollmean(series_y$QQy,5)
  series_y<-series_y[complete.cases(series_y),]

  ###
  #the code below fits trends and breakpoints in timeseries, input needs to be of class ts

  series_y$year.date<-paste0(series_y$year,"-01-01")
  series_y$year.date<-as.Date(series_y$year.date,format="%Y-%m-%d")
  series.ts<-xts(series_y$QQ5y,order.by = series_y$year.date)
  series.ts<-as.ts(series.ts)
  trnd<-Trend(series.ts,breaks = brk.max)
  brk<-trnd$bp$breakpoints
  ###

#trend analysis
  # change.p<-lanzante.test(series_y$QQ5y)
  # change.bu<-bu.test(series_y$QQ5y)
  # change.br<-br.test(series_y$QQ5y)

  #plot
  # cp.info <- grobTree(textGrob(paste0("p-value=",change.br$p.value), x=0.1,  y=0.95, hjust=0,
  #                              gp=gpar(col="red", fontsize=13, fontface="italic")))
  # annotation_custom(cp.info)+
  # geom_vline(xintercept = series_y$year[change.br$estimate],
  #            linetype="solid",color = "red", size=0.5)+
  if(is.na(brk)){
  p<-ggplot(series_y,aes(year,QQ5y)) +
    theme_bw()+
    geom_line()+ geom_line(aes(year,QQy),color="grey") +
    geom_line(aes(year,trnd$trend),color="green") +
    xlab("Year")+ylab("Global radiation [W/m2]")+ggtitle(station.name)
  }else if(length(brk)>1){
  x<-c(min(series_y$year),series_y$year[brk],series_y$year[brk+1],max(series_y$year))
  y<-c(trnd$trend[1],trnd$trend[brk],trnd$trend[brk+1],tail(trnd$trend,n=1))
  df<-data.table(x,y)
  setkey(df)
  x<-df$x
  y<-df$y

  p<-ggplot(series_y,aes(year,QQ5y)) +
    theme_bw()+
    geom_line()+ geom_line(aes(year,QQy),color="grey") +
    # geom_line(aes(year,trnd$trend),color="green") +
    geom_vline(xintercept = series_y$year[brk],
               linetype="solid",color = "green", size=0.5)+
    xlab("Year")+ylab("Global radiation [W/m2]")+ggtitle(station.name)
      for(i in seq(1,length(x),by=2)){
      p <- p+geom_segment(x = x[i], y = y[i], xend = x[i+1], yend = y[i+1],color="green")
      }

  }else{
    x<-c(min(series_y$year),series_y$year[brk],series_y$year[brk+1],max(series_y$year))
    y<-c(trnd$trend[1],trnd$trend[brk],trnd$trend[brk+1],tail(trnd$trend,n=1))

    p<-ggplot(series_y,aes(year,QQ5y)) +
      theme_bw()+
      geom_line()+ geom_line(aes(year,QQy),color="grey") +
      # geom_line(aes(year,trnd$trend),color="green") +
      geom_vline(xintercept = series_y$year[brk],
                 linetype="solid",color = "green", size=0.5)+
      xlab("Year")+ylab("Global radiation [W/m2]")+ggtitle(station.name)
    for(i in seq(1,length(x),by=2)){
      p <- p+geom_segment(x = x[i], y = y[i], xend = x[i+1], yend = y[i+1],color="green")
    }

}
  return(list("Trend"=trnd,"plot"=p)) #"br.test"=change.br,"bu.test"=change.bu,
}

#'Get spatial coverage
#'@description uses the datasets `data("qq_meta")` and `data("qq_start_stop")` to map the
#'spatial coverage of stations within a given time period and colors indicating the
#'completeness of the series.
#'@param meta file containing the geographical information
#'@param station_info file containing the start, stop and completeness of the series
#'@param t1 start of the time period
#'@param t2 stop of the time period
#'@return return a mapview interactive map and a spdf.
#'@author Marieke Dirksen
#'@export
get_spatial_coverage<-function(meta=qq_meta,
                               stations_info=qq_start_stop,
                               t1=1980,t2=2017){
  qq_sub<-stations_info[which(stations_info$start.year<t1 & stations_info$stop.year>t2),]

  qq_sp<-merge(meta,qq_sub,by="STAID")

  # leaflet(qq_sp) %>% addTiles() %>%
  #   addCircleMarkers(~lon, ~lat, label = ~htmlEscape(name),
  #              labelOptions = labelOptions(noHide = TRUE,textOnly = FALSE),
  #              options = markerOptions(riseOnHover = TRUE))

  coordinates(qq_sp)<-~lon+lat
  crs(qq_sp)<-file_loc$CRS.arg

  m<-mapview(qq_sp,zcol="completeness",at=seq(0,1,0.001))
  return(list("map"=m,"sp"=qq_sp))
}

#linear fit
# qq_fit<-lm(Nor ~ year,data=qq_cc)
#
# #in fit$model all the predicted values are stored
# #pvalues are computated after summary is called and can be extracted like:
# qq_cc$fit       <- qq_fit$fitted.values
# qq_cc$residuals <- qq_fit$residuals
#
# qq_fit<-data.frame("season" = unique(qq_cc$season),
#                    "pvalue" = coef(summary(qq_fit))[2,4],
#                    "slope"  = coef(qq_fit)[2])
# # qq_cc$pvalue     <- coef(summary(qq_fit))[2,4]
# # qq_cc$slope      <- coef(qq_fit)[2]
# return(list("qq_cc"=qq_cc,"qq_fit"=qq_fit))
