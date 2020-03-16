#'Visualize homogenized time series from Climatol
#'@description plots the homogenized time series with the interactive plotly library.
#'@param id staid of the station
#'@param brks break points from climatol
#'@param h_series homogenized time series
#'@param o_series original series from ECAD
#'@export
plot_hom_series<-function(id,brks=break_points,h_series=hom_series,o_series=series_comb){
  var<-paste0("sta_id",id)

  #break points for this station
  I<-which(brks$Code==var)
  if(length(I)==0){
    message(paste0("No break points in this series, try for example ",
                   tail(gsub("sta_id","",unique(break_points$Code)),n=1)))
    return(FALSE)
  }

  time_series<-h_series[,1]; names(time_series) <- "time"
  time_series$time<-as.Date(time_series$time)

  original_series<-data.frame(o_series[which(o_series$STAID==id),]$month_year,
                              o_series[which(o_series$STAID==id),]$QQm
  )
  names(original_series) <- c("time","original")

  homogenized_series<-h_series[[var]]; names(homogenized_series) <- "homogenized"
  homogenized_series <- data.frame(time_series,homogenized_series)

  df<-full_join(original_series,homogenized_series,by="time")
  df_long<-tidyr::gather(df,"series","measurement",-time)
  df_long$series<-as.factor(df_long$series)

  #plotting routine
  p<- ggplot2::ggplot(df_long,aes(time,measurement,color=series)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_manual(values = c("red", "darkgrey"))

  if(length(I)!=0){
    p <- p + ggplot2::geom_vline(xintercept = break_points$Date[I],colour="red",linetype=2)
  }else{message("no breaks found")}

  plotly::ggplotly(p,dynamicTicks = TRUE)
}
