#'Calculate monthly all sky
#'@description Needs only qq as input. A minimum of 80\% of the monthly values should be present
#'to calculate monthly averages.
#'@param qq daily global radiation observations
#'@param frac minimum fraction of daily observations in the month
#'@return returns a dataframe with monthly values and additionally the season and year.
#'@author Marieke Dirksen
#'@export
allsky_monthly_qq <- function(qq=qq_cc_day,frac=0.8){
  if(length(qq$QQ)==0){return(NULL)}
  qq$month_year<-format(qq$DATE,"%Y-%m")
  qq$nr_days<-days_in_month(qq$DATE)
  qq_month<-ddply(qq,~month_year,summarise,QQm=mean(QQ),frac=length(QQ)/unique(nr_days))
  qq_month<-qq_month[which(qq_month$frac>frac),]

  #set the dates and seasons back into the df
  if(length(qq_month$QQm>0)){
    qq_month$month_year<-as.Date(paste0(qq_month$month_year,"-01"),format="%Y-%m-%d")
    qq_month$season <- mkseas(x = qq_month$month_year, width = "DJF")
    qq_month$year <- year(qq_month$month_year)
    qq_month$STAID <- unique(qq$STAID)
    return(qq_month)
  } else{return(NULL)}
}

#'Calculate monthly clear sky
#'@description Needs both qq and cc as input. Clear sky monthly values are calculated if a
#'minumum of two daily observations in the month with 0 or 1 okta were measured.
#'@param qq_cc_day daily global radiation and cloud cover observations
#'@param min_obs minimum number of observations required to calculate monthly clear sky
#'@return returns a dataframe with monthly values and additionally the season and year.
#'@author Marieke Dirksen
#'@export
clearsky_monthly_qq_cc <- function(qq_cc=qq_cc_day,min_obs=2){
  qq_cc <- qq_cc[which(qq_cc$CC==0 | qq_cc$CC==1),]
  if(length(qq_cc$QQ)==0){return(NULL)}
  qq_cc$month_year<-format(qq_cc$DATE,"%Y-%m")
  qq_cc_month<-ddply(qq_cc,~month_year,summarise,QQm=mean(QQ),frac=length(QQ)/min_obs)
  qq_cc_month<-qq_cc_month[which(qq_cc_month$frac>=1),]

  #set the dates and seasons back into the df
  if(length(qq_cc_month$QQm)>0){
    qq_cc_month$month_year<-as.Date(paste0(qq_cc_month$month_year,"-01"),format="%Y-%m-%d")
    qq_cc_month$season <- mkseas(x = qq_cc_month$month_year, width = "DJF")
    qq_cc_month$year <- year(qq_cc_month$month_year)
    qq_cc_month$STAID <- unique(qq_cc$STAID)
    return(qq_cc_month)
  } else{return(NULL)}
}

#'Seasonal trends
#'@description Calculates seasonal trends from monthly averaged values calculated from
#'\link{clearsky_monthly_qq_cc} and \link{allsky_monthly_qq}.
#'@param qq_month monthly global radiation observations
#'@return returns a dataframe with seasonal and normalized seasonal values
#'@author Marieke Dirksen
#'@export
seasonal_trend <- function(qq_month){
  qq_cc_season<-ddply(qq_cc_month,~season+year,summarise,QQs=mean(QQm),frac=length(QQm)/3)
  qq_cc_season<-qq_cc_season[which(qq_cc_season$frac==1),]
  qq_cc_season <- qq_cc_season %>% group_by(season) %>% mutate(Nor = scale(QQs))
  return(qq_cc_season)
}
