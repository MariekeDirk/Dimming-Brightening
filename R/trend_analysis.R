#fitting trends
#sen s slope: advanced trends
#point from dim to bright
#slopes and goodness of fit
#see https://rcompanion.org/handbook/G_10.html
fit_trend<-function(qq_cc=qq_cc_allsky_season){
  qq_fit<-lm(Nor ~ year,data=qq_cc)

  #in fit$model all the predicted values are stored
  #pvalues are computated after summary is called and can be extracted like:
  qq_cc$fit       <- qq_fit$fitted.values
  qq_cc$residuals <- qq_fit$residuals

  qq_fit<-data.frame("season" = unique(qq_cc$season),
                     "pvalue" = coef(summary(qq_fit))[2,4],
                     "slope"  = coef(qq_fit)[2])
  # qq_cc$pvalue     <- coef(summary(qq_fit))[2,4]
  # qq_cc$slope      <- coef(qq_fit)[2]
  return(list("qq_cc"=qq_cc,"qq_fit"=qq_fit))
}
