correlation_matrix<-function(series,meta,n.months=12,t1=1990,t2=2017){
  t.seq<-seq(t1,t2)
  `%notin%` <- Negate(`%in%`)
  #length of the reference series
  if(empty(meta) | empty(series)){
    message("One of the input series or meta data is empty, returning NULL")
    return(NULL)
  }
  #check if the series is long enough
  if(length(qq_out)!=0){
    message("Series is not long enough to fill month(s) ",qq_out, " have less than ",n.months," of observations")
    message("To run anyway choose a lower value of n.months")
    return(NULL)
  }

  #select qq staids which match the metadata
  series <- series[complete.cases(series),]
  series <- series[series$STAID %in% unique(meta$STAID)]

  #subset the days to the common period to construct a correlation matrix
  series$month <- month(series$month_year)
  series$year <- year(series$month_year)
  series <- series[series$year %in% t.seq,]

  ###Here we can make a subset of stations which have at least n months in common
  series_occurance <- series %>% group_by(STAID) %>% tally()
  qq_out <- unique(series_occurance$STAID[which(series_occurance$n < (n.months*length(t.seq)*0.8))])
  series <- series[series$STAID %notin% qq_out]

  ###Check if there are enough reference series available
  if(length(unique(series$STAID))<5){
    message(paste0("Series is not long enough reference series (<5) which meet n.months=",n.months))
    return(NULL)
  }
  ###

  series_trend <- aggregate(QQm ~ month+STAID, series, mean)
  names(series_trend) <-c("month","STAID","trend")

  #normalize series
  series_dt <- merge(series,series_trend,by=c("STAID","month"))
  series_dt$norm <- series_dt$QQm/series_dt$trend
  series_dt <- aggregate(QQm ~ month+STAID, series, mean)
  names(series_dt) <-c("month","STAID","trend")
  series_dt <- merge(series,series_dt,by=c("STAID","month"))
  series_dt$norm <- series_dt$QQm/series_dt$trend

  series_sub<-subset(series_dt,select=c("month_year","norm","STAID"))
  setorderv(series_sub,cols="STAID",order=-1)
  # series_sub<-series_sub[series_sub$STAID %in% c(162,54,6930)] #,54,6930
  series_sub<-reshape(series_sub, idvar = "month_year", timevar = "STAID", direction = "wide")
  # series_sub %>% spread(STAID,value)
  # series_sub %>% spread(STAID,value = "norm",-"month_year")

  # setDT(series_sub)[, activityID := rowid(STAID)]
  # series_sub<-dcast(series_sub, ... ~ STAID, value.var=c("norm"))
  series_sub<-subset(series_sub,select=-1)
  matrix.corr<-cor(series_sub,use="pairwise.complete.obs")#as.matrix(correlate(series_sub))
  matrix.corr[,1]<-gsub("norm.","",matrix.corr[,1])
  matrix.corr<-data.table(matrix.corr)
  matrix.corr$rowname<-colnames(matrix.corr)
  df.points<-gather(matrix.corr,key="name","value",-rowname)
  df.points$name<-gsub("norm.","",df.points$name)
  df.points$rowname<-gsub("norm.","",df.points$rowname)
  # qq_matrix<-data.frame(qq_matrix)
  # qq_matrix<-subset(qq_matrix,select=c(-1))
  # matrix.corr <- cor(qq_matrix,use="pairwise.complete.obs") #correlation matrix between all the obs

meta<-meta[meta$STAID %in% unique(series_dt$STAID),]
meta.sp<-meta
coordinates(meta.sp)<-~lon+lat
crs(meta.sp)<-CRS("+init=epsg:4326")
dist<-pointDistance(meta.sp,lonlat = TRUE)
dist<-data.table(dist)
colnames(dist)<-as.character(data.frame(meta.sp)$STAID)
dist$name<-data.frame(meta.sp)$STAID
df.dist<-gather(dist,key="rowname","dist",-name)

df.cordist<-merge(df.points,df.dist,by=c("name","rowname"))
names(df.cordist)<-c("STAID1","STAID2","cor","dist")
df.cordist$cor<-as.numeric(df.cordist$cor)
df.cordist$dist<-df.cordist$dist/1000 #conversion from meters to kilometers
df.cordist<-df.cordist[df.cordist$dist %notin% 0,]
df.cordist<-df.cordist[which(df.cordist$cor>0),]


df.fit<-data.frame(df.cordist$dist,df.cordist$cor)
names(df.fit)<-c("dist","cor")
logEstimate <- lm(cor~log(dist),data=df.fit)
df.logfit<-fortify(logEstimate)
df.logfit$dist<-exp(df.logfit$`log(dist)`)


#no correlation after limit .fitted/log(.fitted)
#see https://physics.stackexchange.com/questions/334837/how-to-determine-correlation-length-when-the-correlation-function-decays-as-a-po?rq=1
df.logfit$epsilon<-df.logfit$.fitted/log(df.logfit$.fitted)
df.logfit<-as.data.table(df.logfit)
setkey(df.logfit,"dist")
df.logfit$dist[which(is.na(df.logfit$epsilon))][1]
#fitted values are for 750km around 0.27

ggplot(df.cordist,aes(dist,cor)) +geom_point() +
  scale_x_continuous(expand=c(0,0),limits = c(0,2600)) +
  scale_y_continuous(expand=c(0,0),limits= c(0,1)) +
  geom_line(data=df.logfit,
            aes(x=dist, y=.fitted),
            color='red',size=1) +
  xlab("Distance [km]")+
  ylab("Correlation")+
  theme_bw()

}
