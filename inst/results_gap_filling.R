
library(data.table)
library(htmltools)
library(greenbrown)
library(mapview)
library(plyr)
library(xts)
data("qq_meta")
data("qq_start_stop")



#What is the completeness of stations with a start around 1965 and stop around 2018?
long_records<-get_spatial_coverage(t1=1980)

df.long_records<-data.frame(long_records$sp)
df.long_records$name<-gsub("/","-",df.long_records$name)
#Trend analysis
#Series from de Bilt
#De Bilt 162
#Potsdam 54
#Stockholm 6930
setkey(series_fill,"month_year")
setkey(series_cs_fill,"month_year")

#to do: concider the same time period for all the series with as start t1
#to do: the number of breaks which can be detected breaks
#change in the function get_change_point to a year number:
#h.years=10, length.series=65, breaks=floor(65/10)
for(i in 1:length(df.long_records$STAID)){
nr=df.long_records$STAID[i]
nm=df.long_records$name[i]
cn=df.long_records$coun_id[i]
lng=df.long_records$measurement.years[i]
# qq_m_c_f[which(qq_m_c_f$STAID==nr),]
# qq_monthly_com_f[which(qq_monthly_com_f$STAID==nr),]
# timeseries_cs<-series_cs_fill[which(series_cs_fill$STAID==nr),]
timeseries_allsky<-series_fill[which(series_fill$STAID==nr),]
trend.allsky<-get_change_point(timeseries_allsky,station.name = paste0("coun_id= ",cn,", name= ",nm),brk.max = floor(lng/15))

trend.allsky$plot
ggsave(filename=paste0("C:/Users/marie/Pictures/",nm,".png"))
# trend.cs<-get_change_point(timeseries_cs)
}


