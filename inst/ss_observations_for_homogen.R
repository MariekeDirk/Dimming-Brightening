library(data.table)
library(mapview)

obs<-fread("D:/SunCloud/long_ss_series.txt")
names(obs)<-c("V1","STAID","lon","lat","COUNID","start","stop","V8")
obs<-obs[which(obs$start<as.Date("1960-01-01") &
                 obs$stop>as.Date("2014-01-01")),]
sp.obs<-obs
coordinates(sp.obs)<-~lon+lat
crs(sp.obs)<-CRS("+init=epsg:4326")
mapview(sp.obs)+t1970$map


#STAIDS in for SS
from_cz<-obs[which(obs$COUNID=="cz" & obs$start<as.Date("1960-01-01")),]

#COUNID in
coun_id_in<-c("is","no","ie","gb","sk","hr","si","rs","ba","bg")
