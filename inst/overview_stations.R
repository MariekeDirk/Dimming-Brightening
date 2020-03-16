library(mapview)
main_dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/"

stations_qq <- read_ECA_info(paste0(main_dir,"SunCloud/qq/stations_qq.txt"))
stations_ss2qq<- read_ECA_info(paste0(main_dir,"SunCloud/ss2qq/stations.txt"))

#all stations in the database
leaflet(stations_qq) %>% addTiles() %>% addCircleMarkers(lng=~lon,lat=~lat)

#make a figure with the homogenized dataset alone
leaflet(stations_ss2qq) %>% addTiles() %>% addCircleMarkers(lng=~lon,lat=~lat)
