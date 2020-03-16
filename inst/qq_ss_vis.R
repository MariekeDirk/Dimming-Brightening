library(data.table)
library(DimBri)
library(rgdal)
library(raster)
library(Orcs)

r1 <- raster(matrix(data = rep(1, 100), nrow = 10, ncol = 10)) # raster1
extent(r1) <- c(-25,37,35,71)
projection(r1) <- CRS("+init=epsg:4326")

ss2qq<-fread("D:/SunCloud/ss2qq/stations.txt",skip=15)
names(ss2qq)<-c("STAID","name","country","lat","lon","elev")
ss2qq$lat<-as.character(gsub(":"," ",ss2qq$lat))
ss2qq$lon<-as.character(gsub(":"," ",ss2qq$lon))
ss2qq$lat = as.numeric(measurements::conv_unit(ss2qq$lat, from = 'deg_min_sec', to = 'dec_deg'))
ss2qq$lon = as.numeric(measurements::conv_unit(ss2qq$lon, from = 'deg_min_sec', to = 'dec_deg'))
coordinates(ss2qq)<-~lon+lat
crs(ss2qq)<-CRS("+init=epsg:4326")
ss2qq<-crop(ss2qq,r1)
ss2qq<-as(ss2qq,"SpatialPoints")
# ss2qq<-list("sp.points",ss2qq,cex=0.3,alpha=0.8,col="blue",first=FALSE)

#add the homogenized series here
main_dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/"
hom_series<-fread("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/homogen/QQ-m_1960-2018_series.csv",header=TRUE) #save the homogenized series to a file

#change format hom_series and gather
ghom_series<-gather(hom_series,key="sta_id",value="QQm",-Date)
ghom_series$sta_id<-gsub("sta_id","",ghom_series$sta_id)
id_in<-unique(ghom_series$sta_id)

stations_qq <- read_ECA_info(paste0(main_dir,"SunCloud/qq/stations_qq.txt"))
stations_qq <- stations_qq[stations_qq$sou_id %in% id_in,]

# load(paste0(main_dir,"homogen/QQ-m_1960-2018.rda"))
#

countries<-readOGR(dsn="D:/natural_earth/ne_10m_admin_0_countries",layer="ne_10m_admin_0_countries")
countries_crop<-crop(countries,r1)
countries_crop<-spTransform(countries_crop,crs(ss2qq))
countries_crop<-list("sp.polygons",countries,cex=0.3,col="darkgrey",alpha=0.7,first=FALSE)
# building_sp<-list("sp.polygons",buildings_crop_amersfoort,fill="black",col=NA,cex=0.05,first=FALSE,which=c(4,5,6))

background<-stack("D:/natural_earth/NE1_HR_LC/NE1_HR_LC.tif")
background_crop<-crop(background,r1)
lout <- rgb2spLayout(background_crop,alpha=0.4)

png(filename = "C:/Users/marie/OneDrive/Afbeeldingen/ss_europe.jpg",
     width=2000,
     height=2000,
     res=300)
spplot(ss2qq,
       cex=0.3,alpha=1,col.regions="red",
       auto.key=FALSE,
       sp.layout=list(lout,countries_crop),
       scales=list(draw=TRUE),
       xlim=c(-25,37),
       ylim=c(35,71)
)
dev.off()

# scale = list("SpatialPolygonsRescale", layout.scale.bar(),
#              offset = c(136400,456580), scale = 300, which=1,
#              fill=c("cyan","green"),first=FALSE)
# s_text0 <- list("sp.text", c(136400, 456600 + 40), "0",col="green" ,cex = 1, which = 1,first=FALSE)
# s_text1 <- list("sp.text", c(136400 + 300, 456600 + 40),col="green" , "300m", cex = 1, which = 1,first=FALSE)
#
# scale2 = list("SpatialPolygonsRescale", layout.scale.bar(),
#               offset = c(136400,456580), scale = 300, which=1,
#               fill=c("black","red"),first=FALSE)
# s2_text0 <- list("sp.text", c(136400, 456600 + 40), "0",col="red",cex = 1, which = 1,first=FALSE)
# s2_text1 <- list("sp.text", c(136400 + 300, 456600 + 40),"300m",col="red", cex = 1, which = 1,first=FALSE)
#
#
# scale4 = list("SpatialPolygonsRescale", layout.scale.bar(),
#               offset = c(136400,456580), scale = 300, which=4,
#               fill=c("cyan","green"),first=FALSE)
# s4_text0 <- list("sp.text", c(136400, 456600 + 40), "0",col="cyan",cex = 1, which = 4,first=FALSE)
# s4_text1 <- list("sp.text", c(136400 + 300, 456600 + 40),"300m",col="cyan", cex = 1, which = 4,first=FALSE)
#
