library(greenbrown)
library(lubridate)
library(raster)
library(ncdf4)
library(xts)
source("inst/settings.R")
main_dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/NCDF_month_year/"

nc.files<-list.files(main_dir,pattern="\\.nc$")

#a subset with the time vector is needed before any further analysis is performed
# select for example one year and calculate the mean,...and so on
st<-stack(paste0(main_dir,"qq_ens_mean_0.1deg_reg_v20.0e.nc"))
timevec<-as.POSIXct(names(st),format="X%Y.%m.%d")

#subset and calculate means
ind<-year(timevec)
nr.yr<-unique(ind)
sub<-which(ind==nr.yr[20])

st_sub<-st[[sub]]
st_year<-stackApply(st_sub,1,mean) # add a count function with a minimum of n days in the whole year
#or compute monthly averages first with a maximum number of missing days == 20%

# brightening<-which(timevec>as.POSIXct("1993-01-01"))
# st<-st[[brightening]]


#TREND ANALYSIS
#http://greenbrown.r-forge.r-project.org/man/TrendRaster.html
#the functions of the trend package can directely be applied to the rastta
#!crop stack to smaller extent first
#but this takes a long time...

#crop extent by column numbers
# st_sub<-crop(st,extent(st,200,300,400,500))
#not run
# test<-TrendRaster(st_sub,start=c(1993,1,1),freq=365)


