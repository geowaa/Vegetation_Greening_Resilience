rm(list = ls())
library(terra)
library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(reshape2)
library(ggsci)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##==== prepare data ====##
pixelmlinear2000_2020<-rast("./Influencing factors/pixelmlinear2000_2020.tif")
pixelmlinear2000_2010<-rast("./Influencing factors/pixelmlinear2000_2010.tif")
pixelmlinear2011_2020<-rast("./Influencing factors/pixelmlinear2011_2020.tif")

#load vegetation land for mask purpose
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")
#spatial=T: to obtain a spatial object rather a marrix 
vegetation_land_point = rasterToPoints(vegetation_land,spatial=T) %>% st_as_sf

#Extract raster data to point
Extra_alldata2000_2020<-raster::extract(pixelmlinear2000_2020,vegetation_land_point)
Extra_alldata2000_2010<-raster::extract(pixelmlinear2000_2010,vegetation_land_point)
Extra_alldata2011_2020<-raster::extract(pixelmlinear2011_2020,vegetation_land_point)

#remove 'ID' column
Extra_alldata2000_2020<-subset(Extra_alldata2000_2020, select = -c(ID))
Extra_alldata2000_2010<-subset(Extra_alldata2000_2010, select = -c(ID))
Extra_alldata2011_2020<-subset(Extra_alldata2011_2020, select = -c(ID))

#rename column names
colnames(Extra_alldata2000_2020)<-c("kNDVI_slope","kNDVI_p","kNDVI_rel",
                                    "premean_slope","premean_p","premean_rel",
                                    "precv_slope","precv_p","precv_rel",
                                    "temmean_slope","temmean_p","temmean_rel",
                                    "temcv_slope","temcv_p","temcv_rel",
                                    "aimean_slope","aimean_p","aimean_rel",
                                    "aicv_slope","aicv_p","aicv_rel")
colnames(Extra_alldata2000_2010)<-c("kNDVI_slope","kNDVI_p","kNDVI_rel",
                                    "premean_slope","premean_p","premean_rel",
                                    "precv_slope","precv_p","precv_rel",
                                    "temmean_slope","temmean_p","temmean_rel",
                                    "temcv_slope","temcv_p","temcv_rel",
                                    "aimean_slope","aimean_p","aimean_rel",
                                    "aicv_slope","aicv_p","aicv_rel")
colnames(Extra_alldata2011_2020)<-c("kNDVI_slope","kNDVI_p","kNDVI_rel",
                                    "premean_slope","premean_p","premean_rel",
                                    "precv_slope","precv_p","precv_rel",
                                    "temmean_slope","temmean_p","temmean_rel",
                                    "temcv_slope","temcv_p","temcv_rel",
                                    "aimean_slope","aimean_p","aimean_rel",
                                    "aicv_slope","aicv_p","aicv_rel")
#obtain the x,y
vegetation_land_dataframe <- as(vegetation_land, "SpatialPixelsDataFrame")
vegetation_land_dataframe <- as.data.frame(vegetation_land_dataframe) %>% 
  rename(vegetation_land  = vegetation_land )

#prepare data for 2000 to 2020
kNDVI_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(1,2)])
kNDVI_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(3,2)])
premean_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(4,5)])
premean_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(6,5)])
precv_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(7,8)])
precv_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(9,8)])
temmean_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(10,11)])
temmean_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(12,11)])
temcv_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(13,14)])
temcv_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(15,14)])
aimean_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(16,17)])
aimean_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(18,17)])
aicv_slope_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(19,20)])
aicv_rel_00_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2020[,c(21,20)])
#remove the non-significant grids
kNDVI_slope_00_20<-kNDVI_slope_00_20[-which(kNDVI_slope_00_20$kNDVI_p > 0.05|is.na(kNDVI_slope_00_20$kNDVI_p)),]
kNDVI_rel_00_20<-kNDVI_rel_00_20[-which(kNDVI_rel_00_20$kNDVI_p > 0.05|is.na(kNDVI_rel_00_20$kNDVI_p)),]
premean_slope_00_20<-premean_slope_00_20[-which(premean_slope_00_20$premean_p > 0.05|is.na(premean_slope_00_20$premean_p)),]
premean_rel_00_20<-premean_rel_00_20[-which(premean_rel_00_20$premean_p > 0.05|is.na(premean_rel_00_20$premean_p)),]
precv_slope_00_20<-precv_slope_00_20[-which(precv_slope_00_20$precv_p > 0.05|is.na(precv_slope_00_20$precv_p)),]
precv_rel_00_20<-precv_rel_00_20[-which(precv_rel_00_20$precv_p > 0.05|is.na(precv_rel_00_20$precv_p)),]
temmean_slope_00_20<-temmean_slope_00_20[-which(temmean_slope_00_20$temmean_p > 0.05|is.na(temmean_slope_00_20$temmean_p)),]
temmean_rel_00_20<-temmean_rel_00_20[-which(temmean_rel_00_20$temmean_p > 0.05|is.na(temmean_rel_00_20$temmean_p)),]
temcv_slope_00_20<-temcv_slope_00_20[-which(temcv_slope_00_20$temcv_p > 0.05|is.na(temcv_slope_00_20$temcv_p)),]
temcv_rel_00_20<-temcv_rel_00_20[-which(temcv_rel_00_20$temcv_p > 0.05|is.na(temcv_rel_00_20$temcv_p)),]
aimean_slope_00_20<-aimean_slope_00_20[-which(aimean_slope_00_20$aimean_p > 0.05|is.na(aimean_slope_00_20$aimean_p)),]
aimean_rel_00_20<-aimean_rel_00_20[-which(aimean_rel_00_20$aimean_p > 0.05|is.na(aimean_rel_00_20$aimean_p)),]
aicv_slope_00_20<-aicv_slope_00_20[-which(aicv_slope_00_20$aicv_p > 0.05|is.na(aicv_slope_00_20$aicv_p)),]
aicv_rel_00_20<-aicv_rel_00_20[-which(aicv_rel_00_20$aicv_p > 0.05|is.na(aicv_rel_00_20$aicv_p)),]

#dataframe to raster
kNDVI_slope_00_20<-rasterFromXYZ(kNDVI_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
kNDVI_rel_00_20<-rasterFromXYZ(kNDVI_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
premean_slope_00_20<-rasterFromXYZ(premean_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
premean_rel_00_20<-rasterFromXYZ(premean_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
precv_slope_00_20<-rasterFromXYZ(precv_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
precv_rel_00_20<-rasterFromXYZ(precv_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temmean_slope_00_20<-rasterFromXYZ(temmean_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temmean_rel_00_20<-rasterFromXYZ(temmean_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temcv_slope_00_20<-rasterFromXYZ(temcv_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temcv_rel_00_20<-rasterFromXYZ(temcv_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aimean_slope_00_20<-rasterFromXYZ(aimean_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aimean_rel_00_20<-rasterFromXYZ(aimean_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aicv_slope_00_20<-rasterFromXYZ(aicv_slope_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aicv_rel_00_20<-rasterFromXYZ(aicv_rel_00_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")

#prepare data for 2000 to 2010
kNDVI_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(1,2)])
kNDVI_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(3,2)])
premean_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(4,5)])
premean_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(6,5)])
precv_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(7,8)])
precv_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(9,8)])
temmean_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(10,11)])
temmean_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(12,11)])
temcv_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(13,14)])
temcv_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(15,14)])
aimean_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(16,17)])
aimean_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(18,17)])
aicv_slope_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(19,20)])
aicv_rel_00_10<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2000_2010[,c(21,20)])
#remove the non-significant grids
kNDVI_slope_00_10<-kNDVI_slope_00_10[-which(kNDVI_slope_00_10$kNDVI_p > 0.05|is.na(kNDVI_slope_00_10$kNDVI_p)),]
kNDVI_rel_00_10<-kNDVI_rel_00_10[-which(kNDVI_rel_00_10$kNDVI_p > 0.05|is.na(kNDVI_rel_00_10$kNDVI_p)),]
premean_slope_00_10<-premean_slope_00_10[-which(premean_slope_00_10$premean_p > 0.05|is.na(premean_slope_00_10$premean_p)),]
premean_rel_00_10<-premean_rel_00_10[-which(premean_rel_00_10$premean_p > 0.05|is.na(premean_rel_00_10$premean_p)),]
precv_slope_00_10<-precv_slope_00_10[-which(precv_slope_00_10$precv_p > 0.05|is.na(precv_slope_00_10$precv_p)),]
precv_rel_00_10<-precv_rel_00_10[-which(precv_rel_00_10$precv_p > 0.05|is.na(precv_rel_00_10$precv_p)),]
temmean_slope_00_10<-temmean_slope_00_10[-which(temmean_slope_00_10$temmean_p > 0.05|is.na(temmean_slope_00_10$temmean_p)),]
temmean_rel_00_10<-temmean_rel_00_10[-which(temmean_rel_00_10$temmean_p > 0.05|is.na(temmean_rel_00_10$temmean_p)),]
temcv_slope_00_10<-temcv_slope_00_10[-which(temcv_slope_00_10$temcv_p > 0.05|is.na(temcv_slope_00_10$temcv_p)),]
temcv_rel_00_10<-temcv_rel_00_10[-which(temcv_rel_00_10$temcv_p > 0.05|is.na(temcv_rel_00_10$temcv_p)),]
aimean_slope_00_10<-aimean_slope_00_10[-which(aimean_slope_00_10$aimean_p > 0.05|is.na(aimean_slope_00_10$aimean_p)),]
aimean_slope_00_10<-aimean_slope_00_10[-which(-10>aimean_slope_00_10$aimean_slope|aimean_slope_00_10$aimean_slope>10),] #Removing outliers
aimean_rel_00_10<-aimean_rel_00_10[-which(aimean_rel_00_10$aimean_p > 0.05|is.na(aimean_rel_00_10$aimean_p)),]
aicv_slope_00_10<-aicv_slope_00_10[-which(aicv_slope_00_10$aicv_p > 0.05|is.na(aicv_slope_00_10$aicv_p)),]
aicv_slope_00_10<-aicv_slope_00_10[-which(-10>aicv_slope_00_10$aicv_slope|aicv_slope_00_10$aicv_slope>10),] #Removing outliers
aicv_rel_00_10<-aicv_rel_00_10[-which(aicv_rel_00_10$aicv_p > 0.05|is.na(aicv_rel_00_10$aicv_p)),]

#dataframe to raster
kNDVI_slope_00_10<-rasterFromXYZ(kNDVI_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
kNDVI_rel_00_10<-rasterFromXYZ(kNDVI_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
premean_slope_00_10<-rasterFromXYZ(premean_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
premean_rel_00_10<-rasterFromXYZ(premean_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
precv_slope_00_10<-rasterFromXYZ(precv_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
precv_rel_00_10<-rasterFromXYZ(precv_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temmean_slope_00_10<-rasterFromXYZ(temmean_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temmean_rel_00_10<-rasterFromXYZ(temmean_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temcv_slope_00_10<-rasterFromXYZ(temcv_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temcv_rel_00_10<-rasterFromXYZ(temcv_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aimean_slope_00_10<-rasterFromXYZ(aimean_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aimean_rel_00_10<-rasterFromXYZ(aimean_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aicv_slope_00_10<-rasterFromXYZ(aicv_slope_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aicv_rel_00_10<-rasterFromXYZ(aicv_rel_00_10[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")

#prepare data for 2011 to 2020
kNDVI_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(1,2)])
kNDVI_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(3,2)])
premean_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(4,5)])
premean_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(6,5)])
precv_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(7,8)])
precv_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(9,8)])
temmean_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(10,11)])
temmean_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(12,11)])
temcv_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(13,14)])
temcv_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(15,14)])
aimean_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(16,17)])
aimean_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(18,17)])
aicv_slope_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(19,20)])
aicv_rel_11_20<-cbind(vegetation_land_dataframe[,2:3],Extra_alldata2011_2020[,c(21,20)])
#remove the non-significant grids
kNDVI_slope_11_20<-kNDVI_slope_11_20[-which(kNDVI_slope_11_20$kNDVI_p > 0.05|is.na(kNDVI_slope_11_20$kNDVI_p)),]
kNDVI_rel_11_20<-kNDVI_rel_11_20[-which(kNDVI_rel_11_20$kNDVI_p > 0.05|is.na(kNDVI_rel_11_20$kNDVI_p)),]
premean_slope_11_20<-premean_slope_11_20[-which(premean_slope_11_20$premean_p > 0.05|is.na(premean_slope_11_20$premean_p)),]
premean_rel_11_20<-premean_rel_11_20[-which(premean_rel_11_20$premean_p > 0.05|is.na(premean_rel_11_20$premean_p)),]
precv_slope_11_20<-precv_slope_11_20[-which(precv_slope_11_20$precv_p > 0.05|is.na(precv_slope_11_20$precv_p)),]
precv_rel_11_20<-precv_rel_11_20[-which(precv_rel_11_20$precv_p > 0.05|is.na(precv_rel_11_20$precv_p)),]
temmean_slope_11_20<-temmean_slope_11_20[-which(temmean_slope_11_20$temmean_p > 0.05|is.na(temmean_slope_11_20$temmean_p)),]
temmean_rel_11_20<-temmean_rel_11_20[-which(temmean_rel_11_20$temmean_p > 0.05|is.na(temmean_rel_11_20$temmean_p)),]
temcv_slope_11_20<-temcv_slope_11_20[-which(temcv_slope_11_20$temcv_p > 0.05|is.na(temcv_slope_11_20$temcv_p)),]
temcv_rel_11_20<-temcv_rel_11_20[-which(temcv_rel_11_20$temcv_p > 0.05|is.na(temcv_rel_11_20$temcv_p)),]
aimean_slope_11_20<-aimean_slope_11_20[-which(aimean_slope_11_20$aimean_p > 0.05|is.na(aimean_slope_11_20$aimean_p)),]
aimean_rel_11_20<-aimean_rel_11_20[-which(aimean_rel_11_20$aimean_p > 0.05|is.na(aimean_rel_11_20$aimean_p)),]
aicv_slope_11_20<-aicv_slope_11_20[-which(aicv_slope_11_20$aicv_p > 0.05|is.na(aicv_slope_11_20$aicv_p)),]
aicv_rel_11_20<-aicv_rel_11_20[-which(aicv_rel_11_20$aicv_p > 0.05|is.na(aicv_rel_11_20$aicv_p)),]

#dataframe to raster
kNDVI_slope_11_20<-rasterFromXYZ(kNDVI_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
kNDVI_rel_11_20<-rasterFromXYZ(kNDVI_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
premean_slope_11_20<-rasterFromXYZ(premean_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
premean_rel_11_20<-rasterFromXYZ(premean_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
precv_slope_11_20<-rasterFromXYZ(precv_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
precv_rel_11_20<-rasterFromXYZ(precv_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temmean_slope_11_20<-rasterFromXYZ(temmean_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temmean_rel_11_20<-rasterFromXYZ(temmean_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temcv_slope_11_20<-rasterFromXYZ(temcv_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
temcv_rel_11_20<-rasterFromXYZ(temcv_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aimean_slope_11_20<-rasterFromXYZ(aimean_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aimean_rel_11_20<-rasterFromXYZ(aimean_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aicv_slope_11_20<-rasterFromXYZ(aicv_slope_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")
aicv_rel_11_20<-rasterFromXYZ(aicv_rel_11_20[1:3],crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs")

#Regional statistics via shapefile data
bioclimatic_zonal <- readOGR("./Bioclimatic zonal/modi_bioclimatic_zonal.shp",layer = "modi_bioclimatic_zonal")

kNDVI_slope_00_20<-raster::extract(kNDVI_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
kNDVI_rel_00_20<-raster::extract(kNDVI_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
premean_slope_00_20<-raster::extract(premean_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
premean_rel_00_20<-raster::extract(premean_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
precv_slope_00_20<-raster::extract(precv_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
precv_rel_00_20<-raster::extract(precv_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
temmean_slope_00_20<-raster::extract(temmean_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
temmean_rel_00_20<-raster::extract(temmean_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
temcv_slope_00_20<-raster::extract(temcv_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
temcv_rel_00_20<-raster::extract(temcv_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
aimean_slope_00_20<-raster::extract(aimean_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
aimean_rel_00_20<-raster::extract(aimean_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
aicv_slope_00_20<-raster::extract(aicv_slope_00_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
aicv_rel_00_20<-raster::extract(aicv_rel_00_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)

kNDVI_slope_00_10<-raster::extract(kNDVI_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
kNDVI_rel_00_10<-raster::extract(kNDVI_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
premean_slope_00_10<-raster::extract(premean_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
premean_rel_00_10<-raster::extract(premean_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
precv_slope_00_10<-raster::extract(precv_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
precv_rel_00_10<-raster::extract(precv_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
temmean_slope_00_10<-raster::extract(temmean_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
temmean_rel_00_10<-raster::extract(temmean_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
temcv_slope_00_10<-raster::extract(temcv_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
temcv_rel_00_10<-raster::extract(temcv_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
aimean_slope_00_10<-raster::extract(aimean_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
aimean_rel_00_10<-raster::extract(aimean_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
aicv_slope_00_10<-raster::extract(aicv_slope_00_10, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
aicv_rel_00_10<-raster::extract(aicv_rel_00_10, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)

kNDVI_slope_11_20<-raster::extract(kNDVI_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
kNDVI_rel_11_20<-raster::extract(kNDVI_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
premean_slope_11_20<-raster::extract(premean_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
premean_rel_11_20<-raster::extract(premean_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
precv_slope_11_20<-raster::extract(precv_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
precv_rel_11_20<-raster::extract(precv_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
temmean_slope_11_20<-raster::extract(temmean_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
temmean_rel_11_20<-raster::extract(temmean_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
temcv_slope_11_20<-raster::extract(temcv_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
temcv_rel_11_20<-raster::extract(temcv_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
aimean_slope_11_20<-raster::extract(aimean_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
aimean_rel_11_20<-raster::extract(aimean_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)
aicv_slope_11_20<-raster::extract(aicv_slope_11_20, bioclimatic_zonal,fun=mean,df=TRUE,na.rm=TRUE)
aicv_rel_11_20<-raster::extract(aicv_rel_11_20, bioclimatic_zonal,fun=sum,df=TRUE,na.rm=TRUE)

#relative importance
rel_00_20<-cbind(kNDVI_rel_00_20[,2],premean_rel_00_20[,2],precv_rel_00_20[,2],temmean_rel_00_20[,2],
                 temcv_rel_00_20[,2],aimean_rel_00_20[,2],aicv_rel_00_20[,2])
rel_00_10<-cbind(kNDVI_rel_00_10[,2],premean_rel_00_10[,2],precv_rel_00_10[,2],temmean_rel_00_10[,2],
                 temcv_rel_00_10[,2],aimean_rel_00_10[,2],aicv_rel_00_10[,2])
rel_11_20<-cbind(kNDVI_rel_11_20[,2],premean_rel_11_20[,2],precv_rel_11_20[,2],temmean_rel_11_20[,2],
                 temcv_rel_11_20[,2],aimean_rel_11_20[,2],aicv_rel_11_20[,2])
rel_00_20<-as.data.frame(rel_00_20)
rel_00_10<-as.data.frame(rel_00_10)
rel_11_20<-as.data.frame(rel_11_20)
names(rel_00_20)<-c("kNDVI_rel","premean_rel","precv_rel","temmean_rel","temcv_rel","aimean_rel","aicv_rel")
names(rel_00_10)<-c("kNDVI_rel","premean_rel","precv_rel","temmean_rel","temcv_rel","aimean_rel","aicv_rel")
names(rel_11_20)<-c("kNDVI_rel","premean_rel","precv_rel","temmean_rel","temcv_rel","aimean_rel","aicv_rel")
rel_00_20<-t(rel_00_20)
rel_00_10<-t(rel_00_10)
rel_11_20<-t(rel_11_20)
colnames(rel_00_20)<-c("Forest","Forest_grass","Grass","Desert_grass")
colnames(rel_00_10)<-c("Forest","Forest_grass","Grass","Desert_grass")
colnames(rel_11_20)<-c("Forest","Forest_grass","Grass","Desert_grass")
rel_00_20<-as.data.frame(rel_00_20)
rel_00_10<-as.data.frame(rel_00_10)
rel_11_20<-as.data.frame(rel_11_20)

#normalized to 0-1
Forest_00_20<-(rel_00_20$Forest-min(rel_00_20$Forest))/(max(rel_00_20$Forest)-min(rel_00_20$Forest))
Forest_grass_00_20<-(rel_00_20$Forest_grass-min(rel_00_20$Forest_grass))/(max(rel_00_20$Forest_grass)-min(rel_00_20$Forest_grass))
Grass_00_20<-(rel_00_20$Grass-min(rel_00_20$Grass))/(max(rel_00_20$Grass)-min(rel_00_20$Grass))
Desert_grass_00_20<-(rel_00_20$Desert_grass-min(rel_00_20$Desert_grass))/(max(rel_00_20$Desert_grass)-min(rel_00_20$Desert_grass))
Forest_00_10<-(rel_00_10$Forest-min(rel_00_10$Forest))/(max(rel_00_10$Forest)-min(rel_00_10$Forest))
Forest_grass_00_10<-(rel_00_10$Forest_grass-min(rel_00_10$Forest_grass))/(max(rel_00_10$Forest_grass)-min(rel_00_10$Forest_grass))
Grass_00_10<-(rel_00_10$Grass-min(rel_00_10$Grass))/(max(rel_00_10$Grass)-min(rel_00_10$Grass))
Desert_grass_00_10<-(rel_00_10$Desert_grass-min(rel_00_10$Desert_grass))/(max(rel_00_10$Desert_grass)-min(rel_00_10$Desert_grass))
Forest_11_20<-(rel_11_20$Forest-min(rel_11_20$Forest))/(max(rel_11_20$Forest)-min(rel_11_20$Forest))
Forest_grass_11_20<-(rel_11_20$Forest_grass-min(rel_11_20$Forest_grass))/(max(rel_11_20$Forest_grass)-min(rel_11_20$Forest_grass))
Grass_11_20<-(rel_11_20$Grass-min(rel_11_20$Grass))/(max(rel_11_20$Grass)-min(rel_11_20$Grass))
Desert_grass_11_20<-(rel_11_20$Desert_grass-min(rel_11_20$Desert_grass))/(max(rel_11_20$Desert_grass)-min(rel_11_20$Desert_grass))

rel_00_20<-data.frame(Forest_00_20, Forest_grass_00_20,Grass_00_20,Desert_grass_00_20)
rel_00_10<-data.frame(Forest_00_10, Forest_grass_00_10,Grass_00_10,Desert_grass_00_10)
rel_11_20<-data.frame(Forest_11_20, Forest_grass_11_20,Grass_11_20,Desert_grass_11_20)
colnames(rel_00_20)<-c("Forest","Forest_grass","Grass","Desert_grass")
colnames(rel_00_10)<-c("Forest","Forest_grass","Grass","Desert_grass")
colnames(rel_11_20)<-c("Forest","Forest_grass","Grass","Desert_grass")

rel_00_20$object<-c("kNDVI_rel","premean_rel","precv_rel","temmean_rel","temcv_rel","aimean_rel","aicv_rel")
rel_00_10$object<-c("kNDVI_rel","premean_rel","precv_rel","temmean_rel","temcv_rel","aimean_rel","aicv_rel")
rel_11_20$object<-c("kNDVI_rel","premean_rel","precv_rel","temmean_rel","temcv_rel","aimean_rel","aicv_rel")
rel_00_20<-melt(rel_00_20)
rel_00_10<-melt(rel_00_10)
rel_11_20<-melt(rel_11_20)

rel_00_20$object<-factor(rel_00_20$object,levels=c("aicv_rel","aimean_rel","temcv_rel","temmean_rel","precv_rel","premean_rel","kNDVI_rel"))
rel_00_10$object<-factor(rel_00_10$object,levels=c("aicv_rel","aimean_rel","temcv_rel","temmean_rel","precv_rel","premean_rel","kNDVI_rel"))
rel_11_20$object<-factor(rel_11_20$object,levels=c("aicv_rel","aimean_rel","temcv_rel","temmean_rel","precv_rel","premean_rel","kNDVI_rel"))

#slope
slope_00_20<-cbind(kNDVI_slope_00_20[,2],premean_slope_00_20[,2],precv_slope_00_20[,2],temmean_slope_00_20[,2],
                   temcv_slope_00_20[,2],aimean_slope_00_20[,2],aicv_slope_00_20[,2])
slope_00_10<-cbind(kNDVI_slope_00_10[,2],premean_slope_00_10[,2],precv_slope_00_10[,2],temmean_slope_00_10[,2],
                   temcv_slope_00_10[,2],aimean_slope_00_10[,2],aicv_slope_00_10[,2])
slope_11_20<-cbind(kNDVI_slope_11_20[,2],premean_slope_11_20[,2],precv_slope_11_20[,2],temmean_slope_11_20[,2],
                   temcv_slope_11_20[,2],aimean_slope_11_20[,2],aicv_slope_11_20[,2])
slope_00_20<-as.data.frame(slope_00_20)
slope_00_10<-as.data.frame(slope_00_10)
slope_11_20<-as.data.frame(slope_11_20)
names(slope_00_20)<-c("kNDVI_slope","premean_slope","precv_slope","temmean_slope","temcv_slope","aimean_slope","aicv_slope")
names(slope_00_10)<-c("kNDVI_slope","premean_slope","precv_slope","temmean_slope","temcv_slope","aimean_slope","aicv_slope")
names(slope_11_20)<-c("kNDVI_slope","premean_slope","precv_slope","temmean_slope","temcv_slope","aimean_slope","aicv_slope")
slope_00_20<-t(slope_00_20)
slope_00_10<-t(slope_00_10)
slope_11_20<-t(slope_11_20)
colnames(slope_00_20)<-c("Forest","Forest_grass","Grass","Desert_grass")
colnames(slope_00_10)<-c("Forest","Forest_grass","Grass","Desert_grass")
colnames(slope_11_20)<-c("Forest","Forest_grass","Grass","Desert_grass")
slope_00_20<-as.data.frame(slope_00_20)
slope_00_10<-as.data.frame(slope_00_10)
slope_11_20<-as.data.frame(slope_11_20)

slope_00_20$object<-c("kNDVI_slope","premean_slope","precv_slope","temmean_slope","temcv_slope","aimean_slope","aicv_slope")
slope_00_10$object<-c("kNDVI_slope","premean_slope","precv_slope","temmean_slope","temcv_slope","aimean_slope","aicv_slope")
slope_11_20$object<-c("kNDVI_slope","premean_slope","precv_slope","temmean_slope","temcv_slope","aimean_slope","aicv_slope")
slope_00_20<-melt(slope_00_20)
slope_00_10<-melt(slope_00_10)
slope_11_20<-melt(slope_11_20)
slope_00_20$object<-factor(slope_00_20$object,levels=c("aicv_slope","aimean_slope","temcv_slope","temmean_slope","precv_slope","premean_slope","kNDVI_slope"))
slope_00_10$object<-factor(slope_00_10$object,levels=c("aicv_slope","aimean_slope","temcv_slope","temmean_slope","precv_slope","premean_slope","kNDVI_slope"))
slope_11_20$object<-factor(slope_11_20$object,levels=c("aicv_slope","aimean_slope","temcv_slope","temmean_slope","precv_slope","premean_slope","kNDVI_slope"))

#rel_00_20
##------Draw------##
Fig.7a<-ggplot()+
  geom_tile(data=rel_00_20,
            aes(x=variable,y=object,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "Reds",direction = 1)+
  coord_equal()+
  #theme_classic()+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  scale_x_discrete(position = "top")+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=0,vjust=0.5),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.key=element_blank(),
        aspect.ratio = 8/3)

Fig.7b<-ggplot()+
  geom_tile(data=rel_00_10,
            aes(x=variable,y=object,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "Reds",direction = 1)+
  coord_equal()+
  #theme_classic()+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  scale_x_discrete(position = "top")+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=0,vjust=0.5),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.key=element_blank(),
        aspect.ratio = 8/3)

Fig.7c<-ggplot()+
  geom_tile(data=rel_11_20,
            aes(x=variable,y=object,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "Reds",direction = 1)+
  coord_equal()+
  #theme_classic()+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  scale_x_discrete(position = "top")+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=0,vjust=0.5),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.key=element_blank(),
        aspect.ratio = 8/3)


Fig.7d<-ggplot()+
  geom_tile(data=slope_00_20,
            aes(x=variable,y=object,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "BrBG",direction = -1)+
  coord_equal()+
  #theme_classic()+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  scale_x_discrete(position = "top")+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=0,vjust=0.5),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.key=element_blank(),
        aspect.ratio = 8/3)

Fig.7e<-ggplot()+
  geom_tile(data=slope_00_10,
            aes(x=variable,y=object,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "BrBG",direction = -1)+
  coord_equal()+
  #theme_classic()+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  scale_x_discrete(position = "top")+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=0,vjust=0.5),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.key=element_blank(),
        aspect.ratio = 8/3)

Fig.7f<-ggplot()+
  geom_tile(data=slope_11_20,
            aes(x=variable,y=object,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "BrBG",direction = -1)+
  coord_equal()+
  #theme_classic()+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  scale_x_discrete(position = "top")+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=0,vjust=0.5),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.key=element_blank(),
        aspect.ratio = 8/3)

Fig.7a
Fig.7b
Fig.7c
Fig.7d
Fig.7e
Fig.7f

export::graph2ppt(Fig.7a,file="Fig.7a.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.7b,file="Fig.7b.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.7c,file="Fig.7c.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.7d,file="Fig.7d.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.7e,file="Fig.7e.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.7f,file="Fig.7f.pptx",width = 5,height = 4.5)
