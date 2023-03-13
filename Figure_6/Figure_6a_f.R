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

#prepare slope data for 2000 to 2020
kNDVI_slope_00_20<-Extra_alldata2000_2020[,c(1,2)]
kNDVI_rel_00_20<-Extra_alldata2000_2020[,c(3,2)]
premean_slope_00_20<-Extra_alldata2000_2020[,c(4,5)]
premean_rel_00_20<-Extra_alldata2000_2020[,c(6,5)]
precv_slope_00_20<-Extra_alldata2000_2020[,c(7,8)]
precv_rel_00_20<-Extra_alldata2000_2020[,c(9,8)]
temmean_slope_00_20<-Extra_alldata2000_2020[,c(10,11)]
temmean_rel_00_20<-Extra_alldata2000_2020[,c(12,11)]
temcv_slope_00_20<-Extra_alldata2000_2020[,c(13,14)]
temcv_rel_00_20<-Extra_alldata2000_2020[,c(15,14)]
aimean_slope_00_20<-Extra_alldata2000_2020[,c(16,17)]
aimean_rel_00_20<-Extra_alldata2000_2020[,c(18,17)]
aicv_slope_00_20<-Extra_alldata2000_2020[,c(19,20)]
aicv_rel_00_20<-Extra_alldata2000_2020[,c(21,20)]
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

colnames(kNDVI_slope_00_20)<-c("slope","p_value")
colnames(premean_slope_00_20)<-c("slope","p_value")
colnames(precv_slope_00_20)<-c("slope","p_value")
colnames(temmean_slope_00_20)<-c("slope","p_value")
colnames(temcv_slope_00_20)<-c("slope","p_value")
colnames(aimean_slope_00_20)<-c("slope","p_value")
colnames(aicv_slope_00_20)<-c("slope","p_value")

colnames(kNDVI_rel_00_20)<-c("rel","p_value")
colnames(premean_rel_00_20)<-c("rel","p_value")
colnames(precv_rel_00_20)<-c("rel","p_value")
colnames(temmean_rel_00_20)<-c("rel","p_value")
colnames(temcv_rel_00_20)<-c("rel","p_value")
colnames(aimean_rel_00_20)<-c("rel","p_value")
colnames(aicv_rel_00_20)<-c("rel","p_value")

kNDVI_slope_00_20$variable<-rep("kNDVI_slope_00_20",301939)
premean_slope_00_20$variable<-rep("premean_slope_00_20",285189)
precv_slope_00_20$variable<-rep("precv_slope_00_20",275048)
temmean_slope_00_20$variable<-rep("temmean_slope_00_20",287246)
temcv_slope_00_20$variable<-rep("temcv_slope_00_20",275395)
aimean_slope_00_20$variable<-rep("aimean_slope_00_20",269412)
aicv_slope_00_20$variable<-rep("aicv_slope_00_20",271982)

kNDVI_rel_00_20$variable<-rep("kNDVI_rel_00_20",301939)
premean_rel_00_20$variable<-rep("premean_rel_00_20",285189)
precv_rel_00_20$variable<-rep("precv_rel_00_20",275048)
temmean_rel_00_20$variable<-rep("temmean_rel_00_20",287246)
temcv_rel_00_20$variable<-rep("temcv_rel_00_20",275395)
aimean_rel_00_20$variable<-rep("aimean_rel_00_20",269412)
aicv_rel_00_20$variable<-rep("aicv_rel_00_20",271982)

slope00_20<-rbind(kNDVI_slope_00_20,premean_slope_00_20,precv_slope_00_20,
      temmean_slope_00_20,temcv_slope_00_20,aimean_slope_00_20,aicv_slope_00_20)

rel00_20<-rbind(kNDVI_rel_00_20,premean_rel_00_20,precv_rel_00_20,
                  temmean_rel_00_20,temcv_rel_00_20,aimean_rel_00_20,aicv_rel_00_20)

slope00_20$variable <- factor(slope00_20$variable,levels=c("kNDVI_slope_00_20",
                                         "premean_slope_00_20",
                                         "precv_slope_00_20",
                                         "temmean_slope_00_20",
                                         "temcv_slope_00_20",
                                         "aimean_slope_00_20",
                                         "aicv_slope_00_20"))
rel00_20$variable <- factor(rel00_20$variable,levels=c("kNDVI_rel_00_20",
                                                           "premean_rel_00_20",
                                                           "precv_rel_00_20",
                                                           "temmean_rel_00_20",
                                                           "temcv_rel_00_20",
                                                           "aimean_rel_00_20",
                                                           "aicv_rel_00_20"))

#prepare data for 2000 to 2010
kNDVI_slope_00_10<-Extra_alldata2000_2010[,c(1,2)]
kNDVI_rel_00_10<-Extra_alldata2000_2010[,c(3,2)]
premean_slope_00_10<-Extra_alldata2000_2010[,c(4,5)]
premean_rel_00_10<-Extra_alldata2000_2010[,c(6,5)]
precv_slope_00_10<-Extra_alldata2000_2010[,c(7,8)]
precv_rel_00_10<-Extra_alldata2000_2010[,c(9,8)]
temmean_slope_00_10<-Extra_alldata2000_2010[,c(10,11)]
temmean_rel_00_10<-Extra_alldata2000_2010[,c(12,11)]
temcv_slope_00_10<-Extra_alldata2000_2010[,c(13,14)]
temcv_rel_00_10<-Extra_alldata2000_2010[,c(15,14)]
aimean_slope_00_10<-Extra_alldata2000_2010[,c(16,17)]
aimean_rel_00_10<-Extra_alldata2000_2010[,c(18,17)]
aicv_slope_00_10<-Extra_alldata2000_2010[,c(19,20)]
aicv_rel_00_10<-Extra_alldata2000_2010[,c(21,20)]
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
aimean_rel_00_10<-aimean_rel_00_10[-which(aimean_rel_00_10$aimean_p > 0.05|is.na(aimean_rel_00_10$aimean_p)),]
aicv_slope_00_10<-aicv_slope_00_10[-which(aicv_slope_00_10$aicv_p > 0.05|is.na(aicv_slope_00_10$aicv_p)),]
aicv_rel_00_10<-aicv_rel_00_10[-which(aicv_rel_00_10$aicv_p > 0.05|is.na(aicv_rel_00_10$aicv_p)),]

colnames(kNDVI_slope_00_10)<-c("slope","p_value")
colnames(premean_slope_00_10)<-c("slope","p_value")
colnames(precv_slope_00_10)<-c("slope","p_value")
colnames(temmean_slope_00_10)<-c("slope","p_value")
colnames(temcv_slope_00_10)<-c("slope","p_value")
colnames(aimean_slope_00_10)<-c("slope","p_value")
colnames(aicv_slope_00_10)<-c("slope","p_value")

colnames(kNDVI_rel_00_10)<-c("rel","p_value")
colnames(premean_rel_00_10)<-c("rel","p_value")
colnames(precv_rel_00_10)<-c("rel","p_value")
colnames(temmean_rel_00_10)<-c("rel","p_value")
colnames(temcv_rel_00_10)<-c("rel","p_value")
colnames(aimean_rel_00_10)<-c("rel","p_value")
colnames(aicv_rel_00_10)<-c("rel","p_value")

kNDVI_slope_00_10$variable<-rep("kNDVI_slope_00_10",251378)
premean_slope_00_10$variable<-rep("premean_slope_00_10",189546)
precv_slope_00_10$variable<-rep("precv_slope_00_10",161620)
temmean_slope_00_10$variable<-rep("temmean_slope_00_10",209156)
temcv_slope_00_10$variable<-rep("temcv_slope_00_10",197256)
aimean_slope_00_10$variable<-rep("aimean_slope_00_10",182147)
aicv_slope_00_10$variable<-rep("aicv_slope_00_10",188969)

kNDVI_rel_00_10$variable<-rep("kNDVI_rel_00_10",251378)
premean_rel_00_10$variable<-rep("premean_rel_00_10",189546)
precv_rel_00_10$variable<-rep("precv_rel_00_10",161620)
temmean_rel_00_10$variable<-rep("temmean_rel_00_10",209156)
temcv_rel_00_10$variable<-rep("temcv_rel_00_10",197256)
aimean_rel_00_10$variable<-rep("aimean_rel_00_10",182147)
aicv_rel_00_10$variable<-rep("aicv_rel_00_10",188969)

slope00_10<-rbind(kNDVI_slope_00_10,premean_slope_00_10,precv_slope_00_10,
                  temmean_slope_00_10,temcv_slope_00_10,aimean_slope_00_10,aicv_slope_00_10)
rel00_10<-rbind(kNDVI_rel_00_10,premean_rel_00_10,precv_rel_00_10,
                  temmean_rel_00_10,temcv_rel_00_10,aimean_rel_00_10,aicv_rel_00_10)

slope00_10$variable <- factor(slope00_10$variable,levels=c("kNDVI_slope_00_10",
                                                           "premean_slope_00_10",
                                                           "precv_slope_00_10",
                                                           "temmean_slope_00_10",
                                                           "temcv_slope_00_10",
                                                           "aimean_slope_00_10",
                                                           "aicv_slope_00_10"))
rel00_10$variable <- factor(rel00_10$variable,levels=c("kNDVI_rel_00_10",
                                                           "premean_rel_00_10",
                                                           "precv_rel_00_10",
                                                           "temmean_rel_00_10",
                                                           "temcv_rel_00_10",
                                                           "aimean_rel_00_10",
                                                           "aicv_rel_00_10"))

#prepare data for 2011 to 2020
kNDVI_slope_11_20<-Extra_alldata2011_2020[,c(1,2)]
kNDVI_rel_11_20<-Extra_alldata2011_2020[,c(3,2)]
premean_slope_11_20<-Extra_alldata2011_2020[,c(4,5)]
premean_rel_11_20<-Extra_alldata2011_2020[,c(6,5)]
precv_slope_11_20<-Extra_alldata2011_2020[,c(7,8)]
precv_rel_11_20<-Extra_alldata2011_2020[,c(9,8)]
temmean_slope_11_20<-Extra_alldata2011_2020[,c(10,11)]
temmean_rel_11_20<-Extra_alldata2011_2020[,c(12,11)]
temcv_slope_11_20<-Extra_alldata2011_2020[,c(13,14)]
temcv_rel_11_20<-Extra_alldata2011_2020[,c(15,14)]
aimean_slope_11_20<-Extra_alldata2011_2020[,c(16,17)]
aimean_rel_11_20<-Extra_alldata2011_2020[,c(18,17)]
aicv_slope_11_20<-Extra_alldata2011_2020[,c(19,20)]
aicv_rel_11_20<-Extra_alldata2011_2020[,c(21,20)]
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

colnames(kNDVI_slope_11_20)<-c("slope","p_value")
colnames(premean_slope_11_20)<-c("slope","p_value")
colnames(precv_slope_11_20)<-c("slope","p_value")
colnames(temmean_slope_11_20)<-c("slope","p_value")
colnames(temcv_slope_11_20)<-c("slope","p_value")
colnames(aimean_slope_11_20)<-c("slope","p_value")
colnames(aicv_slope_11_20)<-c("slope","p_value")

colnames(kNDVI_rel_11_20)<-c("rel","p_value")
colnames(premean_rel_11_20)<-c("rel","p_value")
colnames(precv_rel_11_20)<-c("rel","p_value")
colnames(temmean_rel_11_20)<-c("rel","p_value")
colnames(temcv_rel_11_20)<-c("rel","p_value")
colnames(aimean_rel_11_20)<-c("rel","p_value")
colnames(aicv_rel_11_20)<-c("rel","p_value")

kNDVI_slope_11_20$variable<-rep("kNDVI_slope_11_20",217084)
premean_slope_11_20$variable<-rep("premean_slope_11_20",168881)
precv_slope_11_20$variable<-rep("precv_slope_11_20",170534)
temmean_slope_11_20$variable<-rep("temmean_slope_11_20",205846)
temcv_slope_11_20$variable<-rep("temcv_slope_11_20",158525)
aimean_slope_11_20$variable<-rep("aimean_slope_11_20",165461)
aicv_slope_11_20$variable<-rep("aicv_slope_11_20",128115)

kNDVI_rel_11_20$variable<-rep("kNDVI_rel_11_20",217084)
premean_rel_11_20$variable<-rep("premean_rel_11_20",168881)
precv_rel_11_20$variable<-rep("precv_rel_11_20",170534)
temmean_rel_11_20$variable<-rep("temmean_rel_11_20",205846)
temcv_rel_11_20$variable<-rep("temcv_rel_11_20",158525)
aimean_rel_11_20$variable<-rep("aimean_rel_11_20",165461)
aicv_rel_11_20$variable<-rep("aicv_rel_11_20",128115)

slope11_20<-rbind(kNDVI_slope_11_20,premean_slope_11_20,precv_slope_11_20,
                  temmean_slope_11_20,temcv_slope_11_20,aimean_slope_11_20,aicv_slope_11_20)
rel11_20<-rbind(kNDVI_rel_11_20,premean_rel_11_20,precv_rel_11_20,
                  temmean_rel_11_20,temcv_rel_11_20,aimean_rel_11_20,aicv_rel_11_20)

slope11_20$variable <- factor(slope11_20$variable,levels=c("kNDVI_slope_11_20",
                                                           "premean_slope_11_20",
                                                           "precv_slope_11_20",
                                                           "temmean_slope_11_20",
                                                           "temcv_slope_11_20",
                                                           "aimean_slope_11_20",
                                                           "aicv_slope_11_20"))
rel11_20$variable <- factor(rel11_20$variable,levels=c("kNDVI_rel_11_20",
                                                           "premean_rel_11_20",
                                                           "precv_rel_11_20",
                                                           "temmean_rel_11_20",
                                                           "temcv_rel_11_20",
                                                           "aimean_rel_11_20",
                                                           "aicv_rel_11_20"))

##--Draw--##
# Custom theme
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(5,5,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(size = 0.2),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
                   axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
                   axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
                   axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'))
  return(myTheme)
}

Fig.6a<-ggplot(rel00_20,aes(x=variable,y=rel))+
  geom_boxplot(aes(fill=variable),outlier.shape = NA,width=0.5,size = 0.3)+
  scale_fill_jama(alpha = 0.8)+
  ylim(0,0.7)+
  theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5,5,3,3),
        plot.background = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.15,'lines'),
        axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
        axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
        axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
        axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'),
        aspect.ratio = 3/4)

Fig.6b<-ggplot(rel00_10,aes(x=variable,y=rel))+
  geom_boxplot(aes(fill=variable),outlier.shape = NA,width=0.5,size = 0.3)+
  scale_fill_jama(alpha = 0.8)+
  ylim(0,0.7)+
  theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5,5,3,3),
        plot.background = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.15,'lines'),
        axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
        axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
        axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
        axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'),
        aspect.ratio = 3/4)

Fig.6c<-ggplot(rel11_20,aes(x=variable,y=rel))+
  geom_boxplot(aes(fill=variable),outlier.shape = NA,width=0.5,size = 0.3)+
  scale_fill_jama(alpha = 0.8)+
  ylim(0,0.7)+
  theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5,5,3,3),
        plot.background = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.15,'lines'),
        axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
        axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
        axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
        axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'),
        aspect.ratio = 3/4)

Fig.6d<-ggplot(slope00_20,aes(x=variable,y=slope))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_boxplot(aes(fill=variable),outlier.shape = NA,width=0.5,size = 0.3)+
  scale_fill_jama(alpha = 0.8)+
  ylim(-4,4)+
  theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5,5,3,3),
        plot.background = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.15,'lines'),
        axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
        axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
        axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
        axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'),
        aspect.ratio = 3/4)



Fig.6e<-ggplot(slope00_10,aes(x=variable,y=slope))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_boxplot(aes(fill=variable),outlier.shape = NA,width=0.5,size = 0.3)+
  scale_fill_jama(alpha = 0.8)+
  ylim(-4,4)+
  theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5,5,3,3),
        plot.background = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.15,'lines'),
        axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
        axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
        axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
        axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'),
        aspect.ratio = 3/4)



Fig.6f<-ggplot(slope11_20,aes(x=variable,y=slope))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_boxplot(aes(fill=variable),outlier.shape = NA,width=0.5,size = 0.3)+
  scale_fill_jama(alpha = 0.8)+
  ylim(-4,4)+
  theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5,5,3,3),
        plot.background = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.15,'lines'),
        axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
        axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
        axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
        axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'),
        aspect.ratio = 3/4)

Fig.6a
Fig.6b
Fig.6c
Fig.6d
Fig.6e
Fig.6f

export::graph2ppt(Fig.6a,file="Fig.6a.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.6b,file="Fig.6b.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.6c,file="Fig.6c.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.6d,file="Fig.6d.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.6e,file="Fig.6e.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.6f,file="Fig.6f.pptx",width = 5,height = 4.5)

library(agricolae)
model_00_20<-aov(rel~variable,data=rel00_20)
SNK.test(model_00_20,"variable", console=TRUE, 
         main="comparison for 00_20 data")

model_00_10<-aov(rel~variable,data=rel00_10)
SNK.test(model_00_10,"variable", console=TRUE, 
         main="comparison for 00_10 data")

model_00_10<-aov(rel~variable,data=rel00_10)
SNK.test(model_00_10,"variable", console=TRUE, 
         main="comparison for 00_10 data")

model_11_20<-aov(rel~variable,data=rel11_20)
SNK.test(model_11_20,"variable", console=TRUE, 
         main="comparison for 00_10 data")

