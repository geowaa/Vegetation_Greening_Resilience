rm(list = ls())
library(raster)
library(terra)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##==== prepare data ====##
bioclimatic_zonal <- readOGR("./Bioclimatic zonal/modi_bioclimatic_zonal.shp",layer = "modi_bioclimatic_zonal")
# fortify, i.e., make ggplot2-compatible
bioclimatic_zonal <- fortify(bioclimatic_zonal)

#load kendall tau data
AR1_win60_kendall_2000_2010<-rast("./AR1_pixel_win60/2000_2010/AR1_win60_kendall_2000_2010.tif")
AR1_win60_kendall_2011_2020<-rast("./AR1_pixel_win60/2011_2020/AR1_win60_kendall_2011_2020.tif")
AR1_win60_kendall_2000_2020<-rast("./AR1_pixel_win60/2000_2020/AR1_win60_kendall.tif")
AR1_win60_kendall_2000_2010<-stack(AR1_win60_kendall_2000_2010)
AR1_win60_kendall_2011_2020<-stack(AR1_win60_kendall_2011_2020)
AR1_win60_kendall_2000_2020<-stack(AR1_win60_kendall_2000_2020)

#load vegetation land for mask purpose
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#mask kendall tau by land use
coef_00_10<-mask(AR1_win60_kendall_2000_2010[[1]],vegetation_land)
pvalue_00_10<-mask(AR1_win60_kendall_2000_2010[[2]],vegetation_land)
coef_11_20<-mask(AR1_win60_kendall_2011_2020[[1]],vegetation_land)
pvalue_11_20<-mask(AR1_win60_kendall_2011_2020[[2]],vegetation_land)
coef_00_20<-mask(AR1_win60_kendall_2000_2020[[1]],vegetation_land)
pvalue_00_20<-mask(AR1_win60_kendall_2000_2020[[2]],vegetation_land)

#data 2000-2010
coef_00_10 <- as(coef_00_10, "SpatialPixelsDataFrame")
pvalue_00_10 <- as(pvalue_00_10, "SpatialPixelsDataFrame")
coef_00_10 <- as.data.frame(coef_00_10) %>% 
  rename(kendall_coef = kendall_coef)
pvalue_00_10 <- as.data.frame(pvalue_00_10) %>% 
  rename(kendall_pvalue = kendall_pvalue)
cbind_data<-cbind(coef_00_10[,2:3],coef_00_10[,1],pvalue_00_10[,1])
colnames(cbind_data) <- c('x','y','coef','p')
findset1<-which(cbind_data$p > 0.05)
kendall_00_10<-cbind_data[-findset1,]

#data 2011-2020
coef_11_20 <- as(coef_11_20, "SpatialPixelsDataFrame")
pvalue_11_20 <- as(pvalue_11_20, "SpatialPixelsDataFrame")
coef_11_20 <- as.data.frame(coef_11_20) %>% 
  rename(kendall_coef = kendall_coef)
pvalue_11_20 <- as.data.frame(pvalue_11_20) %>% 
  rename(kendall_pvalue = kendall_pvalue)
cbind_data2<-cbind(coef_11_20[,2:3],coef_11_20[,1],pvalue_11_20[,1])
colnames(cbind_data2) <- c('x','y','coef','p')
findset2<-which(cbind_data2$p > 0.05)
kendall_11_20<-cbind_data2[-findset2,]

#data 2000-2020
coef_00_20 <- as(coef_00_20, "SpatialPixelsDataFrame")
pvalue_00_20 <- as(pvalue_00_20, "SpatialPixelsDataFrame")
coef_00_20 <- as.data.frame(coef_00_20) %>% 
  rename(kendall_coef = kendall_coef)
pvalue_00_20 <- as.data.frame(pvalue_00_20) %>% 
  rename(kendall_pvalue = kendall_pvalue)
cbind_data3<-cbind(coef_00_20[,2:3],coef_00_20[,1],pvalue_00_20[,1])
colnames(cbind_data3) <- c('x','y','coef','p')
findset3<-which(cbind_data3$p > 0.05)
kendall_00_20<-cbind_data3[-findset3,]
##==== prepare data ====##

##==== Draw ====##
crs<-"+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs"

Fig.2d <- ggplot()+
  geom_raster(data=kendall_00_20,aes(x = x,y = y,fill = coef))+
  #geom_raster(data=FG_ar1_rwin60_data2_kendall_p_insig,aes(x = x,y = y),fill="white")+
  scale_fill_distiller(palette = 'BrBG')+
  geom_polygon(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),fill = NA,colour = "black",size = 0.5)+
  coord_sf(crs=crs)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_blank(),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_rect(colour = "black",size = 0.5), #element_blank()
        legend.position = "bottom",
        aspect.ratio = 3/4)  #bottom none
Fig.2d

Fig.2e <- ggplot()+
  geom_raster(data=kendall_00_10,aes(x = x,y = y,fill = coef))+
  #geom_raster(data=FG_ar1_rwin60_data1_kendall_p_insig,aes(x = x,y = y),fill="white")+
  scale_fill_distiller(palette = 'BrBG')+
  #geom_line(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),colour = "black",size = 0.5)+
  geom_polygon(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),fill = NA,colour = "black",size = 0.5)+
  coord_sf(crs=crs)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_blank(),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_rect(colour = "black",size = 0.5), #element_blank()
        legend.position = "bottom",
        aspect.ratio = 3/4)  #bottom none
Fig.2e

Fig.2f <- ggplot()+
  geom_raster(data=kendall_11_20,aes(x = x,y = y,fill = coef))+
  #geom_raster(data=FG_ar1_rwin60_data2_kendall_p_insig,aes(x = x,y = y),fill="white")+
  scale_fill_distiller(palette = 'BrBG')+
  geom_polygon(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),fill = NA,colour = "black",size = 0.5)+
  coord_sf(crs=crs)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_blank(),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_rect(colour = "black",size = 0.5), #element_blank()
        legend.position = "bottom",
        aspect.ratio = 3/4)  #bottom none
Fig.2f

#library(ggpubr)
ggarrange(Fig.2d,Fig.2e,Fig.2f,
          nrow=1,
          widths=c(0.5,0.5,0.5))

export::graph2ppt(plot_2000_2010,file="plot_2000_2010.pptx",width = 5,height = 4.5)
export::graph2ppt(plot_2011_2020,file="plot_2011_2020.pptx",width = 5,height = 4.5)
export::graph2ppt(plot_2000_2020,file="plot_2000_2020.pptx",width = 5,height = 4.5)