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

#load kNDVI sen+MK
kNDVI_trend_2000_2010<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2010.tif")
kNDVI_trend_2011_2020<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2011_2020.tif")
kNDVI_trend_2000_2020<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2020.tif")
kNDVI_trend_2000_2010<-stack(kNDVI_trend_2000_2010)
kNDVI_trend_2011_2020<-stack(kNDVI_trend_2011_2020)
kNDVI_trend_2000_2020<-stack(kNDVI_trend_2000_2020)

#load vegetation land for mask purpose
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#mask kNDVI trend by land use
coef_00_10<-mask(kNDVI_trend_2000_2010[[1]],vegetation_land)
pvalue_00_10<-mask(kNDVI_trend_2000_2010[[3]],vegetation_land)
coef_11_20<-mask(kNDVI_trend_2011_2020[[1]],vegetation_land)
pvalue_11_20<-mask(kNDVI_trend_2011_2020[[3]],vegetation_land)
coef_00_20<-mask(kNDVI_trend_2000_2020[[1]],vegetation_land)
pvalue_00_20<-mask(kNDVI_trend_2000_2020[[3]],vegetation_land)

#data 2000-2010
coef_00_10 <- as(coef_00_10, "SpatialPixelsDataFrame")
pvalue_00_10 <- as(pvalue_00_10, "SpatialPixelsDataFrame")
coef_00_10 <- as.data.frame(coef_00_10) %>% 
  rename(slope = slope)
pvalue_00_10 <- as.data.frame(pvalue_00_10) %>% 
  rename(MK_test = MK_test)
cbind_data<-cbind(coef_00_10[,2:3],coef_00_10[,1],pvalue_00_10[,1])
colnames(cbind_data) <- c('x','y','coef','p')
findset1<-which(cbind_data$p > 0.05)
greening_00_10<-cbind_data[-findset1,]

#data 2011-2020
coef_11_20 <- as(coef_11_20, "SpatialPixelsDataFrame")
pvalue_11_20 <- as(pvalue_11_20, "SpatialPixelsDataFrame")
coef_11_20 <- as.data.frame(coef_11_20) %>% 
  rename(slope = slope)
pvalue_11_20 <- as.data.frame(pvalue_11_20) %>% 
  rename(MK_test = MK_test)
cbind_data2<-cbind(coef_11_20[,2:3],coef_11_20[,1],pvalue_11_20[,1])
colnames(cbind_data2) <- c('x','y','coef','p')
findset2<-which(cbind_data2$p > 0.05)
greening_11_20<-cbind_data2[-findset2,]

#data 2000-2020
coef_00_20 <- as(coef_00_20, "SpatialPixelsDataFrame")
pvalue_00_20 <- as(pvalue_00_20, "SpatialPixelsDataFrame")
coef_00_20 <- as.data.frame(coef_00_20) %>% 
  rename(slope = slope)
pvalue_00_20 <- as.data.frame(pvalue_00_20) %>% 
  rename(MK_test = MK_test)
cbind_data3<-cbind(coef_00_20[,2:3],coef_00_20[,1],pvalue_00_20[,1])
colnames(cbind_data3) <- c('x','y','coef','p')
findset3<-which(cbind_data3$p > 0.05)
greening_00_20<-cbind_data3[-findset3,]

length(which(greening_00_20$coef>0))/(nrow(greening_00_20)+length(findset3))
length(which(greening_00_20$coef<0))/(nrow(greening_00_20)+length(findset3))
##==== prepare data ====##

##==== Draw ====##
crs<-"+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs"

pretty_breaks <- c(0,0.003,0.006)
minVal_1 <- min(greening_00_10$coef, na.rm = T)
maxVal_1 <- max(greening_00_10$coef, na.rm = T)
brks_1 <- c(minVal_1, pretty_breaks, maxVal_1)

minVal_2 <- min(greening_11_20$coef, na.rm = T)
maxVal_2 <- max(greening_11_20$coef, na.rm = T)
brks_2 <- c(minVal_2, pretty_breaks, maxVal_2)

minVal_3 <- min(greening_00_20$coef, na.rm = T)
maxVal_3 <- max(greening_00_20$coef, na.rm = T)
brks_3 <- c(minVal_3, pretty_breaks, maxVal_3)

#cut() function allows you to cut data into bins and specify ‘cut labels’, 
#so it is beneficial to create a factor from a continuous variable.
greening_00_10$brks <- cut(greening_00_10$coef, 
                           breaks = brks_1, 
                           include.lowest = TRUE)
greening_11_20$brks <- cut(greening_11_20$coef, 
                           breaks = brks_2, 
                           include.lowest = TRUE)
greening_00_20$brks <- cut(greening_00_20$coef, 
                           breaks = brks_3, 
                           include.lowest = TRUE)

my_col <- c("#e34a33", "#CDF57A", "#709727", "#267300")

Fig.2a <- ggplot()+
  geom_raster(data=greening_00_20,aes(x = x,y = y,fill = brks))+
  #geom_raster(data=FG_ar1_rwin60_data2_kendall_p_insig,aes(x = x,y = y),fill="white")+
  scale_fill_manual(values = my_col, name = "slope")+
  geom_polygon(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),fill = NA,colour = "black",size = 0.5)+
  coord_sf(crs=crs)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_blank(),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_rect(colour = "black",size = 0.5), #element_blank()
        legend.position = "none",
        aspect.ratio = 3/4)  #bottom none
Fig.2a

Fig.2b <- ggplot()+
  geom_raster(data=greening_00_10,aes(x = x,y = y,fill = brks))+
  #geom_raster(data=FG_ar1_rwin60_data1_kendall_p_insig,aes(x = x,y = y),fill="white")+
  scale_fill_manual(values = my_col, name = "slope")+
  #geom_line(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),colour = "black",size = 0.5)+
  geom_polygon(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),fill = NA,colour = "black",size = 0.5)+
  coord_sf(crs=crs)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_blank(),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_rect(colour = "black",size = 0.5), #element_blank()
        legend.position = "none",
        aspect.ratio = 3/4)  #bottom none
Fig.2b

Fig.2c <- ggplot()+
  geom_raster(data=greening_11_20,aes(x = x,y = y,fill = brks))+
  #geom_raster(data=FG_ar1_rwin60_data2_kendall_p_insig,aes(x = x,y = y),fill="white")+
  scale_fill_manual(values = my_col, name = "slope")+
  geom_polygon(data=bioclimatic_zonal,aes(x=long,y=lat,group=id),fill = NA,colour = "black",size = 0.5)+
  coord_sf(crs=crs)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_blank(),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_rect(colour = "black",size = 0.5), #element_blank()
        legend.position = "none",
        aspect.ratio = 3/4)  #bottom none
Fig.2c

#library(ggpubr)
ggarrange(Fig.2a,Fig.2b,Fig.2c,
          nrow=1,
          widths=c(0.5,0.5,0.5))

export::graph2ppt(Fig.2a,file="Fig.2a.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.2b,file="Fig.2b.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.2c,file="Fig.2c.pptx",width = 5,height = 4.5)

# legend style
guides(fill=guide_legend(
  direction = "horizontal", #horizontal
  keyheight = unit(3, units = "mm"), #3
  keywidth = unit(15, units = "mm"),  #20
  title.position = 'top', #top
  title.hjust = 0.5,
  label.hjust = .5,
  nrow =1,
  byrow = T,  #T
  label.position = "bottom"))

