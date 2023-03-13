rm(list = ls())
library(raster)
library(terra)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(dplyr)
library(tidyr)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##==== prepare data ====##
#load kNDVI sen+MK
kNDVI_trend_2000_2010<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2010.tif")
kNDVI_trend_2011_2020<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2011_2020.tif")
kNDVI_trend_2000_2020<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2020.tif")
kNDVI_trend_2000_2010<-stack(kNDVI_trend_2000_2010)
kNDVI_trend_2011_2020<-stack(kNDVI_trend_2011_2020)
kNDVI_trend_2000_2020<-stack(kNDVI_trend_2000_2020)

#load kendall tau data
AR1_win60_kendall_2000_2010<-rast("./AR1_pixel_win60/2000_2010/AR1_win60_kendall_2000_2010.tif")
AR1_win60_kendall_2011_2020<-rast("./AR1_pixel_win60/2011_2020/AR1_win60_kendall_2011_2020.tif")
AR1_win60_kendall_2000_2020<-rast("./AR1_pixel_win60/2000_2020/AR1_win60_kendall.tif")
AR1_win60_kendall_2000_2010<-stack(AR1_win60_kendall_2000_2010)
AR1_win60_kendall_2011_2020<-stack(AR1_win60_kendall_2011_2020)
AR1_win60_kendall_2000_2020<-stack(AR1_win60_kendall_2000_2020)

#load vegetation land for mask purpose
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#mask kNDVI trend by land use
kNDVI_coef_00_10<-mask(kNDVI_trend_2000_2010[[1]],vegetation_land)
kNDVI_pvalue_00_10<-mask(kNDVI_trend_2000_2010[[3]],vegetation_land)
kNDVI_coef_11_20<-mask(kNDVI_trend_2011_2020[[1]],vegetation_land)
kNDVI_pvalue_11_20<-mask(kNDVI_trend_2011_2020[[3]],vegetation_land)
kNDVI_coef_00_20<-mask(kNDVI_trend_2000_2020[[1]],vegetation_land)
kNDVI_pvalue_00_20<-mask(kNDVI_trend_2000_2020[[3]],vegetation_land)
kNDVI_coef_00_10<-rast(kNDVI_coef_00_10)

kendall_coef_00_10<-mask(AR1_win60_kendall_2000_2010[[1]],vegetation_land)
kendall_pvalue_00_10<-mask(AR1_win60_kendall_2000_2010[[2]],vegetation_land)
kendall_coef_11_20<-mask(AR1_win60_kendall_2011_2020[[1]],vegetation_land)
kendall_pvalue_11_20<-mask(AR1_win60_kendall_2011_2020[[2]],vegetation_land)
kendall_coef_00_20<-mask(AR1_win60_kendall_2000_2020[[1]],vegetation_land)
kendall_pvalue_00_20<-mask(AR1_win60_kendall_2000_2020[[2]],vegetation_land)

#Reclassify kNDVI datasets and kendall datasets
##--prepare the data kNDVI--##
#https://blog.csdn.net/keepclam/article/details/124604521
kNDVI_coef_00_10<-stack(kNDVI_coef_00_10)

a1<-cellStats(kNDVI_coef_00_10,stat = "max")
a2<-cellStats(kNDVI_coef_11_20,stat = "max")
a3<-cellStats(kNDVI_coef_00_20,stat = "max")

b1<-cellStats(kNDVI_coef_00_10,stat = "min")
b2<-cellStats(kNDVI_coef_11_20,stat = "min")
b3<-cellStats(kNDVI_coef_00_20,stat = "min")

kNDVI_coef_type<-c(min(b1,b2,b3),0,1,
              0,max(a1,a2,a3),2)
#min(b1,b2,b3)--kNDVI_coef--0 →→1
#0--kNDVI_coef--max(a1,a2,a3) →→2
rcl_kNDVI_coef_type<-matrix(kNDVI_coef_type,ncol=3,byrow = TRUE)

kNDVI_pvalue_type<-c(0,0.05,3,
                   0.05,1,4)
#0--kNDVI_pvalue_type--0.05 →→3
#0.05--kNDVI_pvalue_type--1 →→4
rcl_kNDVI_pvalue_type<-matrix(kNDVI_pvalue_type,ncol=3,byrow = TRUE)

#Running 'classify' function need "SpatRaster"
kNDVI_coef_00_10<-rast(kNDVI_coef_00_10)
kNDVI_pvalue_00_10<-rast(kNDVI_pvalue_00_10)
kNDVI_coef_11_20<-rast(kNDVI_coef_11_20)
kNDVI_pvalue_11_20<-rast(kNDVI_pvalue_11_20)
kNDVI_coef_00_20<-rast(kNDVI_coef_00_20)
kNDVI_pvalue_00_20<-rast(kNDVI_pvalue_00_20)

kNDVI_coef_00_10<-classify(kNDVI_coef_00_10,rcl_kNDVI_coef_type)
kNDVI_pvalue_00_10<-classify(kNDVI_pvalue_00_10,rcl_kNDVI_pvalue_type)
kNDVI_coef_11_20<-classify(kNDVI_coef_11_20,rcl_kNDVI_coef_type,include.lowest=TRUE) #这个存在问题
kNDVI_pvalue_11_20<-classify(kNDVI_pvalue_11_20,rcl_kNDVI_pvalue_type)
kNDVI_coef_00_20<-classify(kNDVI_coef_00_20,rcl_kNDVI_coef_type)
kNDVI_pvalue_00_20<-classify(kNDVI_pvalue_00_20,rcl_kNDVI_pvalue_type)

#multiply the raster 'coef' and raster 'pvalue'
kNDVI_00_10<-kNDVI_coef_00_10*kNDVI_pvalue_00_10
kNDVI_11_20<-kNDVI_coef_11_20*kNDVI_pvalue_11_20
kNDVI_00_20<-kNDVI_coef_00_20*kNDVI_pvalue_00_20
#3 coef<0, P<0.05 Significant browning
#4 coef<0, P>0.05 Non-significant browning
#6 coef>0, P<0.05 Significant greening
#8 coef>0, P>0.05 Non-significant greening

#merge type 4 and 8
kNDVI_00_10[kNDVI_00_10 == 8] <- 4
kNDVI_11_20[kNDVI_11_20 == 8] <- 4
kNDVI_00_20[kNDVI_00_20 == 8] <- 4
#3 coef<0, P<0.05 Significant browning
#4 ------, P>0.05 Non-significant trend
#6 coef>0, P<0.05 Significant greening

##--prepare the data kendall--##
#classify for kendall data
kendall_coef_type<-c(-1,0,5,
                   0,1,6)
#-1--kendall_coef--0 →→5
#0--kendall_coef--1 →→6
rcl_kendall_coef_type<-matrix(kendall_coef_type,ncol=3,byrow = TRUE)

kendall_pvalue_type<-c(0,0.05,7,
                     0.05,1,8)
#0--kNDVI_pvalue_type--0.05 →→7
#0.05--kNDVI_pvalue_type--1 →→8
rcl_kendall_pvalue_type<-matrix(kendall_pvalue_type,ncol=3,byrow = TRUE)

kendall_coef_00_10<-rast(kendall_coef_00_10)
kendall_pvalue_00_10<-rast(kendall_pvalue_00_10)
kendall_coef_11_20<-rast(kendall_coef_11_20)
kendall_pvalue_11_20<-rast(kendall_pvalue_11_20)
kendall_coef_00_20<-rast(kendall_coef_00_20)
kendall_pvalue_00_20<-rast(kendall_pvalue_00_20)

#classify
kendall_coef_00_10<-classify(kendall_coef_00_10,rcl_kendall_coef_type)
kendall_pvalue_00_10<-classify(kendall_pvalue_00_10,rcl_kendall_pvalue_type)
kendall_coef_11_20<-classify(kendall_coef_11_20,rcl_kendall_coef_type)
kendall_pvalue_11_20<-classify(kendall_pvalue_11_20,rcl_kendall_pvalue_type)
kendall_coef_00_20<-classify(kendall_coef_00_20,rcl_kendall_coef_type)
kendall_pvalue_00_20<-classify(kendall_pvalue_00_20,rcl_kendall_pvalue_type,include.lowest=TRUE)

kendall_00_10<-kendall_coef_00_10*kendall_pvalue_00_10
kendall_11_20<-kendall_coef_11_20*kendall_pvalue_11_20
kendall_00_20<-kendall_coef_00_20*kendall_pvalue_00_20 
#35 coef<0, P<0.05 Significant decreasing-resilience gains
#40 coef<0, P>0.05 Non-significant decreasing
#42 coef>0, P<0.05 Significant increasing-resilience loss
#48 coef>0, P>0.05 Non-significant increasing

#merge type 40 and 48
kendall_00_10[kendall_00_10 == 40] <- 48
kendall_11_20[kendall_11_20 == 40] <- 48
kendall_00_20[kendall_00_20 == 40] <- 48
#35 coef<0, P<0.05 Significant decreasing-resilience gains
#42 coef>0, P<0.05 Significant increasing-resilience loss
#48 ------, P>0.05 Non-significant trend
##--prepare the data kendall--##

#merge kNDVI data and kendall data
overlay_00_10<-kNDVI_00_10*kendall_00_10
overlay_11_20<-kNDVI_11_20*kendall_11_20
overlay_00_20<-kNDVI_00_20*kendall_00_20
#3*35 105 Significant browning and resilience gains
#3*42 126 Significant browning and resilience loss
#3*48 144 Significant browning and Non-significant trend
#4*35 140 Non-significant trend and resilience gains
#4*42 168 Non-significant trend and resilience loss
#4*48 192 Non-significant trend and Non-significant trend
#6*35 210 Significant greening and resilience gains
#6*42 252 Significant greening and resilience loss
#6*48 288 Significant greening and Non-significant trend

#convert raster data to dataframe for mapping purpose
overlay_00_10<-stack(overlay_00_10)
overlay_11_20<-stack(overlay_11_20)
overlay_00_20<-stack(overlay_00_20)

writeRaster(overlay_00_10,filename = "./overlay_00_10.tif")
writeRaster(overlay_11_20,filename = "./overlay_11_20.tif")
writeRaster(overlay_00_20,filename = "./overlay_00_20.tif")

overlay_00_10 <- as(overlay_00_10, "SpatialPixelsDataFrame")
overlay_00_10 <- as.data.frame(overlay_00_10) %>% 
  rename(type = slope)
overlay_11_20 <- as(overlay_11_20, "SpatialPixelsDataFrame")
overlay_11_20 <- as.data.frame(overlay_11_20) %>% 
  rename(type = slope)
overlay_11_20$type<-as.character(overlay_11_20$type)
overlay_00_20 <- as(overlay_00_20, "SpatialPixelsDataFrame")
overlay_00_20 <- as.data.frame(overlay_00_20) %>% 
  rename(type = slope)


##==== Draw ====##
#https://zhuanlan.zhihu.com/p/430107483 配色网站
library(ggsci) #package for color
bioclimatic_zonal <- readOGR("./Bioclimatic zonal/modi_bioclimatic_zonal.shp",layer = "modi_bioclimatic_zonal")
# fortify, i.e., make ggplot2-compatible
bioclimatic_zonal <- fortify(bioclimatic_zonal)
crs<-"+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=4000000 +y_0=0 +datum=WGS84
+units=m +no_defs"

Fig.4a <- ggplot()+
  geom_raster(data=overlay_00_20,aes(x = x,y = y,fill = factor(type)))+
  scale_color_npg()+
  scale_fill_npg()+
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
Fig.4a

Fig.4b <- ggplot()+
  geom_raster(data=overlay_00_10,aes(x = x,y = y,fill = factor(type)))+
  scale_color_npg()+
  scale_fill_npg()+
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
Fig.4b

Fig.4c <- ggplot()+
  geom_raster(data=overlay_11_20,aes(x = x,y = y,fill = factor(type)))+
  scale_color_npg()+
  scale_fill_npg()+
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
Fig.4c

export::graph2ppt(Fig.4a,file="./Fig.4a.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.4b,file="./Fig.4b.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.4c,file="./Fig.4c.pptx",width = 5,height = 4.5)


#draw Stacked Bar Chart
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

#prepare data
data_00_10<-as.data.frame(table(overlay_00_10$type))
data_11_20<-as.data.frame(table(overlay_11_20$type))
data_00_20<-as.data.frame(table(overlay_00_20$type))

all_data<-cbind(data_00_10[,1:2],data_11_20[,2],data_00_20[,2])
all_data<-all_data %>% rename(F2000_2010=Freq,F2011_2020='data_11_20[, 2]',F2000_2020='data_00_20[, 2]')
all_data$Var1<-as.character(all_data$Var1)

#convert data from wider to longer
all_data<-all_data %>% pivot_longer(.,-1,names_to = "periods",values_to = "Freq") 
#Change the order labels/ticks in the x-axis
all_data$periods <- factor(all_data$periods,levels = c("F2000_2020","F2000_2010","F2011_2020"))

#Vertical
Fig.4d<-ggplot(data=all_data,aes(periods,Freq,fill=Var1))+  
  geom_bar(stat="identity", position="fill",color="black", width=0.8,size=0.25)+
  scale_color_npg()+
  scale_fill_npg()+
  xlab("Periods") + ylab("Frequency (%)")+  
  theme_custom()+
  theme(aspect.ratio = 6/2)
Fig.4d

#Horizontal
ggplot(data=all_data,aes(periods,Freq,fill=Var1))+  
  geom_bar(stat="identity", position="fill",color="black", width=0.8,size=0.25)+
  coord_flip()+
  scale_color_npg()+
  scale_fill_npg()+
  xlab("Periods") + ylab("Frequency (%)")+  
  theme_custom()+
  theme(aspect.ratio = 2/6)

export::graph2ppt(Fig.4d,file="./Fig.4d.pptx",width = 5,height = 4.5)






