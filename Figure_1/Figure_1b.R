rm(list = ls())
library(raster)
library(terra)
library(dplyr)
library(raster)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##==== prepare data ====##
kNDVI_trend_00_10<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2010.tif")
kNDVI_trend_11_20<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2011_2020.tif")

#load vegetation areas to mask kNDVI_trend
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#mask kNDVI trend by land use
kNDVI_slope_00_10<-mask(raster(kNDVI_trend_00_10[[1]]),vegetation_land)
pvalue_00_10<-mask(raster(kNDVI_trend_00_10[[3]]),vegetation_land)
kNDVI_slope_11_20<-mask(raster(kNDVI_trend_11_20[[1]]),vegetation_land)
pvalue_11_20<-mask(raster(kNDVI_trend_11_20[[3]]),vegetation_land)

#data 2000-2010
kNDVI_slope_00_10 <- as(kNDVI_slope_00_10, "SpatialPixelsDataFrame")
kNDVI_slope_00_10 <- as.data.frame(kNDVI_slope_00_10) %>% 
  rename(slope = slope)
pvalue_00_10 <- as(pvalue_00_10, "SpatialPixelsDataFrame")
pvalue_00_10 <- as.data.frame(pvalue_00_10) %>% 
  rename(MK_test = MK_test)
data_00_10<-cbind(kNDVI_slope_00_10[,2:3],kNDVI_slope_00_10[,1],pvalue_00_10[,1])
colnames(data_00_10) <- c('x','y','slope','MK_test')
findSet1 <- which(is.na(data_00_10$MK_test)|(data_00_10$MK_test > 0.05))
data_00_10 <- data_00_10[-findSet1,]
data_00_10<-data_00_10[,3]

#data 2011-2020
kNDVI_slope_11_20 <- as(kNDVI_slope_11_20, "SpatialPixelsDataFrame")
kNDVI_slope_11_20 <- as.data.frame(kNDVI_slope_11_20) %>% 
  rename(slope = slope)
pvalue_11_20 <- as(pvalue_11_20, "SpatialPixelsDataFrame")
pvalue_11_20 <- as.data.frame(pvalue_11_20) %>% 
  rename(MK_test = MK_test)
data_11_20<-cbind(kNDVI_slope_11_20[,2:3],kNDVI_slope_11_20[,1],pvalue_11_20[,1])
colnames(data_11_20) <- c('x','y','slope','MK_test')
findSet2 <- which(is.na(data_11_20$MK_test)|(data_11_20$MK_test > 0.05))
data_11_20 <- data_11_20[-findSet2,]
data_11_20<-data_11_20[,3]

#statistics for annotations larger or smaller than zero
length(data_00_10) #190093
length(which(data_00_10>0)) #189442

length(data_11_20) #81336
length(which(data_11_20>0)) #78643


#merge data
alldata = data.frame(
  type = factor(c(rep("data_00_10",times=190093),rep("data_11_20",times=81336))),
  value = c(data_00_10, data_11_20)
)

mean <- alldata %>% 
  group_by(type) %>%
  summarise(grp.median = median(value))
mean
##==== prepare data ====##

##==== Draw ====##
Fig.1b <- ggplot(alldata, aes(x = value))+
  geom_density(aes(fill = type), position = "identity",alpha = 0.5) +
  geom_vline(aes(xintercept = grp.median, color = type), data = mean, linetype = "dashed") +
  geom_vline(aes(xintercept = 0),linetype = "dashed") +
  scale_color_manual(values = c("#156077","#f46f20"))+  #"#868686FF", "#EFC000FF"
  scale_fill_manual(values = c("#156077","#f46f20"))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
        plot.title=element_text(size = 10),
        text=element_text(size = 10),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.position = "none",
        aspect.ratio = 3/4
  )  #aspect.ratio = 3/6.5

Fig.1b

export::graph2ppt(greening,file="./Fig.1b.pptx",width = 5,height = 4.5)
