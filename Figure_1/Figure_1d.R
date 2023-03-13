rm(list = ls())
library(terra)
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##==== prepare data ====##
AR1_win60_kendall_2000_2010<-rast("./AR1_pixel_win60/2000_2010/AR1_win60_kendall_2000_2010.tif")
AR1_win60_kendall_2011_2020<-rast("./AR1_pixel_win60/2011_2020/AR1_win60_kendall_2011_2020.tif")
AR1_win60_kendall_2000_2020<-rast("./AR1_pixel_win60/2000_2020/AR1_win60_kendall.tif")

#load vegetation land for mask purpose
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#mask kendall tau by land use
all_coef_00_10<-mask(raster(AR1_win60_kendall_2000_2010[[1]]),vegetation_land)
all_pvalue_00_10<-mask(raster(AR1_win60_kendall_2000_2010[[2]]),vegetation_land)
all_coef_11_20<-mask(raster(AR1_win60_kendall_2011_2020[[1]]),vegetation_land)
all_pvalue_11_20<-mask(raster(AR1_win60_kendall_2011_2020[[2]]),vegetation_land)

#prepare all data
all_coef_00_10 <- as(all_coef_00_10, "SpatialPixelsDataFrame")
all_coef_00_10 <- as.data.frame(all_coef_00_10) %>% 
  rename(kendall_coef = kendall_coef)
all_pvalue_00_10 <- as(all_pvalue_00_10, "SpatialPixelsDataFrame")
all_pvalue_00_10 <- as.data.frame(all_pvalue_00_10) %>% 
  rename(kendall_pvalue = kendall_pvalue)
all_data_00_10<-cbind(all_coef_00_10[,2:3],all_coef_00_10[,1],all_pvalue_00_10[,1])
colnames(all_data_00_10) <- c('x','y','coef','p')
findSet1 <- which(is.na(all_data_00_10$p)|(all_data_00_10$p > 0.05))
all_data_00_10 <- all_data_00_10[-findSet1,]
all_data_00_10<-all_data_00_10[,3]

all_coef_11_20 <- as(all_coef_11_20, "SpatialPixelsDataFrame")
all_coef_11_20 <- as.data.frame(all_coef_11_20) %>% 
  rename(kendall_coef = kendall_coef)
all_pvalue_11_20 <- as(all_pvalue_11_20, "SpatialPixelsDataFrame")
all_pvalue_11_20 <- as.data.frame(all_pvalue_11_20) %>% 
  rename(kendall_pvalue = kendall_pvalue)
all_data_11_20<-cbind(all_coef_11_20[,2:3],all_coef_11_20[,1],all_pvalue_11_20[,1])
colnames(all_data_11_20) <- c('x','y','coef','p')
findSet2 <- which(is.na(all_data_11_20$p)|(all_data_11_20$p > 0.05))
all_data_11_20 <- all_data_11_20[-findSet2,]
all_data_11_20<-all_data_11_20[,3]

#statistics for annotations larger or smaller than zero
#vegetation land 401482
length(all_data_00_10) #306860
length(which(all_data_00_10>0)) #81198
81198/401482
(306860-81198)/401482

length(all_data_11_20) #318080
length(which(all_data_11_20>0)) #170446
170446/401482
(318080-170446)/401482

#combine data
all_data = data.frame(
  type = factor(c(rep("all_data_00_10",times=306860),
                  rep("all_data_11_20",times=318080))),
  value = c(all_data_00_10, all_data_11_20))

##==== draw ====##
Fig.1d<-ggplot(all_data,aes(value,fill=type,colour = type)) +
  geom_histogram(aes(y=..density..),bins = 120,colour = "white", 
                 alpha=0.4,
                 
                 #lwd = 0.75,
                 #linetype = 1,
                 position = "identity")+  #colour = "black",
  geom_density(alpha=.2)+
  scale_color_manual(values = c("#0A6A62","#925710"))+  #"#0A6A62","#925710"  #"#178f92","#845d29"
  scale_fill_manual(values = c("#0A6A62","#925710"))+
  geom_vline(xintercept=0,lty=5)+                       
  theme_few()+
  theme(axis.title.x = element_text(color = "black", size = 8, family = "Arial"),
        axis.title.y = element_text(color = "black", size = 8, family = "Arial"),
        axis.text = element_text(color = "black", size = 8, family = "Arial"),
        panel.border = element_rect(colour = "black", size=0.5),
        legend.position = 'none',
        aspect.ratio = 3/4
  )

Fig.1d

export::graph2ppt(Fig.1d,file="./Fig.1d.pptx",width = 5,height = 4.5)
