rm(list = ls())
library(terra)
library(sf)
library(terra)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(agricolae)
library(export)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##==== Function ====##
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
##==== Function ====##

##==== prepare data ====##
#load kNDVI sen+MK
kNDVI_trend_2000_2010<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2010.tif")
kNDVI_trend_2011_2020<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2011_2020.tif")
kNDVI_trend_2000_2020<-rast("./Greening_kNDVI_trend/kNDVI_sen_mk2000_2020.tif")
kNDVI_trend_2000_2010<-stack(kNDVI_trend_2000_2010)
kNDVI_trend_2011_2020<-stack(kNDVI_trend_2011_2020)
kNDVI_trend_2000_2020<-stack(kNDVI_trend_2000_2020)
#load bioclimatic_zonal
bioclimatic_zonal<-raster("./Bioclimatic zonal/modi_bioclimatic_zonal.tif")

#load vegetation land for mask purpose
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#mask kNDVI trend and bioclimatic_zonal by vegetation areas
coef_00_10<-mask(kNDVI_trend_2000_2010[[1]],vegetation_land)
pvalue_00_10<-mask(kNDVI_trend_2000_2010[[3]],vegetation_land)
coef_11_20<-mask(kNDVI_trend_2011_2020[[1]],vegetation_land)
pvalue_11_20<-mask(kNDVI_trend_2011_2020[[3]],vegetation_land)
coef_00_20<-mask(kNDVI_trend_2000_2020[[1]],vegetation_land)
pvalue_00_20<-mask(kNDVI_trend_2000_2020[[3]],vegetation_land)

#mask kNDVI trend by bioclimatic zonal for scope purpose
coef_00_10<-mask(coef_00_10,bioclimatic_zonal)
pvalue_00_10<-mask(pvalue_00_10,bioclimatic_zonal)
coef_11_20<-mask(coef_11_20,bioclimatic_zonal)
pvalue_11_20<-mask(pvalue_11_20,bioclimatic_zonal)
coef_00_20<-mask(coef_00_20,bioclimatic_zonal)
pvalue_00_20<-mask(pvalue_00_20,bioclimatic_zonal)

bioclimatic_zonal<-mask(bioclimatic_zonal,vegetation_land)
bioclimatic_zonal <- as(bioclimatic_zonal, "SpatialPixelsDataFrame")
bioclimatic_zonal <- as.data.frame(bioclimatic_zonal) %>% 
  rename(zone = modi_bioclimatic_zonal)

#data 2000-2010
coef_00_10 <- as(coef_00_10, "SpatialPixelsDataFrame")
pvalue_00_10 <- as(pvalue_00_10, "SpatialPixelsDataFrame")
coef_00_10 <- as.data.frame(coef_00_10) %>% 
  rename(slope = slope)
pvalue_00_10 <- as.data.frame(pvalue_00_10) %>% 
  rename(MK_test = MK_test)
cbind_data<-cbind(coef_00_10[,2:3],coef_00_10[,1],pvalue_00_10[,1],bioclimatic_zonal[,1])
colnames(cbind_data) <- c('x','y','coef','p','zone')
findset1<-which(cbind_data$p > 0.05)
greening_00_10<-cbind_data[-findset1,]
table(greening_00_10[,5])
#1     2     3     4 
#36603 75197 50066 28068

#data 2011-2020
coef_11_20 <- as(coef_11_20, "SpatialPixelsDataFrame")
pvalue_11_20 <- as(pvalue_11_20, "SpatialPixelsDataFrame")
coef_11_20 <- as.data.frame(coef_11_20) %>% 
  rename(slope = slope)
pvalue_11_20 <- as.data.frame(pvalue_11_20) %>% 
  rename(MK_test = MK_test)
cbind_data2<-cbind(coef_11_20[,2:3],coef_11_20[,1],pvalue_11_20[,1],bioclimatic_zonal[,1])
colnames(cbind_data2) <- c('x','y','coef','p','zone')
findset2<-which(cbind_data2$p > 0.05)
greening_11_20<-cbind_data2[-findset2,]
table(greening_11_20[,5])
#1     2     3     4 
#10481 18359 33044 19351 

#data 2000-2020
coef_00_20 <- as(coef_00_20, "SpatialPixelsDataFrame")
pvalue_00_20 <- as(pvalue_00_20, "SpatialPixelsDataFrame")
coef_00_20 <- as.data.frame(coef_00_20) %>% 
  rename(slope = slope)
pvalue_00_20 <- as.data.frame(pvalue_00_20) %>% 
  rename(MK_test = MK_test)
cbind_data3<-cbind(coef_00_20[,2:3],coef_00_20[,1],pvalue_00_20[,1],bioclimatic_zonal[,1])
colnames(cbind_data3) <- c('x','y','coef','p','zone')
findset3<-which(cbind_data3$p > 0.05)
greening_00_20<-cbind_data3[-findset3,]
table(greening_00_20[,5])
#1      2      3      4 
#56714 102916 102960  80240 

#comparison between groups
model_00_10<-aov(coef~zone,data=greening_00_10)
SNK.test(model_00_10,"zone", console=TRUE, 
                main="Comparison for data greening_00_10")
model_11_20<-aov(coef~zone,data=greening_11_20)
SNK.test(model_11_20,"zone", console=TRUE, 
         main="Comparison for data greening_11_20")
model_00_20<-aov(coef~zone,data=greening_00_20)
SNK.test(model_00_20,"zone", console=TRUE, 
         main="Comparison for data greening_00_20")

#tranform the variable 'zone' from num type to factor type
greening_00_10$zone<-as.character(greening_00_10$zone)
greening_11_20$zone<-as.character(greening_11_20$zone)
greening_00_20$zone<-as.character(greening_00_20$zone)
##==== prepare data ====##

##==== Draw ====##
Fig.3a<-ggplot(data = greening_00_20,mapping = aes(x = zone,y = coef))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_violin(mapping = aes(fill = zone),width = 0.7,size = 0.4,alpha = 0.6)+
  geom_boxplot(mapping = aes(fill = zone),outlier.shape = NA,width = 0.3,size = 0.3)+
  scale_fill_manual(values = c('#028501','#009985','#7ed4c9','#ffc653'))+
  scale_x_discrete(labels = c("FOR", "FOR-GRASS","GRASS","DES-GRASS"))+
  ylab("Greening trend")+
  xlab("zone")+
  theme_custom()+
  theme(aspect.ratio = 3/4)
Fig.3a

Fig.3b<-ggplot(data = greening_00_10,mapping = aes(x = zone,y = coef))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_violin(mapping = aes(fill = zone),width = 0.7,size = 0.4,alpha = 0.6)+
  geom_boxplot(mapping = aes(fill = zone),outlier.shape = NA,width = 0.3,size = 0.3)+
  scale_fill_manual(values = c('#028501','#009985','#7ed4c9','#ffc653'))+
  scale_x_discrete(labels = c("FOR", "FOR-GRASS","GRASS","DES-GRASS"))+
  ylab("Greening trend")+
  xlab("zone")+
  theme_custom()+
  theme(aspect.ratio = 3/4)
Fig.3b

Fig.3c<-ggplot(data = greening_11_20,mapping = aes(x = zone,y = coef))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_violin(mapping = aes(fill = zone),width = 0.7,size = 0.4,alpha = 0.6)+
  geom_boxplot(mapping = aes(fill = zone),outlier.shape = NA,width = 0.3,size = 0.3)+
  scale_fill_manual(values = c('#028501','#009985','#7ed4c9','#ffc653'))+
  scale_x_discrete(labels = c("FOR", "FOR-GRASS","GRASS","DES-GRASS"))+
  ylab("Greening trend")+
  xlab("zone")+
  theme_custom()+
  theme(aspect.ratio = 3/4)
Fig.3c

export::graph2ppt(Fig.3a,file="./Fig.3a.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.3b,file="./Fig.3b.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.3c,file="./Fig.3c.pptx",width = 5,height = 4.5)

