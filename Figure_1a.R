rm(list = ls())
library(raster)
library(terra)
library(ggplot2)
library(export)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

##====theme function====##
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(5,5,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(size = 0.2),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.title.y = element_text(size = 10.5,margin = margin(0,3,0,0),face = 'bold',family = 'Times'),
                   axis.title.x = element_text(size = 10.5,margin = margin(4,0,0,0),face = 'bold',family = 'Times'),
                   axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
                   axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'))
  return(myTheme)
}
##====theme function====##

##==== prepare data ====##
#load kNDVI data
kNDVI_yearly<-rast("./kNDVI_yearly_2000_2020/kNDVI_yearly_2000_2020.tif")
kNDVI_yearly<-stack(kNDVI_yearly)

#load vegetation areas to mask kNDVI
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#kNDVI mean value from 2000 to 2020
kNDVI_mean_2000_2020<-data.frame()
for (i in 1:21){
  mask<-mask(kNDVI_yearly[[i]],vegetation_land)
  mean_val<-cellStats(mask,mean)
  kNDVI_mean_2000_2020[i,1]<-mean_val
  print(i)
}

kNDVI_mean_2000_2020$Year<-2000:2020
colnames(kNDVI_mean_2000_2020) <- c("value", "year")
kNDVI_mean_2000_2020<-kNDVI_mean_2000_2020[,c(2,1)]
##==== prepare data ====##

##==== Draw ====##
Fig.1a<-ggplot() + 
  geom_point(data=kNDVI_mean_2000_2020,aes(x=year, y=value),size=2,color="#00adb5")+  #alpha #f9a828
  geom_smooth(data=kNDVI_mean_2000_2020,aes(x=year, y=value),method = "glm", color = "black", size = 0.5)+
  scale_y_continuous(
    name = "Annual mean kNDVI"
  )+
  theme_custom()+
  theme(aspect.ratio = 3/4)

Fig.1a

export::graph2ppt(Fig.1a,file="./Fig.1a.pptx")

#Annotation
summary(lm(value~year,data=kNDVI_mean_2000_2020))

