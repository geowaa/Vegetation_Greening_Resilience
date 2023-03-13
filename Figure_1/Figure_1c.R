rm(list = ls())
library(terra)
library(raster)
library(tsbox)
#provide ts_df()
library(ggplot2)
library(lubridate)
#lubridate::make_date(year, month, day)
library(ggthemes)
#provide theme_few()
#ts_ggplot https://www.tsbox.help/reference/ts_ggplot.html
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
AR1_pixel_win60<-rast("./AR1_pixel_win60/2000_2020/AR1_pixel_win60.tif")
AR1_pixel_win60_2000_2010<-rast("./AR1_pixel_win60/2000_2010/AR1_pixel_win60_2000_2010.tif")
AR1_pixel_win60_2011_2020<-rast("./AR1_pixel_win60/2011_2020/AR1_pixel_win60_2011_2020.tif")
AR1_pixel_win60<-stack(AR1_pixel_win60)
AR1_pixel_win60_2000_2010<-stack(AR1_pixel_win60_2000_2010)
AR1_pixel_win60_2011_2020<-stack(AR1_pixel_win60_2011_2020)

#load vegetation areas to mask AR1_pixel_win60
vegetation_land<-raster("./Vegetation areas/vegetation_land.tif")

#Calculate AR1 mean value from 2000 to 2020  
AR1_pixel_win60_mean<-data.frame()
for (i in 1:192){
  mask<-mask(AR1_pixel_win60[[i]],vegetation_land)
  mean_val<-cellStats(mask,mean)
  AR1_pixel_win60_mean[i,1]<-mean_val
  print(i)
}

#convert to ts object 2000.2-2005.1 60 months, the date for starting a time window
AR1_pixel_win60_mean<-ts(AR1_pixel_win60_mean,start=c(2005,1), frequency=12)
AR1_pixel_win60_mean<-ts_df(ts_c(AR1_pixel_win60_mean=AR1_pixel_win60_mean))

#Breakpoints
library(bfast)
bk.data<-ts(AR1_pixel_win60_mean[,2],start = c(2005,1),frequency = 12)
breakpoints<-bfast(bk.data,h=0.15,season=c("none"),breaks=1)
breakpoints
#Corresponding to breakdates:
#  2.5 %    breakpoints 97.5 % 
#  1 2010(11) 2010(12)    2011(1)

##==== prepare data ====##

##==== Draw ====##
Fig.1c<-ggplot(AR1_pixel_win60_mean) +
  geom_rect(aes(xmin=make_date(2005,1), xmax=make_date(2010,12), ymin=-Inf, ymax=Inf),fill='#B5CFD8',alpha = .3)+ #eaf6f6 #d3d4d8 #8f8787 #ebcbae
  geom_rect(aes(xmin=make_date(2010,12), xmax=make_date(2020,12), ymin=-Inf, ymax=Inf),fill='#E8ECF1',alpha = .3)+
  geom_line(mapping=aes(x=time,y=value),size=0.8,color="#ffb400")+
  labs(x="Time",y="Mean AR(1)")+
  theme_tsbox() +
  scale_color_tsbox()+
  theme_custom()+
  theme(aspect.ratio = 3/4,
        legend.position="none")

Fig.1c

export::graph2ppt(Fig.3a,file="./Fig.1c.pptx",width = 5,height = 4.5)
##==== Draw ====##

#Annotation
mean_p1<-AR1_pixel_win60_mean[1:72,2]
timevec1<-c(1:72)
mean_p2<-AR1_pixel_win60_mean[73:192,2]
timevec2<-c(1:120)

kendall1<-cor.test(timevec1,mean_p1,alternative=c("two.sided"),
                   method=c("kendall"),conf.level=0.95)
#tau=-0.952 p-value<2.2e-16
kendall2<-cor.test(timevec2,mean_p2,alternative=c("two.sided"),
                   method=c("kendall"),conf.level=0.95)
#tau=0.863 p-value<2.2e-16
