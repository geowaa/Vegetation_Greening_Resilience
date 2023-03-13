rm(list = ls())
library(reshape2)
library(ggsci)
library(RColorBrewer)
library(ggplot2)
setwd("E:/Article writing/3 Vegetation greening and resilience/Relevant code/Figure_code/Datasets")

####----prepare data----####
dat00_20<-read.csv("./Overlay_greening_resilience/2000_2020.csv",
                header=TRUE) #row.names = 1
dat00_10<-read.csv("./Overlay_greening_resilience/2000_2010.csv",
                   header=TRUE)
dat11_20<-read.csv("./Overlay_greening_resilience/2011_2020.csv",
                   header=TRUE)

dat00_20<-melt(dat00_20)
dat00_10<-melt(dat00_10)
dat11_20<-melt(dat11_20)

dat00_20$X <- factor(dat00_20$X,levels=c("Non-significant trend and Non-significant trend",
                                         "Non-significant trend and resilience loss",
                                         "Non-significant trend and resilience gains",
                                         "Significant browning and Non-significant trend",
                                         "Significant greening and Non-significant trend",
                                         "Significant browning and resilience loss",
                                         "Significant browning and resilience gains",
                                         "Significant greening and resilience loss",
                                         "Significant greening and resilience gains"))
dat00_10$X <- factor(dat00_10$X,levels=c("Non-significant trend and Non-significant trend",
                                         "Non-significant trend and resilience loss",
                                         "Non-significant trend and resilience gains",
                                         "Significant browning and Non-significant trend",
                                         "Significant greening and Non-significant trend",
                                         "Significant browning and resilience loss",
                                         "Significant browning and resilience gains",
                                         "Significant greening and resilience loss",
                                         "Significant greening and resilience gains"))
dat11_20$X <- factor(dat11_20$X,levels=c("Non-significant trend and Non-significant trend",
                                         "Non-significant trend and resilience loss",
                                         "Non-significant trend and resilience gains",
                                         "Significant browning and Non-significant trend",
                                         "Significant greening and Non-significant trend",
                                         "Significant browning and resilience loss",
                                         "Significant browning and resilience gains",
                                         "Significant greening and resilience loss",
                                         "Significant greening and resilience gains"))

####----draw----####
Fig.5a<-ggplot()+
  geom_tile(data=dat00_20,
            aes(x=variable,y=X,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "YlGnBu",direction = 1)+
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
Fig.5a
  
Fig.5b<-ggplot()+
  geom_tile(data=dat00_10,
            aes(x=variable,y=X,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "YlGnBu",direction = 1)+
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
Fig.5b

Fig.5c<-ggplot()+
  geom_tile(data=dat11_20,
            aes(x=variable,y=X,fill=value),
            height=1,
            width=1,
            color="white")+
  scale_fill_distiller(palette = "YlGnBu",direction = 1)+
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
Fig.5c

export::graph2ppt(Fig.5a,file="./Fig.5a.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.5b,file="./Fig.5b.pptx",width = 5,height = 4.5)
export::graph2ppt(Fig.5c,file="./Fig.5c.pptx",width = 5,height = 4.5)

