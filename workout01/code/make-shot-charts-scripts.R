#-------------------------------------
#title: "make-shot-chart-script"
#output: "make-shot-chart-script.R"
#Author: "Jane Zhang"
#width = 6.5, height = 5 inches
#-------------------------------------
library(jpeg)
library(grid)
library(ggplot2)
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(readJPEG(court_file),
              width=unit(1,"npc"),
              height = unit(1,"npc"))

pdf(file="../images/klay-thompson-shot-charts.pdf",width=6.5,height=5)
ggplot(data=klay) +
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle("Shot chart-Klay")+theme_minimal()
dev.off()                      


pdf(file="../images/andre-iguodala-shot-charts.pdf",width=6.5,height=5)
ggplot(data=andre) +
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle("Shot chart-Andre")+theme_minimal()
dev.off()                      



pdf(file="../images/kevin-durant-shot-charts.pdf",width=6.5,height=5)
ggplot(data=kevin) +
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle("Shot chart-Kevin")+theme_minimal()
dev.off()                      


pdf(file="../images/draymond-green-shot-charts.pdf",width=6.5,height=5)
ggplot(data=graymond) +
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle("Shot chart-Draymond")+theme_minimal()
dev.off()                      


pdf(file="../images/stephen-curry-shot-charts.pdf",width=6.5,height=5)
ggplot(data=stephen) +
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle("Shot chart-Stephen")+theme_minimal()
dev.off()                      


pdf(file="../images/gsw-shot-chars.pdf",width=8,height=7)
ggplot(data=combine) +
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag),size = 1)+ylim(-50,420)+
  ggtitle("Shot chart: GSW (2016 season)")+facet_wrap(~name)+theme_minimal()
dev.off()






