#HW9 PlotByTime<-function(time, count)

install.packages("ggplot2") #ggplot
library(ggplot2)
PlotbyTime<-function(time,count){
  dat<-data.frame(time,count)
  ggplot(dat, aes(x=time, y=count))+
    geom_point()+
    theme(axis.text.x = element_text(size=6, angle=90))+
    ggtitle("Number of Council Districts Involved Across Time")+
    labs(x="Year",y="Number of Council Districts")
}

