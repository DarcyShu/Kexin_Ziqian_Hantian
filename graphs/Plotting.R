#HW9 PlotByTime<-function(time, count)

install.packages("ggplot2") #ggplot
library(ggplot2)
<<<<<<< HEAD
FinalDate<-AggregateByCase(dat$ARRESTTIME,dat$COUNCIL_DISTRICT)
qplot(x,y,data=,main="")
qplot(Time,DistrCount,data=FinalDate,main="Count_Crime")
ggplot(FinalDate,aes(x=Time,y=FinalDate$DistrCount))+
  geom_point()+
  theme(axis.text.x = element_text(size=3,angle=90))
=======
PlotbyTime<-function(time,count){
  dat<-data.frame(time,count)
  ggplot(dat, aes(x=time, y=count))+
    geom_point()+
    theme(axis.text.x = element_text(size=6, angle=90))+
    ggtitle("Number of Council Districts Involved Across Time")+
    labs(x="Year",y="Number of Council Districts")
}
>>>>>>> c0e32a4401708dcec172b4378d2dc3afa27f3fba

