#HW9 PlotByTime<-function(time, count)

install.packages("ggplot2") #plot
library(ggplot2)
FinalDate<-AggregateByCase(dat$ARRESTTIME,dat$COUNCIL_DISTRICT)
Time<-gsub("-","",FinalDate$time)
plot(factor(Time), FinalDate$DistrCount, 
     col="blue", 
     lwd=3, 
     xlabel="Arrstdate", 
     ylabel="DistrCount", 
     main="Count_Crime", 
     type = "l")

install.packages("ggplot2") #ggplot
library(ggplot2)
qplot(Time,DistrCount,data=FinalDate,main="Count_Crime")
ggplot(FinalDate,aes(x=Time,y=FinalDate$DistrCount))+
  geom_point()+
  theme(axis.text.x = element_text(size=3,angle=90))

