# HW9-Main Script: Kexin_Ziqian_Hantian
rm(list=ls())
# Reading a Rdata file from Github. 
install.packages("repmis")
library(repmis)
dat<-source_data("https://github.com/DarcyShu/Kexin_Ziqian_Hantian/raw/master/HW9-ArrestMini.csv")
# Build the length unique function.
lenunique<-function(x){
  length(unique(x))
}
# Aggregate function.
AggregateByCase<-function(group,x){
  idx<-which(is.na(x))
  newCouncil_District<-x[-idx]      # Get rid of all NAs.
  newArrestTime<-group[-idx]
  time1<-as.character(newArrestTime)
  time2<-strsplit(time1,"T")        # Split the date by "T".
  time3<-lapply(time2,"[[",1)       
  time4<-unlist(time3)
  date<-as.Date(time4,"%Y-%m-%d")   # Convert the format of dates into Rstudio format. 
  date<-as.factor(date)
  dat<-data.frame(date,newCouncil_District)
  count<-aggregate(dat$newCouncil_District,list(dat$date),lenunique)   # Count the numbers of districts by given days. 
  names(count)[1:2]<-c("time","DistrCount")
  return(count)
}
FinalDate<-AggregateByCase(dat$ARRESTTIME,dat$COUNCIL_DISTRICT)

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

PlotbyTime(FinalDate$time,FinalDate$DistrCount)



