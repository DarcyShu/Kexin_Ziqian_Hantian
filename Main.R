#HW10--Kexin_Ziqian_Hantian
rm(list=ls())
install.packages("repmis")
library(repmis)
dat<-source_data("https://github.com/DarcyShu/Kexin_Ziqian_Hantian/raw/master/ArrestData.csv")

CleanOffense<-function(x,frame){
  offense<-strsplit(as.character(x),"/") 
  mainoffense<-sapply(offense,"[[",1) 
  First_Offenses<-mainoffense 
  matches<-gregexpr("[0-9]+",x) 
  FullCode<-regmatches(x,matches) 
  FullCode<-sapply(FullCode,"[[",1) 
  frame<-cbind(frame,FullCode,stringsAsFactors=F) 
  FullCode[which(nchar(FullCode) == 2)]<-"13(a)" 
  FullCode[which(nchar(FullCode) == 4)]<-substr( FullCode[which(nchar(FullCode) == 4)],1,2) 
  FullCode[which(nchar( FullCode) == 3)]<-substr( FullCode[which(nchar( FullCode) == 3)],1,1) 
  ShortCode<-as.character(FullCode) 
  frame<-cbind(frame,ShortCode,stringsAsFactors = FALSE) 
  frame<-na.omit(frame) 
  return(frame) 
} 

frame<-CleanOffense(dat$OFFENSES,dat)

#String Split the time and save the time and date as identified time variable in R envrionment 
library(lubridate)
CleanTime<-function(x,frame){
  time_date<-sub("T","",x)
  date<-as.POSIXct(time_date)
  day<-weekdays(date)
  hour<-hour(date)
  month<-month(date)
  frame2<-data.frame(frame,day,hour,month)
  return(frame2)
}
frame2<-CleanTime(frame$ARRESTTIME,frame)

weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
temp<-NULL
week<-NULL
time<-NULL
for (i in weekday) {
  for (z in 1:24){
  CountOffense<-length(frame2$OFFENSES[which(frame2$day == i & frame2$hour == z)])
  week<-c(week,i)
  time<-c(time,z)
  temp<-c(temp,CountOffense)
  }
} 
newdata<-data.frame(week,time,temp)
library(ggplot2)
PlotbyTime<-function(week,hourpoint,arrest){
  dat<-data.frame(week,hourpoint,arrest)
  ggplot(dat, aes(x=week, y=hourpoint))+
    geom_raster(aes(fill=arrest),hjust=0.5,vjust=0.5,interpolate=FALSE)+
    theme(axis.text.x = element_text(size=6, angle=90))+
    ggtitle("Number of of Police Arrests in Pittsburgh from 2014-2017, by Time of Arrest")+
    labs(x="Hour of Arrest (Local Time)",y="Day of Week of Arrest Happened")
}
PlotbyTime(newdata$week,newdata$time,newdata$temp)

#Set function to filter the dataframe by each case 
FilterByCase<-function(frame,x,z){
  dat<-filter(frame,x == z)
  return(dat)
}
dat2<-FilterByCase(frame2,frame2$ShortCode,"13(a)")
dat3<-FilterByCase(frame2,frame2$ShortCode,"27")
dat4<-FilterByCase(frame2,frame2$ShortCode,"39")

#Set function to graph the frequncy of crime in each month for specific type of crime
GraphByMonth<-function(x,y,z,h){
  A<-tapply(y,x,length)
  ans<-barplot(A,xlab = z, main = h)
  return(ans)
}

GraphByMonth(dat2$month,dat2$OFFENSES,"month","Frequency of Crime in Each Month for 13(a)")
GraphByMonth(dat3$month,dat3$OFFENSES,"month","Frequency of Crime in Each Month for 27")
GraphByMonth(dat3$month,dat3$OFFENSES,"month","Frequency of Crime in Each Month for 39")


 





