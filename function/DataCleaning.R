# HW9-ArrestData. Kexin_Ziqian_Hantian
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
  dat<-data.frame(date,newCouncil_District)
  count<-aggregate(dat$newCouncil_District,list(dat$date),lenunique)   # Count the numbers of districts by given days. 
  names(count)[1:2]<-c("time","DistrCount")
  return(count)
}

