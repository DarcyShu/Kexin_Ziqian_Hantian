#HW11-Kexin_Ziqian_Hantian
rm(list=ls())

# Reading a Rdata file from Github. 
install.packages("repmis")
library(repmis)
dat<-source_data("https://github.com/DarcyShu/ArrestData-Whole-Group/raw/master/Group2/ArrestData.csv")
#27red, 13(a) blue,39 purple,9 orange

dat <- read.csv("ArrestData(1).csv")

#datacleaning funnction to make a new data frame with the first offenses in each row 
#and the short code for corresponding offenses
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
  return(frame) 
} 

frame<-CleanOffense(dat$OFFENSES,dat)
#test the data 
sort(table(frame$ShortCode))

#String Split the time and save the time and date as identified time variable in R envrionment 
install.packages("lubridate")
library(lubridate)
CleanTime<-function(x,frame){
  # x<-dat$ARRESTTIME
  # frame<-dat
  
  time_date<-as.character(x)
  time_date<-sub("T"," ",x)
  date<-as.POSIXlt(time_date,format = "%Y-%m-%d %H:%M:%S") # set format explicitly
  day<-weekdays(date)
  hour<-hour(date)
  month<-month(date)
  year<-year(date)
  quarter<-quarter(date)
  frame2<-data.frame(frame,days=day,hour=hour,month=month,year=year,quarter=quarter,date=date)
  frame2<-frame2[which(frame2$year > 2015),]
  idx<-which(frame2$quarter == 1 & frame2$year == 2017)
  frame2$quarter[idx]<-5
  return(frame2)
}

frame2<-CleanTime(frame$ARRESTTIME,frame)

#Set the function to make the dataframe with date and count of the arrest in that date 
CasebyDate<-function(x,y){
  newArrestTime<-strsplit((as.character(x)),"T")
  newdate<-sapply(newArrestTime,"[[",1)
  newdate<-as.Date(newdate, "%Y-%m-%d")
  newdate<-as.factor(newdate)
  frame3<-aggregate(y,list(newdate),length)
  names(frame3)[1:2]<-c("date","CountOfArrestment")
  return(frame3)
  }

frame3<-CasebyDate(frame2$ARRESTTIME,frame2$OFFENSES)

###############Calendar Graph#############################
#set the function to make calendar heatmap 
install.packages("lattice")
install.packages("grid")
install.packages("chron")
calendarHeat <- function(dates, 
                         values, 
                         ncolors=99, 
                         color="b2w", 
                         varname="Values",
                         date.form = "%Y-%m-%d", ...) {
  require(lattice)
  require(grid)
  require(chron)
  if (class(dates) == "character" | class(dates) == "factor" ) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"),
                            "-1-1",sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"),
                            "-12-31", sep = ""))
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
  
  # Merge moves data by one day
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates) 
  caldat$value[match(dates, caldat$date.seq)] <- values
  
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()                        
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)  
    sub.seq <- seq(1,length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }  
  caldat <- cbind(caldat, seq=d.loc)
  
  #color styles
  r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
  r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
  b2w <- c("#F1EEF6","#BDC9E1","#74A9CF","#2B8CBE","#045A8D")   #blue to white
  
  assign("col.sty", get(color))
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  def.theme <- lattice.getOption("default.theme")
  cal.theme <-
    function() {  
      theme <-
        list(
          strip.background = list(col = "transparent"),
          strip.border = list(col = "transparent"),
          axis.line = list(col="transparent"),
          par.strip.text=list(cex=0.8))
    }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                              as.table=TRUE,
                              aspect=.12,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE,
                              main = paste("Calendar Heat Map of ", varname, sep = ""),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 0.7),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.6,
                                  tck=0)),
                              xlim =c(0.4, 54.6),
                              ylim=c(6.6,-0.6),
                              cuts= ncolors - 1,
                              col.regions = (calendar.pal(ncolors)),
                              xlab="" ,
                              ylab="",
                              colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                              subscripts=TRUE
  ) )
  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs))  {
      if (panel.locs[row, column] > 0)
      {
        trellis.focus("panel", row = row, column = column,
                      highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          } else {
            x.start <- adj.start - 0.5
          }
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          } else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start <  2) {
          grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1), 
                       y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                       gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5), 
                     y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1),
                     y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1),
                     y = c(y.start - 0.5 , -0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start),
                     y = c(y.start - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6  ) {
            grid.lines(x = c(x.start + 1, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start),
                     y = c( - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6  ) {
            grid.lines(x = c(x.start, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12)  {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                     default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
      }
    }
    trellis.unfocus()
  } 
  lattice.options(default.theme = def.theme)
}

#Plot calendar heatmap for the Arrestments from 2016 to 2017
calendarHeat(frame3$date,frame3$CountOfArrestment,ncolors = 99,color="b2w",varname = "Arrestment",data.form = "%Y-%m-%d") 
#####################################################################

#Loop to make dataframe with 4 types of offenses, Year_Month and corrsponding count of offenses 
Month_Type<-function(Offenses,Y,x,y,j,k){
#Offenses<-c("27","13(a)","9","39")
#Y<-c(2016,2017)
CountOffense<-NULL
OffensesType<-NULL
Month<-NULL
Year<-NULL
for (i in Offenses) {
  for (h in Y) {
    for (z in 1:12) {
    temp<-length(x[which(y == i &  j == z &  k == h)])
    OffensesType<-c(OffensesType,i)
    Month<-c(Month,z)
    Year<-c(Year,h)
    CountOffense<-c(CountOffense,temp)
    Year_Month<-paste(Year,Month,sep = "-")
    frame4<-data.frame(OffensesType,Year_Month,CountOffense,Year,Month)
      }
    }
  }
return(frame4)
}


Offenses<-c("27","13(a)","9","39")
Y<-c(2016,2017)
#x<-frame2$OFFENSES
#y<-frame2$ShortCode
#j<-frame2$month
#k<-frame2$year
frame4<-Month_Type(Offenses,Y,frame2$OFFENSES,frame2$ShortCode,frame2$month,frame2$year)
frame4<-frame4[-(which(frame4$Year == 2017 & frame4$Month > 3)),]
frame4<-frame4[1:3]

#############Create the Bar Plot by ggplot###########################
#Create the bar plot in R.
library(ggplot2)
# Dodged bar charts
#Create a new time level.
c<-as.character(frame4$Year_Month)
levels(c)
Order<-c("2016-1","2016-2","2016-3","2016-4","2016-5","2016-6","2016-7","2016-8","2016-9","2016-10", 
         "2016-11","2016-12","2017-1","2017-2","2017-3")
frame4$Year_Month<-factor(frame4$Year_Month,levels=Order,ordered = T)
levels(frame4$Year_Month)
frame4$OffensesType<-as.character(frame4$OffensesType)
levels(frame4$OffensesType)

frame4$OffensesType[frame4$OffensesType=="13(a)"]<-"Controlled Substance Possesion"
frame4$OffensesType[frame4$OffensesType=="27"]<-"Assault"
frame4$OffensesType[frame4$OffensesType=="39"]<-"Theft"
frame4$OffensesType[frame4$OffensesType=="9"]<-"Inchoate Crimes"

#Create the graph
ggplot(frame4, aes(x=Year_Month, y=CountOffense, fill=OffensesType)) + 
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x=element_text(size=10,angle=45))+
  scale_fill_manual(values = c("brown1","deepskyblue1","darkorange1","darkslateblue"))+
  geom_text(aes(label=CountOffense), position=position_dodge(width=0.9), vjust=-0.25, size=3)
# almost right! Just keep trying
#geom_text(aes(label=CountOffense),vjust=1.6, color="black",position = position_dodge(0.9),size=3.5)
  
#####################################################################
#Loop to make a new dataframe with the weekday, hour and the number of offenses in each specific time point 
weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
Offenses<-c("27","13(a)","9","39")
CountOffense<-NULL
week<-NULL
time<-NULL
q<-NULL
OffensesType<-NULL
for (k in Offenses) {
  for ( h in 1:5 ){
    for (i in weekday){
      for (z in 1:24) {
    temp<-length(frame2$OFFENSES[which(frame2$day == i & frame2$hour == z & frame2$quarter == h & frame2$ShortCode == k)])
    week<-c(week,i)
    time<-c(time,z)
    q<-c(q,h)
    CountOffense<-c(CountOffense,temp)
    OffensesType<-c(OffensesType,k)
      }
    }
  }
}

frame5<-data.frame(OffensesType,q,week,time,CountOffense)

# Change the format of data frame that sperates 5 quaters 
idx<-which(frame5$q == 1)
Count1<-frame5$CountOffense[idx]
Weekday<-frame5$week[idx]
Time<-frame5$time[idx]
Type<-frame5$OffensesType[idx]
frame6<-data.frame(Type,Weekday,Time,Count1)
frame7<-matrix(NA,672,4)
k<-1
for (i in 2:5) {
  idx<-which(frame5$q == i)
  temp<-frame5$CountOffense[idx]
  frame7[,k]<-temp
  k<-k+1
}
frame8<-cbind(frame6,frame7)
names(frame8)[5:8]<-c("Count2","Count3","Count4","Count5")

#test the distribution of each weekdays in each quarter 
Time_Series<-function(dates){
  if (class(dates) == "character" | class(dates) == "factor" ) {
  dates<-strptime(dates, "%Y-%m-%d")
}
caldat<-dates
min.date<-as.Date(paste(format(min(dates), "%Y"),
                          "-1-1",sep = ""))
max.date<-as.Date(paste(format(max(dates), "%Y"),
                          "-12-31", sep = ""))
caldat2<-seq(min.date, max.date, by="days")
weekday<-weekdays(caldat2)
quarter<-quarter(caldat2)
year<-year(caldat2)
month<-month(caldat2)
TimeFrame<-data.frame(caldat2,weekday,quarter,year,month)
TimeFrame<-TimeFrame[-(which(TimeFrame$year == 2017 & TimeFrame$month > 3)),]
idx<-which(TimeFrame$quarter == 1 & TimeFrame$year == 2017)
TimeFrame$quarter[idx]<-5
return(TimeFrame)
}
TimeFrame<-Time_Series(frame2$ARRESTTIME)
tapply(TimeFrame$weekday,TimeFrame$quarter,table)

#add new columns which show the mean of arrests in each specific time point to data frame 8 
Count1M<-round(frame8$Count1/13,2)
Count2M<-round(frame8$Count2/13,2)
Count3M<-round(frame8$Count3/13,2)
Count4M<-round(frame8$Count4/13,2)
Count5M<-round(frame8$Count5/13,2)
frame9<-data.frame(frame8,Count1M,Count2M,Count3M,Count4M,Count5M)

#replace the value of the count if the frequency of that weekday is not usual in that quarter 
frame9$Count3M<-ifelse(frame9$Weekday == "Friday",round(frame9$Count3/14,2),frame9$Count3M) 
frame9$Count4M<-ifelse(frame9$Weekday == "Saturday",round(frame9$Count4/14,2),frame9$Count4M) 
frame9$Count5M<-ifelse(frame9$Weekday == "Saturday",round(frame9$Count5/12,2),frame9$Count5M) 

#############Create the Heatmap by ggplot###########################
library(ggplot2)
# Add a time level to correct the sequence
levels(frame9$Weekday)   #"Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"   "Wednesday"
bc<-as.character(frame9$Weekday)
levels(bc)
order1<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
frame9$Weekday<-factor(frame9$Weekday,levels=order1,ordered = T)
levels(frame9$Weekday)

PlotbyTime<-function(week,hourpoint,arrest){
  dat<-data.frame(week,hourpoint,arrest)
  ggplot(dat, aes(x=week, y=hourpoint))+
    geom_tile(aes(fill=arrest),colour="white")+
    theme(axis.text.x = element_text(size=11, angle=0))+
    labs(x="Day of Week of Offenses Happened",y="Hour of Offenses (Local Time)")+
    scale_fill_continuous(limits=c(0,100),low="gray",high="#2980B9")+
    # add limits here!
    guides(fill=guide_legend("Offenses"))
}

#Graphs for Assault in 2016 and 2017 by quarter.
PlotbyTime(frame9$Weekday[which(frame9$Type=="27")],frame9$Time[which(frame9$Type=="27")],frame9$Count1M[which(frame9$Type=="27")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="27")],frame9$Time[which(frame9$Type=="27")],frame9$Count2M[which(frame9$Type=="27")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="27")],frame9$Time[which(frame9$Type=="27")],frame9$Count3M[which(frame9$Type=="27")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="27")],frame9$Time[which(frame9$Type=="27")],frame9$Count4M[which(frame9$Type=="27")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="27")],frame9$Time[which(frame9$Type=="27")],frame9$Count5M[which(frame9$Type=="27")])

#Graphs for Controlled substance Possession in 2016 and 2017 by quarter.
PlotbyTime(frame9$Weekday[which(frame9$Type=="13(a)")],frame9$Time[which(frame9$Type=="13(a)")],frame9$Count1M[which(frame9$Type=="13(a)")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="13(a)")],frame9$Time[which(frame9$Type=="13(a)")],frame9$Count2M[which(frame9$Type=="13(a)")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="13(a)")],frame9$Time[which(frame9$Type=="13(a)")],frame9$Count3M[which(frame9$Type=="13(a)")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="13(a)")],frame9$Time[which(frame9$Type=="13(a)")],frame9$Count4M[which(frame9$Type=="13(a)")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="13(a)")],frame9$Time[which(frame9$Type=="13(a)")],frame9$Count5M[which(frame9$Type=="13(a)")])

#Graphs for Inchoate Crimes in 2016 and 2017 by quarter.
PlotbyTime(frame9$Weekday[which(frame9$Type=="9")],frame9$Time[which(frame9$Type=="9")],frame9$Count1M[which(frame9$Type=="9")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="9")],frame9$Time[which(frame9$Type=="9")],frame9$Count2M[which(frame9$Type=="9")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="9")],frame9$Time[which(frame9$Type=="9")],frame9$Count3M[which(frame9$Type=="9")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="9")],frame9$Time[which(frame9$Type=="9")],frame9$Count4M[which(frame9$Type=="9")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="9")],frame9$Time[which(frame9$Type=="9")],frame9$Count5M[which(frame9$Type=="9")])

#Graphs for Theft in 2016 and 2017 by quarter.
PlotbyTime(frame9$Weekday[which(frame9$Type=="39")],frame9$Time[which(frame9$Type=="39")],frame9$Count1M[which(frame9$Type=="39")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="39")],frame9$Time[which(frame9$Type=="39")],frame9$Count2M[which(frame9$Type=="39")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="39")],frame9$Time[which(frame9$Type=="39")],frame9$Count3M[which(frame9$Type=="39")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="39")],frame9$Time[which(frame9$Type=="39")],frame9$Count4M[which(frame9$Type=="39")])
PlotbyTime(frame9$Weekday[which(frame9$Type=="39")],frame9$Time[which(frame9$Type=="39")],frame9$Count5M[which(frame9$Type=="39")])


