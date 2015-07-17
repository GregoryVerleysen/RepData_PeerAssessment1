#COURSERA
#Repoducible Research: Project1
#Gregory Verleysen

setwd("~/COURSERA/Reproducible research/Project1")

#LOADING AND PREPROCESSING THE DATA
#read in data
data<-read.csv("activity.csv")
data2<-na.omit(data)

#WHAT IS THE MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY
#calculate the total number of steps taken per day
TotalSteps<-as.vector(by(data2$steps,data2$date,sum))
print(TotalSteps)
#make a histogram
hist(TotalSteps)
#calculate mean and median
mean(TotalSteps,na.rm=TRUE)
median(TotalSteps,na.rm=TRUE)

#WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN
#calculate mean
adap<-aggregate(data2$steps,list(data2$interval),mean)
print(adap)
#plot mean
plot(adap,type="l")
#find maximum
adap[which.max(adap$x),1]

#IMPUTING MISSING VALUES
#find number of missing values
length(which(is.na(data)))

means<-rep(adap$x,length(unique(data$date)))

data3<-data

for(i in 1:length(data3$steps)){
  if(is.na(data3$steps[i])) data3$steps[i] <- means[i]
}

#make histogram
TotalSteps2<-as.vector(by(data3$steps,data3$date,sum))
hist(TotalSteps2)
#mean and median
mean(TotalSteps2)
median(TotalSteps2)

#ARE THERE DIFFERENCES BETWEEN WEEKDAYS AND WEEKENDS
thedate<-as.Date(data3$date)

data3$day <- weekdays(as.Date(data3$date))

data3$daytype<-ifelse(data3$day %in% c("maandag","dinsdag","woensdag","donderdag","vrijdag"),"weekday","weekend")

#plot
library(ggplot2)
meanstep <- aggregate(steps ~ interval + daytype, data=data3, mean)

ggplot(meanstep, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")

library(lattice)
xyplot(steps  ~ interval|daytype, superpose=TRUE, aspect="xy", data = meanstep,type="l")
