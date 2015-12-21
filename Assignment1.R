#Load required libraries
library(knitr)
opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(ggplot2)

## 1. Loading and preprocessing the data

#Read in the data
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character",
                                                           "integer"))
#Format date data
data$date <- as.Date(data$date)

#Take a look at the formatted data
str(data)
head(data)

## 2. What is mean total number of steps taken per day?

#Aggregate the data by date, adding the total steps per day
stepsperday <- aggregate(steps~date, data, sum)

#Aggregate the sum of steps per day
sumstepsperday <- aggregate(data$steps ~ data$date, FUN=sum)
colnames(sumstepsperday)<- c("Date", "Steps")

#Take a look at it to make sure things have been aggregated correctly
head(sumstepsperday)

#Make a histogram of the total steps per day
hist(sumstepsperday$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

#Calculate mean
as.integer(mean(sumstepsperday$Steps))
#Mean is 10766 steps per day

#Calculate median
as.integer(median(sumstepsperday$Steps))
#Median is 10765 steps per day

## 3. What is the average daily activity pattern?

#Aggregate the number of steps per 5-minute interval, take the mean of each interval
stepsperinterval<-aggregate(steps~interval,data,mean)

## 3.1. Make a time-series plot of the 5-minute intervals and the mean of steps taken per interval
timeseriesplot<-ggplot(stepsperinterval, aes(stepsperinterval$interval, stepsperinterval$steps))+geom_line(color="blue",size=1)+theme_bw()
timeseriesplot<-timeseriesplot+xlab("5-Minute Interval")+ylab("Avg Number of Steps")+ggtitle("Time Series Plot")
timeseriesplot

## 3.2.Which 5-minute interval, on average, across all days in the dataset, contains the maximum number of steps?

stepsperinterval[which.max(stepsperinterval$steps),]
##Shows that interval with highest number of steps is 835 is with 206.17 steps

## 4. Impute missing values

# 4.1. Summarize all the missing values to show number of missing values
sum(is.na(data$steps))
#Shows that there are 2304 lines with missing data

## 4.2. Devise a strategy for filling in all of the missing values in the dataset.

#Create a new dataset as the original and use a for loop for filling in the missing values with the average number of steps per 5-minute interval:

## 4.3. Create a new data frame, a copy of the original data set with NAs included.
data2<-data

#Convert dates and steps as done with original data set
data2$date<-as.Date(data2$date)
data2$steps<-as.numeric(data2$steps)

#Create a for-loop that writes over any NA values with the mean of that 5-minute interval
for (i in 1:nrow(data2)) {
        if (is.na(data2$steps[i])){
                data2$steps[i] <- stepsperinterval[which(data2$interval[i] == stepsperinterval$interval), ]$steps
                        }
                }

#Aggregate the new filled-in data set by date, adding the total steps per day
stepsperday2<-aggregate(steps~date,data2,sum)

## 4.4. Make a new histogram using the filled-in data set
Filleddata<-ggplot(stepsperday2,aes(x=stepsperday2$steps))+theme_bw()+geom_histogram(fill="bisque2",color="black",binwidth=1000)
Filleddata<-Filleddata+geom_vline(xintercept=mean(stepsperday2$steps,na.rm=TRUE),size=2,color="blue",alpha=.5)+
geom_vline(xintercept=median(stepsperday2$steps,na.rm=TRUE),size=2,color="red",alpha=.5)
Filleddata<-Filleddata+xlab("Steps per Day, Filled")+ylab("Frequency")+ggtitle("Histogram of Total Steps per Day, Filled")
Filleddata

#Obtain the new mean and median from our filled-in data set
mean(stepsperday2$steps)
median(stepsperday2$steps)
#Shows there is no appreciable difference between that data with NAs and the data in which the NAs have been filled in.

## 5. Are there differences in activity patterns between weekdays and weekends?

#Copy the filled in data frame and work on it while protecting the original.
data2daytypes <- data2

#Add new column to our data set listing the specific day of the week
data2daytypes$day<-weekdays(data2daytypes$date)

#Runs a for-loop that looks for days that are weekends vs. weekdays
wkdvswend <- function(x){
        if(x %in% c("Saturday", "Sunday")) {
                return("Weekend")
  }
                else(return("Weekday"))
  }

#Create a new "weekday" column that uses the for-loop to populate whether the specific day is during the weekend vs. weekday
data2daytypes$weekday<-as.factor(sapply(data2daytypes$day,wkdvswend))

#Aggregate the means of the steps by weekday and by 5-minute interval
aggdaytype<-aggregate(steps~interval+weekday,data2daytypes, mean)

## 5.2. Make a panel plot to show the difference in the means of steps between weekdays vs weekends
wkdvswkendplot<-ggplot(aggdaytype,aes(x=interval, y=steps))+geom_line(color="blue")+facet_grid(weekday~.)+theme_bw()
wkdvswkendplot<-wkdvswkendplot+xlab("5-Minute Intervals")+ylab("Avg Number of Steps")+ggtitle("Weekdays vs. Weekends")
wkdvswkendplot
#Panel shows that number of steps on weekends is much lower than on weekdays.
