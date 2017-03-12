---
title: "PA1_template.Rmd"
author: "Frans van der Heijden"
date: "12 maart 2017"
output:
  html_document: default
  pdf_document: default
---

### Peer-graded Assignment: Course Project 1 of the course Reproducible Research

The data are collected from the gihub respository by donwloading and unzipping the data file

### R code used:

'read the data from the respository
'URL of the data: url_file<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

'download the data: download.file(url_file, destfile = "./data/data_activity.zip")

'unzip the zip file: unzip(zipfile = "./data/data_activity.zip", exdir = "./data") 
  

### The data file is read into Rstudio and assigned to the data file activity
```{r}
activity<-read.csv("D:/coursera/workdir_r/data/activity.csv")
activity$date<-as.Date(activity$date)
```

### Histogram of the total number of steps taken each day:
```{r histogram}
# determine the sum of steps taken each day
  steps_a_day<- tapply(activity$steps, activity$date, sum)
  # histogram of the number of steps taken every day
  hist(steps_a_day, xlab = "number of steps taken", ylab= "number of days", main = "Dailey steps taken")
```

### The mean and median of the total number of steps taken are:
```{r}
# mean and median of the total number of steps taken per day
summary(steps_a_day)[c('Mean','Median')]
```


### Time series plot of the average number of steps taken:
```{r}
# the average number of steps taken of each time interval, averaged across all days
  ## remove non applicable values
  temp<-subset(activity, !is.na(activity$steps))
  ## average of each time interval
    avg_steps_interval<-with(temp, aggregate(steps, by=list(interval), FUN=mean))
    names(avg_steps_interval)<-c("interval", "avg_no_steps")
    
  ## plot of the activity during the day
    library("ggplot2")
    p<-ggplot(avg_steps_interval, aes(x=avg_steps_interval$interval, y=avg_steps_interval$avg_no_steps))
    p + geom_line(color="blue") + labs(x="minutes", y="average number of steps") +
      labs(title="Average number of steps in time intervals of 5 minutes 
during the day in the months October and November 2012") 
```


### The 5-minute interval that, on average, contains the maximum number of steps
```{r}
# The interval with maximum average of steps
  avg_steps_interval[which.max(avg_steps_interval$avg_no_steps),]
```


### Code to describe and show a strategy for imputing missing data
```{r}
# total number of NA records
  dim(activity[is.na(activity$steps),])[1]
  
# replace the NA with the median of the time interval
  ## median steps for each time interval
  median_interval<- with(temp, aggregate(steps, by=list(interval), FUN=median))
  
  ## replace the NA value for each record in the data file activity and create a new data file
  new_activity<-activity
  for( i in 1:length(activity$steps) ){
    ## when number of steps value is NA replace the NA by the median value of the interval
    if (is.na(new_activity$steps[i]))
      new_activity$steps[i] <- median_interval[new_activity$interval[i]==median_interval$Group.1,2]
  }
```



### Histogram of the total number of steps taken each day after missing values are imputed
```{r}
  # determine the sum of steps taken each day on the corrected data file
  new_steps_a_day<- tapply(new_activity$steps, new_activity$date, sum)
  # histogram of the number of steps taken every day
  hist(new_steps_a_day, xlab = "number of steps taken", ylab= "number of days", main = "Dailey steps taken 
       (when NA values are replaced by the median of the time period)")
```


### mean and median of the total number of steps taken per day after the correction for NA values
```{r}
summary(new_steps_a_day)[c('Mean','Median')]
```
Both mean and median are decreased. Most NA values are in time intervals where the median is zero. Adding zeros decreases the mean and median.



### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
  # split the activity patterns between weekdays and weekends
  # create a new data file 'final' with an additional data field 'day'
  library(dplyr)
  final <-mutate(new_activity,day="")
  for( i in 1:length(final$steps) ){
    ## establish the day of the week and allocate to either weekday of weekend
    if (weekdays(final$date[i]) %in% c("zaterdag", "zondag") ){
      final$day[i]<-"weekend"
    }
    else {
      final$day[i] <-"weekday"
    }
  }
  
  # show the avearge number of steps during the day by interval and split by weekdays or weekends
  by1<-final$interval
  by2<-final$day
  avg_steps_final<-with(final, aggregate(steps, by=list(by1,by2), FUN=mean))
  names(avg_steps_final)<-c("interval","day_type", "avg_no_steps")
  
  # plot 
  p<-ggplot(avg_steps_final, aes(x=avg_steps_final$interval, y=avg_steps_final$avg_no_steps))
  p + geom_line(color="blue") + labs(x="minutes", y="average number of steps") +
    facet_grid(avg_steps_final$day_type ~ .) +
    labs(title="Average number of steps in time intervals of 5 minutes 
during the day in the months October and November 2012")
```

