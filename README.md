# Reproducible-Research-project-1
---
title: "Course Project 1"
author: "Juan Esteban Fonseca"
date: "31 de julio de 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##Synopsis 
The purpose of this project was to use MarkDown and knitr in order to present results to answer some questions about the FitBit data.

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Loading and preprocessing data
```{r preprocessing}
if(!file.exists("datafile.zip")) {
  temporal<-tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temporal)
  unzip(temporal)
  unlink(temporal)
}

data<-read.csv("activity.csv")

```

##Mean total number of steps taker per day

A histogram is made with the aggregated steps by day in order to calculate de mean and the median

```{r q1}
steps.day<- aggregate(steps~ date, data, sum)
hist(steps.day$steps,main=paste("Total Steps per day"),col="red",xlab = "Number of steps")
smean<-mean(steps.day$steps)
smedian<-median(steps.day$steps)
```

The steps per day mean is `r smean`and the median `r smedian`

##Average daily activity pattern

Here the average steps for each interval for all days is calculated, including the corresponding plot and showing the major interval.

```{r q2}
steps.interval<- aggregate(steps~ interval, data, mean)
plot(steps.interval$interval,steps.interval$steps,type = "l",xlab="Interval", ylab="Steps", main="Average number o f steps per day by interval")

max.interval<-steps.interval[which.max(steps.interval$steps),1]
```
The max interval is `r max.interval`


##Missing values imputation and contrast with analysis without it


```{r q3}
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps.interval$steps[match(data$interval, steps.interval$interval)], data$steps))
```

There are `r incomplete`cases


Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data.

```{r q4}
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```
Recount total steps by day and create Histogram.
```{r q5}
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps per day"), col="blue", xlab="Number of Steps")

#Create Histogram to show difference. 

hist(steps.day$steps, main = paste("Total Steps per day"), col="red",xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

Calculate new mean and median for imputed data.
```{r q7}
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```
Calculate difference between imputed and non-imputed data.
```{r q8}
mean_diff <- rmean.i - smean
med_diff <- rmedian.i - smedian
```


Calculate total difference.
```{r q9}
total_diff <- sum(steps_by_day_i$steps) - sum(steps.day$steps)
```

The imputed data mean is `r rmean.i`
The imputed data median is `r rmedian.i`
The difference between the non-imputed mean and imputed mean is `r mean_diff`
The difference between the non-imputed mean and imputed mean is `r med_diff`
The difference between total number of steps between imputed and non-imputed data is `r total_diff`. Thus, there were r total_diff more steps in the imputed data.

##Are there differences in activity patterns between weekdays and weekends?

Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.
```{r q10}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
