---
title: "project1"
author: "Safwan"
date: "02/05/2021"
output: html_document
---



## R Markdown
#steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
#2. date: The date on which the measurement was taken in YYYY-MM-DD format
#3. interval: Identifier for the 5-minute interval in which measurement was taken

#Loading and preprocessing data

```r
data <- read.csv("activity.csv",header = T)
dim(data)
```

```
## [1] 17568     3
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
#Total number of steps taken per day

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 4.0.5
```

```r
totalNumberofSteps <- with(data, tapply(steps, as.factor(data$date), sum, na.rm = T))
hist(totalNumberofSteps, main = "HISTOGRAM OF TOTAL NUMBER OF STEPS PER DAY", xlab = "TOTAL NUMBER OF STEPS")
```

<img src="project1_files/figure-html/total steps-1.png" width="672" />

```r
summary(totalNumberofSteps) #to get mean and median
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
#average daily pattern

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.5
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
StepsPerTime <- aggregate(steps~interval,data=data,FUN=mean,na.action=na.omit)
StepsPerTime$time <- StepsPerTime$interval/100
# draw the line plot
h <- ggplot(StepsPerTime, aes(time, steps))
h+geom_line(col="brown")+ggtitle("AVERAGE STEPS PER TIME INTERVAL")+xlab("TIME")+ylab("STEPS")+theme(plot.title = element_text(face="bold", size=14))
```

<img src="project1_files/figure-html/stepspertime-1.png" width="672" />

```r
# table for dplyr
ST <- tbl_df(StepsPerTime)
```

```
## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
## Please use `tibble::as_tibble()` instead.
```

```r
# find the column
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))
```

```
## # A tibble: 1 x 2
##    time steps
##   <dbl> <dbl>
## 1  8.35  206.
```
#Imputing missing values

```r
MissingData <- data[is.na(data$steps),]
dim(MissingData )
```

```
## [1] 2304    3
```

```r
length(MissingData$steps)
```

```
## [1] 2304
```

```r
MeanSteps <- with(data, tapply(steps, data$interval, mean))
MissingData$steps <-MeanSteps
NewData <- rbind(data,MissingData)
NewData <- NewData[order(NewData$date), ]
TotalNumberofSteps <- with(NewData, tapply(steps, as.factor(NewData$date), sum))
#Make a histogram of the total number of steps taken each day
hist(TotalNumberofSteps, main = "HISTOGRAM OF TOTAL NUMBER OF STEPS PER DAY", xlab = "TOTAL NUMBER OF STEPS")
```

<img src="project1_files/figure-html/missing values-1.png" width="672" />

```r
summary(TotalNumberofSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```
#Are there differences in activity patterns between weekdays and weekends?

```r
NewData$days <- weekdays(as.Date(NewData$date))
# find weekend features in the dataset
Weekend_Feature <- grep("Saturday|Sunday", NewData$days, ignore.case = T)
# subset data of the weekend
Weekend_Data<-  NewData[Weekend_Feature, ]
Weekend_Data$weekday <- "weekend"

# subset data of the weekday
Weekday_Data <- subset(NewData,NewData$days!=Weekend_Feature)
```

```
## Warning in NewData$days != Weekend_Feature: longer object length is not a
## multiple of shorter object length
```

```r
Weekday_Data$weekday <- "weekday"

NewData2 <- rbind(Weekday_Data, Weekend_Data)
mean_number_steps <- aggregate(steps~ interval+weekday, NewData2, mean)
g <- qplot(interval, steps, data = mean_number_steps, facets = weekday~.)
g + geom_line(size = 1) + ylab("MEAN STEPS") + ggtitle("AVERAGE NUMBER OF STEPS TAKEN \n AVERAGED ACROSS ALL WEEKDAYS AND WEEKENDS ")
```

<img src="project1_files/figure-html/weekdays-1.png" width="672" />












