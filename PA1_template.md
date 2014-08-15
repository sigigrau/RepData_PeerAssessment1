# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

First, we load the data:


```r
data <- read.table("activity.csv", header=TRUE, sep=",")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Then, we convert the date variable to date type:


```r
data <- transform(data, date = as.Date(date))
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
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


## What is mean total number of steps taken per day?

First, we remove the missing values:


```r
completeCases <- data[complete.cases(data$steps),c("steps","date")]
```

Then, we calculate the number of steps taken per day and rename the columns:


```r
stepsPerDay <- aggregate(completeCases$steps, by=list(completeCases$date), FUN=sum)
names(stepsPerDay) <- c("date", "steps")
```

Then, we make a histogram of the total number of steps taken per day:


```r
barplot(
    stepsPerDay$steps, 
    names.arg=stepsPerDay$date,
    main="Total steps taken per day",
    xlab="Day",
    ylab="Steps",
    col="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

An finally, we calculate the mean and the median total number of steps taken per day:


```r
meanSteps <- mean(stepsPerDay$steps)
medianSteps <- median(stepsPerDay$steps)
```

- The mean number of steps taken per day is **1.0766 &times; 10<sup>4</sup>**.
- The median number of steps taken per day is **10765**.


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?