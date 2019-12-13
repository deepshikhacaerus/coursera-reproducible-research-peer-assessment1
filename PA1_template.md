# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())

```r
getwd()
setwd("C:/Users/Deep_shikha/Documents/Reproducible Research")
zipfile <- "C:/Users/Deep_shikha/Documents/Reproducible Research/repdata_data_activity.zip"
filedir <- "C:/Users/Deep_shikha/Documents/Reproducible Research"
unzip_path <- "C:/Users/Deep_shikha/Documents/Reproducible Research/repdata_data_activity"  ##### path for storing the unzipped files #######
if (!file.exists(filedir)){
  dir.create(filedir)
}
unzip(zipfile,exdir=unzip_path) ####### exdir is the extract directory ##########
datafile <- file.path(unzip_path,"activity.csv")
activity <- read.csv(datafile)
```
##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity$date <- ymd(activity$date)
activity$weekend <- as.factor(ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday","weekend","weekday"))
activity$dayofweek <- as.factor(weekdays(activity$date))
```

-----

## What is mean total number of steps taken per day?

```r
stepsByDay <- activity %>% group_by(date) %>% summarise(stepsperday = sum(steps,na.rm = TRUE))
```

##### 1. Make a histogram of the total number of steps taken each day

```r
qplot(stepsperday,data=stepsByDay,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
* Mean: 9354.2295
* Median:  10395

-----

## What is the average daily activity pattern?

```r
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot

```r
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```

* Most Steps at: 8:35

----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 

```r
numMissingValues <- length(which(is.na(activityData$steps)))
```

* Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
```


##### 4. Make a histogram of the total number of steps taken each day 

```r
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
```
* Mean (Imputed): 1.0766 &times; 10<sup>4</sup>
* Median (Imputed):  1.0766 &times; 10<sup>4</sup>


----

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Make a panel plot containing a time series plot


```r
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


