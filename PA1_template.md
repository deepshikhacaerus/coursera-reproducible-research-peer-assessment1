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
meanstepsperday <- stepsByDay %>% summarise(average = mean(stepsperday,na.rm = TRUE),median=median(stepsperday,na.rm = TRUE))
meanstepsperday
```
* Mean: 9354.2295
* Median:  10395

-----

## What is the average daily activity pattern?

```r
interval_average <- activity %>% group_by(interval) %>% summarise(average = mean(steps,na.rm = TRUE))
```

##### 1. Make a time series plot

```r
qplot(interval,average,data=interval_average,geom="line",xlab = "5-minute intervals",ylab = "Average steps taken across all days")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_average[which.max(interval_average$average),]
```

* Most Steps at: 8:35

----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 

```r
# subset dataset where there are no NAs
activity_no_NA <- activity[which(!is.na(activity$steps)),]
# calculate the mean steps for each interval
interval_only <- activity_no_NA %>% group_by(interval) %>% summarise(average=mean(steps))
# convert the average to integer
interval_only$average <- as.integer(interval_only$average)
#subset dataset where steps have NAs
activity_na <- activity[which(is.na(activity$steps)),]
# fill NAs with average steps based on interval
activity_na$steps <- ifelse(activity_na$interval==interval_only$interval,interval_only$average)
# row bind the datasets that do not have NAs and the dataset where NAs are replaced with
# mean values
activity_impute <- rbind(activity_no_NA,activity_na)
```

nrow(activity_na)


* Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
stepsByDay_impute <- activity_impute %>% group_by(date) %>% summarise(stepsperday = sum(steps))
```


##### 4. Make a histogram of the total number of steps taken each day 

```r
qplot(stepsperday,data=stepsByDay_impute,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
totalstepsperday_impute <- activity_impute %>% group_by(date) %>% summarise(stepsperday = sum(steps))
mean_n_median <- totalstepsperday_impute %>% summarise(average=mean(stepsperday),median=median(stepsperday))
mean_n_median
```
* Mean (Imputed): 1.0766 &times; 10<sup>4</sup>
* Median (Imputed):  1.0766 &times; 10<sup>4</sup>


----

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
meansteps <- activity_impute %>% group_by(interval,weekend) %>%   summarise(average = mean(steps))y')
```

##### 2. Make a panel plot containing a time series plot


```r
qplot(interval,average,data=meansteps,geom="line",facets=weekend~.,xlab="5-minute interval",ylab="average number of steps",main="Average steps pattern between Weekday and Weekend")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


