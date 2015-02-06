# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The raw data was obtained from the github repo https://github.com/rdpeng/RepData_PeerAssessment1 
The file name is activity.csv and there are 17,568 observations with three variables (columns): 

- **steps:** Number of steps taken in a 5-minute interval (missing = NA)  
- **date:** The date (YYYY-MM-DD) in which the measurement was taken  
- **interval:** Five minute interval identifier integer (HHMM)  

```r
dataset <- read.csv("../Data/activity.csv")
str(dataset)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Pre-process interval into POSIXct timeOfDay variable for plot x-axis
Convert integer interval variable into factor variable 

```r
hour <- as.integer(dataset$interval / 100)
min <- as.integer(dataset$interval %% 100)
time <- paste(as.character(hour), ":", as.character(min), sep="")
dataset$timeOfDay <- as.POSIXct(strptime(time, "%H:%M"))
dataset$interval <- as.factor(dataset$interval)
```
  
  
## What is mean total number of steps taken per day?
Calculates stepsPerDay the total steps taken per day by summing dataset\$steps (steps taken in each 5 minute interval) over dataset\$date (61 level factor variable for each day).  Calculates the mean and median steps per day from stepsPerDay.  

```r
stepsPerDay <- tapply(dataset$steps, dataset$date, FUN=function(x) sum(x, na.rm=TRUE))
meanStepsPerDay <- sprintf("%.2f", mean(stepsPerDay, na.rm=TRUE))
medianStepsPerDay <- median(stepsPerDay, na.rm=TRUE)
hist(stepsPerDay, breaks=10, xlab="Total Steps Per Day")
```

![](PA1_template_files/figure-html/stepsPerDay-1.png) 

**The mean number of steps taken each day is : 9354.23**  
**The median of the number of steps taken each day is : 10395**  


## What is the average daily activity pattern?
Calculates the daily activity pattern (average number of steps for each 5 minute interval) by taking the mean of the dataset\$steps over the factor dataset\$interval with 288 5 minute interval levels in a 24 hour day.

```r
activityPattern <- as.data.frame(tapply(dataset$steps, dataset$interval, 
                                        FUN=function(x) mean(x, na.rm=TRUE)))
```

Sets up x-axis for time of day plot and combines with activityPattern in t.    
The x,y plot of t is displayed.  

```r
timeOfDay <- as.data.frame(unique(dataset$timeOfDay))
t <- cbind(timeOfDay, activityPattern)
names(t) <- c("TimeOfDay","ActivityPattern")
plot(t$TimeOfDay, t$ActivityPattern, type="l",
     main="Time Series Activity By Time Of Day",     
     xlab = "Time Of Day",
     ylab = "Average Steps Per 5 Minute Interval")
```

![](PA1_template_files/figure-html/plotTimeOfDaySteps-1.png) 

```r
# computes the time of the maximum step activity
t1 <- t[t[,"ActivityPattern"]==max(t[,"ActivityPattern"]),]
maxSteps <- sprintf("%.2f", t1["ActivityPattern"])
maxStepsTime <- as.character(format(t1["TimeOfDay"], "%H:%M"))  
```

**The maximum average number of steps per 5 minute interval is 206.17 and occurs at 08:35**    

## Imputing missing values

```r
stepsMissing <- sum(is.na(dataset$steps))
dateMissing <- sum(is.na(dataset$date))
intervalMissing <- sum(is.na(dataset$interval))
```
Missing 2304 data values from steps column    
Missing 0 data values from date column     
Missing 0 data values from interval column   

Impute each 'steps' NA missing values with the corresponding 'interval' (time of day) average number of steps.  

```r
dataset$intervalavg <- ave(dataset$steps, dataset$interval, FUN=function(x) mean(x, na.rm=TRUE))
dataset$steps[is.na(dataset$steps)] <- as.integer(dataset$intervalavg[is.na(dataset$steps)])
```

**Strategy for imputing missing data.**  
Recalculate the total number of steps per day with the imputed NAs.
Plot histogram and compare with previous results.
Since there are no NA's the na.rm is no longer needed with the sum function.  


```r
stepsPerDayAll <- tapply(dataset$steps, dataset$date, sum)
hist(stepsPerDayAll, breaks=10, 
     main="Histogram of steps per day with imputed missing values",
     xlab="Total Steps Per Day")
```

![](PA1_template_files/figure-html/recalcStepTotal-1.png) 

```r
meanStepsPerDayAll <- sprintf("%.2f", mean(stepsPerDayAll, na.rm=TRUE))
medianStepsPerDayAll <- median(stepsPerDayAll, na.rm=TRUE)
```
The average number of steps taken per day with imputed NAs is : 10749.77  
The median of the number of steps taken per day within imputed NAs is : 10641  

The mean with imputed NAs of 10749.77 is less than the removed NA mean of 9354.23  
The median with imputed NAs of 10641 is less than the removed NA median of 10395  
  
In this instance, imputing the NA (missing) data decreased both the estimated mean and median for the total number of steps taken per day.

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable 'weekend' with two levels - 'weekday' and 'weekend' from days of the week.   

```r
dataset$weekend <- ifelse(weekdays(as.POSIXct(dataset$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")
dataset$weekend <- as.factor(dataset$weekend)
str(dataset$weekend)
```

```
##  Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Split the data by the weekend factor and calculate the weekend and weekday step activity by time of day.  
Bind these data with the timeOfDay from above into variable t.  


```r
dataset2 <- split(dataset, dataset$weekend)
weekdayActivity <- as.data.frame(tapply(dataset2$weekday$steps, dataset2$weekday$interval, 
                                        FUN=function(x) mean(x, na.rm=TRUE)))
weekendActivity <- as.data.frame(tapply(dataset2$weekend$steps, dataset2$weekend$interval, 
                                        FUN=function(x) mean(x, na.rm=TRUE)))
t <- cbind(timeOfDay, weekdayActivity, weekendActivity)
names(t) <- c("TimeOfDay","Weekday", "Weekend")
summary(t)
```

```
##    TimeOfDay                      Weekday           Weekend       
##  Min.   :2015-02-06 00:00:00   Min.   :  0.000   Min.   :  0.000  
##  1st Qu.:2015-02-06 05:58:45   1st Qu.:  2.156   1st Qu.:  1.188  
##  Median :2015-02-06 11:57:30   Median : 25.700   Median : 32.312  
##  Mean   :2015-02-06 11:57:30   Mean   : 35.553   Mean   : 42.312  
##  3rd Qu.:2015-02-06 17:56:15   3rd Qu.: 50.806   3rd Qu.: 74.594  
##  Max.   :2015-02-06 23:55:00   Max.   :230.356   Max.   :166.625
```


```r
par(mfrow=c(2,1)) # setup for 2 rows and 1 column of plots
par(mai=c(0.5,0.8,0.2,0.0)) # margins (bottom, left, top, right)
with(t, {
    plot(TimeOfDay, Weekday, type="l", main="Activity on Weekdays",
         ylab="Steps per 5 Minutes", ylim=c(0.0, 300.0))
    plot(TimeOfDay, Weekend, type="l", main="Activity on Weekends",
         ylab="Steps per 5 Minutes", ylim=c(0.0, 300.0))
})
```

![](PA1_template_files/figure-html/plotActivity-1.png) 

Yes.  There are noticeable differences in activity patterns between weekdays and weekends.  
