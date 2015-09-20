Report on Personal Activity Monitoring Device
=============================================
Download the file from Course website, unzip the downloaded file and load the data


```r
if(!file.exists("ACTMON_DAT")){dir.create("ACTMON_DAT")}
unzip(zipfile="RepData_PeerAssessment1-master.zip", exdir="./ACTMON_DAT")
act_mon_ds <- read.csv("./ACTMON_DAT/activity.csv", header = TRUE, sep =",")
```
Strip out all NAs from the data set & Calculate total steps taken/day

```r
act_mon_mv <- na.omit(act_mon_ds)
total_steps_day <- aggregate(steps ~ date, act_mon_mv, sum)
```
Chart total steps taken/day chart using histogram

```r
hist(total_steps_day$steps, main="Total Steps taken per day", xlab="Number of Steps", ylab="Frequency", border="blue", col="green", ylim = c(0,40))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Find the mean and median of total steps taken/day

```r
tot_mean <- mean(total_steps_day$steps)
tot_median <- median(total_steps_day$steps)
print(tot_mean)
```

```
## [1] 10766.19
```

```r
print(tot_median)
```

```
## [1] 10765
```

Find the average daily acivity pattern

```r
avg_daily_ds <- aggregate(steps ~ interval, act_mon_mv, mean)
```

Plot the chart for daily activity pattern 

```r
plot(avg_daily_ds$interval,avg_daily_ds$steps, type="l", xlab="Interval", ylab="Avg Number of Steps",main="Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Calculate the total number of missing values in the dataset

```r
mv_tot <- sum(is.na(act_mon_ds$steps))
print(mv_tot)
```

```
## [1] 2304
```

Fill out the missing values by using the mean for the 5 minute interval

```r
impute_ds <- act_mon_ds
na <- is.na(impute_ds$steps)
mean_ds <- tapply(act_mon_mv$steps, act_mon_mv$interval, mean)
impute_ds$steps[na] <- mean_ds[as.character(impute_ds$interval[na])]
```
Calculate total number of steps taken for the new data set

```r
tot_daily_steps_ds <- tapply(impute_ds$steps, impute_ds$date, sum, na.rm=TRUE)
```

Chart total number of steps taken each day using new data set

```r
hist(tot_daily_steps_ds, main="Total number of Steps taken each day", xlab="Number of Steps", ylab="Frequency", border="blue", col="green", ylim= c(0,35))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Calculate mean and median of the steps taken/day

```r
mean_steps <- mean(tot_daily_steps_ds)
median_steps <- median(tot_daily_steps_ds)
print(mean_steps)
```

```
## [1] 10766.19
```

```r
print(median_steps)
```

```
## [1] 10766.19
```
Create a new factor variable in the dataset with weekday and weekend

```r
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

impute_ds$day_of_week <- as.factor(ifelse(is.element(weekdays(as.Date(impute_ds$date)),weekday), "weekday", "weekend"))
```
Calculate average number of steps taken across all weekday and weekend days and plot the same

```r
wd_we_activity <- aggregate(steps ~ interval + day_of_week, impute_ds, mean)
```

```r
library(lattice)
xyplot(wd_we_activity$steps ~ wd_we_activity$interval | wd_we_activity$day_of_week, main="Differences in activity patterns between weekdays and weekends",xlab="Interval", ylab="Steps taken per Day",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 



