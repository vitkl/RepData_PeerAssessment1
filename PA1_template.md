# Reproducible Research: Peer Assessment 1
Vitalii Kleshchevnikov  

Running this Rmd requires tidyr and ggplot packages

## Loading and preprocessing the data

Reading data - and quick look. 

```r
# setwd("./RepData_PeerAssessment1")
activity.dat = read.csv("activity.csv", colClasses = c("numeric", "Date","integer"),stringsAsFactors = F)
head(activity.dat)
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

```r
str(activity.dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
"spreading" data frame - so that each row contains all observations for each day,
intervals in columns

```r
activity.dat.1row.per.date = tidyr::spread(activity.dat, interval, steps)
activity.dat.1row.per.interval = tidyr::spread(activity.dat, date, steps)
# cheking how data was spread - code below produces lond output - disabled
# str(activity.dat.1row.per.interval)
# str(activity.dat.1row.per.date)
```


## What is mean total number of steps taken per day?

Removing days which doesn't have any measurements and
calculating total number of steps per day (NA omitted) - plotting histogram

```r
activity.dat.1row.per.date.na.rm = activity.dat.1row.per.date[complete.cases(activity.dat.1row.per.date),]
total_steps.per.day = rowSums(activity.dat.1row.per.date.na.rm[,2:289])

library(ggplot2)
qplot(x=total_steps.per.day, geom = "histogram", bins = 15) +
      xlab("the total number of steps taken each day") +
      ggtitle("Histogram of the total number of steps taken each day") +
      theme(title = element_text(size = rel(1.2)))
```

![](PA1_template_files/figure-html/steps_per_day-1.png)<!-- -->


```r
mean_steps.per.day = format(round(mean(total_steps.per.day),0), scientific=F)
# round to the nearest integer value (steps cannot be fractional)
# format to avoid 1.07*10^4 representation
```
mean number of steps per day is 10766


```r
median_steps.per.day = format(round(median(total_steps.per.day),0),scientific=F)
# round to the nearest integer value (steps cannot be fractional)
# format to avoid 1.07*10^4 representation
```
median number of steps per day is 10765


## What is the average daily activity pattern?
Calculating the mean number of steps per time interval (over all days) and
the interval containing maximum number of steps

```r
mean_steps.per.interval = rowMeans(activity.dat.1row.per.interval[,2:62], na.rm = T)
max_step_interval = 
      activity.dat.1row.per.interval$interval[mean_steps.per.interval == max(mean_steps.per.interval)]
```


```r
qplot(x = activity.dat.1row.per.interval$interval, y = mean_steps.per.interval, geom="path") + 
      xlab("time interval") +
      ylab("mean number of steps")+
      ggtitle("the average (mean) number of steps taken in different intervals during the day") + scale_x_continuous(breaks = seq(0,2400,100))+
      theme(title = element_text(size = rel(1)))
```

![](PA1_template_files/figure-html/plotting_steps_per_interval-1.png)<!-- -->

The interval containing maximum number of steps (on average(mean) across all days) - 835


## Imputing missing values


```r
missing_rows = sum(!complete.cases(activity.dat))
```
the number of intervals missing observations (the number of steps) is 2304

Imputing missing values by setting each missing observation (steps) to be equal 
to the mean number of steps taken in that interval

```r
activity.dat.1row.per.interval.imputed = activity.dat.1row.per.interval
activity.dat.1row.per.interval.imputed[is.na(activity.dat.1row.per.interval)] = mean_steps.per.interval
```
Calculating total number of steps per day (NA imputed) - plotting histogram

```r
total_steps.per.day.imputed = 
      colSums(activity.dat.1row.per.interval.imputed[,2:62], na.rm = F)

library(ggplot2)
qplot(x=total_steps.per.day.imputed, geom = "histogram", bins = 15) + 
      xlab("the total number of steps taken each day") +
      ggtitle("Histogram of the total number of steps taken each day \n (after missing values were imputed)") +
      theme(title = element_text(size = rel(1.2)))
```

![](PA1_template_files/figure-html/histogram_steps_day-1.png)<!-- -->


```r
mean_steps.per.day.imputed = format(round(mean(total_steps.per.day.imputed),0),scientific=F)
# round to the nearest integer value (steps cannot be fractional)
# format to avoid 1.07*10^4 representation
```
mean number of steps per day doesn't change after imputing and equals to 10766.


```r
median_steps.per.day.imputed = format(round(median(total_steps.per.day.imputed),0),scientific=F)
# round to the nearest integer value (steps cannot be fractional)
# format to avoid 1.07*10^4 representation
```
Median number of steps per day after imputing is 10765. 
Imputing missing values doesn't substantially change median either.

## Are there differences in activity patterns between weekdays and weekends?

Code below transfors the data.frame with imputed values into the form of original 
dataset, adds the fourth variable weekday/weekend, separates data.frame by 
weekday/weekend, transforms data.frame by spreading, recalculates the mean 
number of steps in each time interval over weekdays or weekends. 
Plots the result: during the day on x-axis, average(mean) number of steps on the 
y-axis)


```r
activity.dat.weekdays = tidyr::gather(data = activity.dat.1row.per.interval.imputed,
                                      key = date, value = steps, -interval)
activity.dat.weekdays[,4] = weekdays(as.Date(activity.dat.weekdays$date))
colnames(activity.dat.weekdays)[4] = "weekday"
# Changing specific days to weekday/weekend
activity.dat.weekdays$weekday = gsub("Friday", "weekday", activity.dat.weekdays$weekday)
activity.dat.weekdays$weekday = gsub("Monday", "weekday", activity.dat.weekdays$weekday)
activity.dat.weekdays$weekday = gsub("Tuesday", "weekday", activity.dat.weekdays$weekday)
activity.dat.weekdays$weekday = gsub("Wednesday", "weekday", activity.dat.weekdays$weekday)
activity.dat.weekdays$weekday = gsub("Thursday", "weekday", activity.dat.weekdays$weekday)
activity.dat.weekdays$weekday = gsub("Saturday", "weekend", activity.dat.weekdays$weekday)
activity.dat.weekdays$weekday = gsub("Sunday", "weekend", activity.dat.weekdays$weekday)
# Transforming data for plotting
activity.dat.weekdays.day = dplyr::filter(activity.dat.weekdays, weekday == "weekday")
activity.dat.weekdays.day.s = tidyr::spread(activity.dat.weekdays.day, date, steps)
mean_steps.per.interval.weekday = data.frame(interval = activity.dat.weekdays.day.s$interval, 
                  mean_steps = rowMeans(activity.dat.weekdays.day.s[,3:ncol(activity.dat.weekdays.day.s)]), 
                  weekday = activity.dat.weekdays.day.s$weekday)

activity.dat.weekdays.end = dplyr::filter(activity.dat.weekdays, weekday == "weekend")
activity.dat.weekdays.end.s = tidyr::spread(activity.dat.weekdays.end, date, steps)
mean_steps.per.interval.weekend = data.frame(interval = activity.dat.weekdays.end.s$interval, 
                  mean_steps = rowMeans(activity.dat.weekdays.end.s[,3:ncol(activity.dat.weekdays.end.s)]), 
                  weekday = activity.dat.weekdays.end.s$weekday)

activity.weekdays = rbind(mean_steps.per.interval.weekday, mean_steps.per.interval.weekend)

# plotting
ggplot(activity.weekdays, aes(x=interval, y = mean_steps))+
      geom_path() + facet_grid(weekday~.)+
            xlab("time interval") +
      ylab("mean number of steps")+
      ggtitle("the average (mean) number of steps taken in different intervals \n during the day, weekdays as compared to weekends") +
      scale_x_continuous(breaks = seq(0,2400,100))+
      theme(title = element_text(size = rel(1)))
```

![](PA1_template_files/figure-html/weekdays_vs_weekends-1.png)<!-- -->

From the graph you can infer the person tends to get up and go to bed earlier during weekdays. Another tendency is that person walks more during the day (10:00-20:00).
Finally, you can see a strong spike in activity in the morning, around 8:35. The
absence of such spike in the evening suggests it may not be generated by 
rushing to workplace, rather by physical activity, let's say jogging.
