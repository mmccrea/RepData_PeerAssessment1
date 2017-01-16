# Reproducible Research: Peer Assessment 1
This data analysis was done using the dplyr and ggplot2 packages.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```
## Loading and preprocessing the data
The provided activity.csv contains the step count in 5 minute intervals over two months during October and November, 2012.  The data was read in using read.csv(), and then converted into a tbl using tbl_df.

```r
actdat<- read.csv("activity.csv")
actdat<- tbl_df(actdat)
print(actdat)
```

```
## # A tibble: 17,568 × 3
##    steps       date interval
##    <int>     <fctr>    <int>
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

## What is mean total number of steps taken per day?
The sum of the number of steps recorded in each 5 minute interval for eachday was calculated using the dplyr summarize function.

```r
steps_DailySum <- group_by(actdat,date) %>% summarize(sumsteps=sum(steps,na.rm=TRUE))
```

```r
sum_steps <- ggplot(data=steps_DailySum, aes(steps_DailySum$sumsteps),xlab = "Total Steps Per Day")+ geom_histogram(binwidth=500.0, col="blue",fill="blue",alpha=0.5) + labs(title="Total Steps Per Day from Activity Monitor Data") + labs(x="Total Steps Per Day", y="Count")

print(sum_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
ttl_steps_mean <- summarize(steps_DailySum, Mean_DailySum=mean(sumsteps,na.rm=TRUE))
ttl_steps_mean <- as.numeric(ttl_steps_mean[1,1])

ttl_steps_median <- summarize(steps_DailySum, Mean_DailySum=median(sumsteps,na.rm=TRUE))
ttl_steps_median <- as.numeric(ttl_steps_median[1,1])
```
The mean of the total number of steps counted in a day is 9354.2295082, and the median is 1.0395\times 10^{4}.  

## What is the average daily activity pattern?
To examine the average daily activity pattern the data is grouped by the 5 minute intervals and summarize is then used to calculate the average of each interval over all days.

```r
steps_IntMean <- group_by(actdat,interval) %>% summarize(meanInterval=mean(steps,na.rm=TRUE))
```


```r
plot_IntMean <- ggplot(data=steps_IntMean, aes(x=interval,y=meanInterval))+ geom_line(col="blue", alpha=0.65)+geom_point(col="blue") + labs(title="Averge Daily Activity") + labs(x="5 Minute Interval", y="Mean Steps")
print(plot_IntMean)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
which(steps_IntMean[,2]==max(steps_IntMean[,2]))
```

```
## [1] 104
```
The 104 five minute interval contains the highest average number of steps per day over the data set.

## Imputing missing values
As there are a number of days with intervals that were not recorded it is useful to attempt to fill in the missing values.
### Total Number of Missing Values

```r
StepsisNA <-is.na(actdat[,1])
sum_StepsisNA <-sum(StepsisNA)
numIntervals <-nrow(actdat)
percentNA <- sum_StepsisNA/numIntervals
```
In total of there are 2304 entries missing, that is 0.1311475% of the total number of entries in the data set.

### Filling In Missing Values
To fill in the missing values the average number of steps in a given interval over all days is used to fill in each missing value.

```r
actdat2<-actdat
for(i in 1:nrow(actdat2)){
  if(StepsisNA[i] == TRUE){
    actdat2[i,1]= steps_IntMean[i,2]
  }
}
```
The total number of steps per day can then be histogrammed as was done with the pre-replacement data set.

```r
steps_DailySum2 <- group_by(actdat2,date) %>% summarize(sumsteps=sum(steps,na.rm=TRUE))
sum_steps2 <- ggplot(data=steps_DailySum2, aes(steps_DailySum2$sumsteps),xlab = "Total Steps Per Day")+ geom_histogram(binwidth=500.0, col="red",fill="red",alpha=0.5) + labs(title="Total Steps Per Day Replacing NA with Interval Averages") + labs(x="Total Steps Per Day", y="Count")
print(sum_steps2)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Some changes are seen in this plot compared to the previous one.

```r
ttl_steps_mean2 <- summarize(steps_DailySum2, Mean_DailySum=mean(sumsteps,na.rm=TRUE))
ttl_steps_mean2 <- as.numeric(ttl_steps_mean2[1,1])
ttl_steps_median2 <- summarize(steps_DailySum2, median_DailySum=median(sumsteps,na.rm=TRUE))
ttl_steps_median2 <- as.numeric(ttl_steps_median2[1,1])
```
In the data set where the NA values have been replaced, the mean of the total number of steps counted in a day is 9530.7244046, and the median is 1.0439\times 10^{4}.

```r
diff_ttl_steps_mean <- ttl_steps_mean - ttl_steps_mean2
diff_ttl_steps_median<-  ttl_steps_median - ttl_steps_median2
```
The original data set mean is different from the replaced data set by -176.4948964.  The median has also changed by -44.

Plotting histogram of the difference between the total daily steps of the two data sets shows that while most of the differences are small there is one day where the difference is over 9000, which would appear to be a day that was completely or nearly completely NA values.

```r
diff_steps <- steps_DailySum
diff_steps$diff <- (steps_DailySum$sumsteps - steps_DailySum2$sumsteps)
plot_diff_steps <- ggplot(data=diff_steps, aes(diff_steps$diff))+ geom_histogram(binwidth=100.0, col="red",fill="red",alpha=0.5) + labs(title="Difference Between Original and Replaced Total Steps") + labs(x="Difference Total Steps Per Day", y="Count")
print(plot_diff_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
Using the data set with the replaced values the activity difference between the weekdays, Monday-Friday, and the weekend, Saturday and Sunday, will be examined.

```r
actdat2 <- mutate(actdat2, WeekDay=as.factor(ifelse(weekdays(as.POSIXlt(date)) %in% c("Saturday","Sunday"),"weekend","weekday") ) )
```

```r
steps_IntMean_week <- group_by(actdat2,WeekDay,interval) %>% summarize(meanInterval=mean(steps,na.rm=TRUE))

print(steps_IntMean_week)
```

```
## Source: local data frame [576 x 3]
## Groups: WeekDay [?]
## 
##    WeekDay interval meanInterval
##     <fctr>    <int>        <dbl>
## 1  weekday        0    2.3179245
## 2  weekday        5    0.4584906
## 3  weekday       10    0.1783019
## 4  weekday       15    0.2037736
## 5  weekday       20    0.1018868
## 6  weekday       25    1.5273585
## 7  weekday       30    0.7132075
## 8  weekday       35    1.1716981
## 9  weekday       40    0.0000000
## 10 weekday       45    1.8367925
## # ... with 566 more rows
```


```r
#par(mfrow = c(2, 1))
plot_IntMean <- ggplot(data=steps_IntMean_week, aes(x=interval,y=meanInterval))+ geom_line(col="blue", alpha=0.65)+geom_point(col="blue") + labs(title="Averge Daily Activity") + labs(x="5 Minute Interval", y="Mean Steps") + facet_grid(WeekDay ~ .)
print(plot_IntMean)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

As can be seen the weekday activity has two peaks with an intermediate range between, and the weekend data has a more constant level of activity.
