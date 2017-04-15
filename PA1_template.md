---
title: "PA1_template.Rmd"
author: "Youngkeun Yoon"
date: "2017년 4월 15일"
output: html_document
---

### What is mean total number of steps taken per day?

```r
data <- read.csv("activity.csv")
stepsperdate <- aggregate(steps~date, data, sum, na.rm=TRUE)
print(stepsperdate)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
hist(stepsperdate$steps, breaks=17, xlab="Total Number of Steps Taken Per day")  
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
median(stepsperdate$steps)
```

```
## [1] 10765
```

```r
mean(stepsperdate$steps)
```

```
## [1] 10766.19
```
### What is the average daily activity pattern?



```r
avgperinterval <- aggregate(steps~interval, data, mean, na.rm=TRUE)
plot(avgperinterval, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
avgperinterval[avgperinterval$steps==max(avgperinterval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
sum(is.na(data))
```

```
## [1] 2304
```
### Imputing missing values



```r
data2 <- data
narows <- is.na(data2$steps)
table(narows)
```

```
## narows
## FALSE  TRUE 
## 15264  2304
```

```r
datafornarows <- tapply(data2$steps, data2$interval, mean, na.rm=TRUE, simplify=T)
data2$steps[narows] <- datafornarows[as.character(data2$interval[narows])]

nonadata <- tapply(data2$steps, data2$date, sum, simplify=T)
hist(nonadata, breaks=17, xlab="Total Number of Steps Taken Per day(imputed)")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
Before imputation : Median(10765), Mean(10766.19)
After imputation : Median(10766.19), Mean(10766.19)
Imputation caused the median and mean to become identical. 

```r
median(nonadata)
```

```
## [1] 10766.19
```

```r
mean(nonadata)
```

```
## [1] 10766.19
```
### Are there differences in activity patterns between weekdays and weekends?
I've coded Saturday and Sunday in Korean because of my system default language. 

```r
for (i in 1:nrow(data2)) {
        if (weekdays(as.Date(data2$date[i])) == "토요일" | weekdays(as.Date(data2$date[i])) == "일요일") {
                data2$daytype[i] = "weekend"
        } else {
                data2$daytype[i] = "weekday"
        }
}

        
weekenddata2 <- data2[data2$daytype=="weekend", ]
tapweekenddata2 <- aggregate(steps~interval, weekenddata2, mean, na.rm=TRUE)
weekdaydata2 <- data2[data2$daytype=="weekday", ]
tapweekdaydata2 <- aggregate(steps~interval, weekdaydata2, mean, na.rm=TRUE)
plot(tapweekenddata2, type="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
plot(tapweekdaydata2, type="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)
