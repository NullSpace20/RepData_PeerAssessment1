------------------------------------------------------------------------

title: "Reproducible Research Project 1"

author: "Mahmoud Samy"

date: "Dec 16, 2018"

output: md\_document

fig\_caption: yes

keep\_md: yes

toc: yes

pdf\_document: default

self\_contained: no

------------------------------------------------------------------------

Reproducible Research: First Programming Assinment
==================================================

    library(ggplot2)
    library(scales)
    library(Hmisc)

Loading and preprocessing the data
----------------------------------

    if(!file.exists('./data/activity.csv')){
        unzip('./data/activity.zip')
    }
    activityData <- read.csv('./data/activity.csv')
    View(activityData)

What is mean total number of steps taken per day?
-------------------------------------------------

    stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
    stepsByDay

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##          0        126      11352      12116      13294      15420 
    ## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
    ##      11015          0      12811       9900      10304      17382 
    ## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
    ##      12426      15098      10139      15084      13452      10056 
    ## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
    ##      11829      10395       8821      13460       8918       8355 
    ## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
    ##       2492       6778      10119      11458       5018       9819 
    ## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
    ##      15414          0      10600      10571          0      10439 
    ## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
    ##       8334      12883       3219          0          0      12608 
    ## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
    ##      10765       7336          0         41       5441      14339 
    ## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
    ##      15110       8841       4472      12787      20427      21194 
    ## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
    ##      14478      11834      11162      13646      10183       7047 
    ## 2012-11-30 
    ##          0

### Make a histogram of the total number of steps taken each day

    aggData <- aggregate(steps ~ date, data = activityData, FUN = sum)
    hist(aggData$step, xlab = "Total of Steps", main = "Total of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Calculate and report the mean and median total number of steps taken per day

    stepsByDayMean <- mean(stepsByDay)
    stepsByDayMedian <- median(stepsByDay)

-   Mean: 9354.2295082
-   Median: 10395

### Make a time series plot

    aggData2 <- aggregate(steps ~ interval, data = activityData, FUN = mean, na.rm = TRUE)

    plot(aggData2$steps ~ aggData2$interval,type = "l", xlab ="Time (5-minute intervals)", ylab="Average of steps taken across all days", main="Average of steps taken for each interval across all days")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    aggData2$interval[which.max(aggData2$steps)]

    ## [1] 835

    sum(is.na(activityData))

    ## [1] 2304

\#\#My strategy is to fill up all the missing values with the mean for
that 5-minute interval.

    fillingNA <- function(data, naSub)
    {
            size <- nrow(data)
            
            for(i in 1:size)
            {
                    if(is.na(data$steps[i]))
                    {
                            # Replacing the NA for the 5-minute interval mean
                            data$steps[i] = naSub$steps[naSub$interval == data$interval[i]]
                    }
            }
            data
    }

    newDataSet <- fillingNA(activityData,aggData2)

-   Number of missing values on the new data set.

<!-- -->

    sum(is.na(newDataSet))

    ## [1] 0

-   It was created a new subset of the data without missing values. This
    subset is the result of aggregating the total of steps by day.

<!-- -->

    newDataSetaggData <- aggregate(steps ~ date, data = newDataSet, FUN = sum)

-   Drawing the histogram of the total number of steps:

<!-- -->

    hist(newDataSetaggData$step, xlab = "Total of Steps", main = "Total of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)

-   Calculating the mean total number of steps taken per day of the data
    without missing values

<!-- -->

    mean(newDataSetaggData$step)

    ## [1] 10766.19

-   Calculating the median total number of steps taken per day of the
    data without missing values

<!-- -->

    median(newDataSetaggData$step)

    ## [1] 10766.19

The days with only missing values on the original data:

    testData <- activityData  # Making a copy of the original data
    testData[is.na(testData)] <- 0 # Replace NA by zeros
    x <- aggregate(steps ~ date, data = testData, FUN = sum) # Agg by data
    x[x$step == 0,]    # Days with zero number of steps

    ##          date steps
    ## 1  2012-10-01     0
    ## 8  2012-10-08     0
    ## 32 2012-11-01     0
    ## 35 2012-11-04     0
    ## 40 2012-11-09     0
    ## 41 2012-11-10     0
    ## 45 2012-11-14     0
    ## 61 2012-11-30     0

    transfWeekDays <- function(oldData)
    {
            oldData$weekDays <- weekdays(as.Date(oldData$date))
            size <- nrow(oldData)
            
            for(i in 1:size)
            {
                    if(oldData$weekDays[i] == "Saturday" | oldData$weekDays[i] == "Sunday")
                    {
                            oldData$weekDays[i] <- "weekend"
                    }
                    else
                    {
                            oldData$weekDays[i] <- "weekday"
                    }
            }
            oldData              
    }
    newDataSet <- transfWeekDays(newDataSet)

-   It was created a new subset of the data without missing values. This
    subset is the result of aggregating the total of steps by interval
    and weekday.

<!-- -->

    aggData3 <- aggregate(steps ~ interval + weekDays, data = newDataSet, FUN = mean, na.rm = TRUE)

    library(lattice)
    xyplot(steps ~ interval | weekDays, data = aggData3, layout=c(1,2), type ="l", main ="Average number of steps taken across all days")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-20-1.png)
