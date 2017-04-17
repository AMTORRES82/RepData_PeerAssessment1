Data loading
------------

    if(!file.exists("repdata%2Fdata%2Factivity.zip")) {
      tempfile <- tempfile()
      download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",tempfile)
      unzip(tempfile)
      unlink(tempfile)
    }
    summary(tempfile)

    ##    Length     Class      Mode 
    ##         1 character character

Histogram, mean and median
--------------------------

    data <- read.csv("activity.csv")
    summary(data)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    stepsday <- aggregate(steps ~ date, data, sum)
    hist(stepsday$steps, main = paste("Total Steps by Day"), col="green", xlab="Number of Steps")

![](PA1_template_files/figure-markdown_strict/explore-1.png)

    rmean <- mean(stepsday$steps)
    rmedian <- median(stepsday$steps)

Time series plot of the average number of steps taken?
------------------------------------------------------

    stepsint <- aggregate(steps ~ interval, data, mean)

    plot(stepsint$interval,stepsint$steps, type="l", col="purple", xlab="Interval", ylab="Number of Steps",main="Average of Steps by Interval")

![](PA1_template_files/figure-markdown_strict/timeserie-1.png)

    max_interval <- stepsint[which.max(stepsint$steps),1]

Code to describe and show a strategy for imputing missing data
--------------------------------------------------------------

    incomplete <- sum(!complete.cases(data))
    imputed_data <- transform(data, steps = ifelse(is.na(data$steps), stepsint$steps[match(data$interval, stepsint$interval)], data$steps))
    imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0

Histogram of the total number of steps taken each day after missing values are imputed
--------------------------------------------------------------------------------------

    stepsday_i <- aggregate(steps ~ date, imputed_data, sum)
    hist(stepsday_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
    hist(stepsday$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
    legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)

![](PA1_template_files/figure-markdown_strict/hist-1.png)

    rmean.i <- mean(stepsday_i$steps)
    rmedian.i <- median(stepsday_i$steps)

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
---------------------------------------------------------------------------------------------------------

    mean_diff <- rmean.i - rmean
    med_diff <- rmedian.i - rmedian

    total_diff <- sum(stepsday_i$steps) - sum(stepsday$steps)

    weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday")
    imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

    stepsint_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

    library(lattice)

    xyplot(stepsint_i$steps ~ stepsint_i$interval|stepsint_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,1), type="l",col="red")

![](PA1_template_files/figure-markdown_strict/mean-1.png)
