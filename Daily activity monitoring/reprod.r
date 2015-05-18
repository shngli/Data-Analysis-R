# Author: Chisheng Li

## Step 1: Load and preprocess the data
activity <- read.csv("activity.csv", header = TRUE)

#Convert data to Date class
activity$date <- as.Date(as.character(activity$date))

str(activity)
#'data.frame':   17568 obs. of  3 variables:
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : Date, format: "2012-10-01" "2012-10-01" ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

summary(activity)
#     steps             date               interval     
# Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
# 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
# Median :  0.00   Median :2012-10-31   Median :1177.5  
# Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
# 3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
# Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
# NA's   :2304 

###########################

## Step 2: What is mean total number of steps taken per day?
# You can ignore the missing values in the dataset.

## 1. Make a histogram of the total number of steps taken each day
# Include the days where the total number of steps taken is 0
library(plyr)
library(ggplot2)

stepsPerDayNA <- ddply(activity, "date", summarise, totalSteps = sum(steps, na.rm=T))

cuts1 <- data.frame(Thresholds="Mean", vals = mean(stepsPerDayNA$totalSteps))
cuts2 <- data.frame(Thresholds="Median", vals = median(stepsPerDayNA$totalSteps))
cuts <- rbind(cuts1,cuts2)

png("/histogram2a.png", height=600, width=600)
ggplot(data = stepsPerDayNA, aes(x = stepsPerDayNA$totalSteps)) + geom_histogram() + 
    geom_vline(data=cuts, aes(xintercept=vals, linetype=Thresholds, colour = Thresholds), show_guide = TRUE) + 
    xlab("Total number of steps") + ggtitle("Total Number of Steps Taken Per Day (include missing values)")
dev.off()
## 2. Report the mean and median total number of steps taken per day
mean(stepsPerDayNA$totalSteps)
#[1] 9354.23
median(stepsPerDayNA$totalSteps)
#[1] 10395


## Alternative solution when ignore the missing values
stepsPerDay <- ddply(activity, "date", summarise, totalSteps = sum(steps))

cuts1 <- data.frame(Thresholds="Mean", vals = mean(stepsPerDay$totalSteps, na.rm=T))
cuts2 <- data.frame(Thresholds="Median", vals = median(stepsPerDay$totalSteps, na.rm=T))
cuts <- rbind(cuts1,cuts2)

png("/histogram2b.png", height=600, width=600)
ggplot(data = stepsPerDay, aes(x = stepsPerDay$totalSteps)) + geom_histogram() + 
    geom_vline(data=cuts, aes(xintercept=vals, linetype=Thresholds, colour = Thresholds), show_guide = TRUE) + 
    xlab("Total number of steps") + ggtitle("Total Number of Steps Taken Per Day (exclude missing values)")
dev.off()

mean(stepsPerDay$totalSteps, na.rm=T)
#[1] 10766.19
median(stepsPerDay$totalSteps, na.rm=T)
#[1] 10765

#or stepsPerDay <- tapply(activity$steps, activity$date, sum)
# mean(stepsPerDay, na.rm=T)
# median(stepsPerDay, na.rm=T)

###########################

## Step 3: What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
intervalavg <- ddply(activity, "interval", summarise, avgSteps = mean(steps, na.rm=T))

summary(intervalavg)
#    interval         avgSteps      
# Min.   :   0.0   Min.   :  0.000  
# 1st Qu.: 588.8   1st Qu.:  2.486  
# Median :1177.5   Median : 34.113  
# Mean   :1177.5   Mean   : 37.383  
# 3rd Qu.:1766.2   3rd Qu.: 52.835  
# Max.   :2355.0   Max.   :206.170  
mean(intervalavg$avgSteps)
#[1] 37.3826
median(intervalavg$avgSteps)
#[1] 34.11321

png("/timeseries3.png", height=600, width=600)
ggplot(data=intervalavg, aes(x=intervalavg$interval, y=intervalavg$avgSteps)) + geom_line() + 
    ggtitle("Average Daily Activity Pattern Per 5-min Interval") + xlab("Interval (24-hours)") + 
    ylab("Average Number of Steps Taken")
dev.off()

# 2. Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
intervalavg[which.max(intervalavg$avgSteps),]
#    interval avgSteps
#104      835 206.1698

###########################

## Step 4: Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)

table((is.na(activity$steps)))

#FALSE  TRUE 
#15264  2304 

#or sum(is.na(activity$steps))
#[1] 2304

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# 3. Create a new dataset that is equal to the original dataset but with the
# missing data filled in.

# Fill all missing values with the average value for that 5-min interval
averages <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), mean, na.rm=TRUE)

fill_value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[intervalavg$interval==interval, "steps"])
    return(filled)
}
activityFilled <- activity
activityFilled$steps <- mapply(fill_value, activityFilled$steps, activityFilled$interval)

# 4. Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day. Do
# these values differ from the estimates from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total
# daily number of steps?
stepsPerDayFilled <- ddply(activityFilled, "date", summarise, totalSteps = sum(steps))

cuts1 <- data.frame(Thresholds="Mean", vals = mean(stepsPerDayFilled$totalSteps))
cuts2 <- data.frame(Thresholds="Median", vals = median(stepsPerDayFilled$totalSteps))
cuts <- rbind(cuts1,cuts2)

png("/histogram4.png", height=600, width=600)
ggplot(data = stepsPerDayFilled, aes(x = stepsPerDayFilled$totalSteps)) + geom_histogram() + 
    geom_vline(data=cuts, aes(xintercept=vals, linetype=Thresholds, colour = Thresholds), show_guide = TRUE) + 
    xlab("Total number of steps") + ggtitle("Total Number of Steps Taken Per Day (missing values filled)")
dev.off()

mean(stepsPerDayFilled$totalSteps)
#[1] 10766.19
median(stepsPerDayFilled$totalSteps)
#[1] 10766.19

# Alternative
#totalStepsFilled <- tapply(activityFilled$steps, activityFilled$date, sum)
#mean(totalStepsFilled)
#median(totalStepsFilled)

###########################

## Step 5: Are there differences in activity patterns between weekdays and weekends?
# Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday”
# and “weekend” indicating whether a given date is a weekday or weekend day.
dayofWeek <- ifelse(weekdays(activityFilled$date)=="Saturday" | weekdays(activityFilled$date)=="Sunday","weekend","weekday")
activityFilled$day <- as.factor(dayofWeek)

#averages <- aggregate(steps ~ interval + day, data=activityFilled, mean)

dayActivity <- ddply(activityFilled, c("interval","day"), summarise, avgSteps=mean(steps))

# 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or 
# weekend days (y-axis).
png("/timeseries5.png", height=600, width=600)
ggplot(dayActivity, aes(interval, avgSteps)) + geom_line(aes(colour=day)) + facet_grid(day ~ .) + 
    ggtitle("Average Daily Activity Per 5-Min Interval (Weekday vs Weekend)") + xlab("interval (24-hours)") + 
    ylab("Average Number of Steps Taken")
dev.off()
