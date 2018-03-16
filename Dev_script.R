# Getting and cleaning data course project 


# Initiation
rm(list=ls())       # Clear data
gc()                # Clear memory R
# Set Working directory
oldwd <- getwd()
setwd("D:/NoBackup/Box/Box Sync/Coursera/Git/Repr_Research_wk2")
#library(downloader)
#library(ggplot2)
library(dplyr)

# Loading and preprocessing the data
## Show any code that is needed to:
### 1. Load the data (i.e. read.csv())
Url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
download.file(Url,destfile="repdata%2Fdata%2Factivity.zip",mode = "wb") 
unzip(zipfile="repdata%2Fdata%2Factivity.zip") 
df <- read.csv('activity.csv', sep=',')
### 2. Process/transform the data (if necessary) into a format suitable for your analysis
df$date <- as.Date(df$date, "%Y-%m-%d")
df$time <- format(strptime(sprintf("%04d", df$interval), format="%H%M"), format = "%H:%M")
df$datetime <- as.POSIXct(paste(df$date, df$time), format="%Y-%m-%d %H:%M")
df$mincounter <- df$interval%%100/5 + floor(df$interval/100)*12+1

# What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
### 1. Calculate the total number of steps taken per day
df_sum <- aggregate(df$steps, by=list(Category=df$date), FUN=sum)
colnames(df_sum) <- c("Day", "SumNoSteps")
### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. 
###    Make a histogram of the total number of steps taken each day
hist(df_sum$SumNoSteps, breaks = 25, main="Number of days and steps taken",xlab="Categorical index of steps taken", 
     ylab="Number of days", col='green')
### 3. Calculate and report the mean and median of the total number of steps taken per day
df_mean <- aggregate(df$steps, by=list(Category=df$date), FUN=mean)
colnames(df_mean) <- c("Day", "MeanNoSteps")
df_median <- aggregate(df$steps, by=list(Category=df$date), FUN=median)
colnames(df_median) <- c("Day", "MedianNoSteps")
# Not necessary?
barplot(df_mean$x, main="Mean number of steps per day",xlab="Day", ylab="Number of steps", col='green', 
        names.arg=df_mean$Category)
barplot(df_median$x, main="Median number of steps per day",xlab="Day", ylab="Number of steps", col='green', 
        names.arg=df_median$Category)



# What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across 
###    all days (y-axis)
df_interval <-df[complete.cases(df),]
df_interval <- aggregate(df_interval$steps, by=list(Category=df_interval$mincounter), FUN=mean)
colnames(df_interval) <- c("time", "MeanNoSteps")
df_interval$MeanNoSteps <- round(df_interval$MeanNoSteps)
plot(df_interval$MeanNoSteps~df_interval$time, main = "Average steps per day", xlab = "Time of the day", ylab = "Number of steps", 
     xaxt="n", type = "l")
axis(1, at=1:289, labels=df$time[1:289])
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
MaxMincounter <- subset(df_interval$time, df_interval$MeanNoSteps == max(df_interval$MeanNoSteps))
MaxTime <- df[!duplicated(df$mincounter==MaxMincounter),4][[2]]



# Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce 
## bias into some calculations or summaries of the data.

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NoRowsNa <- sum(is.na(df$steps))
### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For 
###    example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Strategy:
# Subsitute by mean of that 5-minute interval

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
df_ImputeNa <- df %>% 
    group_by(mincounter) %>% 
    mutate(steps= ifelse(is.na(steps), round(mean(steps, na.rm=TRUE)), steps))

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of 
###    steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing 
###    missing data on the estimates of the total daily number of steps?
df_sumImpute <- aggregate(df_ImputeNa$steps, by=list(Category=df_ImputeNa$date), FUN=sum)
colnames(df_sumImpute) <- c("Day", "SumNoSteps")
hist(df_sumImpute$SumNoSteps, breaks = 25, main="Number of days and steps taken",xlab="Categorical index of steps taken", 
     ylab="Number of days", col='green')
df_meanImpute <- aggregate(df_ImputeNa$steps, by=list(Category=df_ImputeNa$date), FUN=mean)
colnames(df_meanImpute) <- c("Day", "MeanNoSteps")
df_medianImpute <- aggregate(df_ImputeNa$steps, by=list(Category=df_ImputeNa$date), FUN=median)
colnames(df_medianImpute) <- c("Day", "MedianNoSteps")

# Write down what the impact is
# The impact is that the average is added to several days and time intervals, therefore the total number of steps and total average has changed.
# The histograms shows this too (average peak is higher)



# Are there differences in activity patterns between weekdays and weekends?
## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday 
###    or weekend day.
weekdaysDutch <- c('maandag', 'dinsdag', 'woensdag', 'donderdag', 'vrijdag')
df_ImputeNa$weekday <- factor((weekdays(df_ImputeNa$date) %in% weekdaysDutch), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps 
###    taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of 
###    what this plot should look like using simulated data.
# Data preperation for weekday
df_intervalWeekday <- aggregate(df_ImputeNa$steps[df_ImputeNa$weekday=='weekday'], 
                                by=list(Category=df_ImputeNa$mincounter[df_ImputeNa$weekday=='weekday']), FUN=mean)
colnames(df_intervalWeekday) <- c("time", "MeanNoStepsWeekday")
df_intervalWeekday$MeanNoStepsWeekday <- round(df_intervalWeekday$MeanNoStepsWeekday)

# data preperation for weekend
df_intervalWeekend <- aggregate(df_ImputeNa$steps[df_ImputeNa$weekday=='weekend'], 
                                by=list(Category=df_ImputeNa$mincounter[df_ImputeNa$weekday=='weekend']), FUN=mean)
colnames(df_intervalWeekend) <- c("time", "MeanNoStepsWeekend")
df_intervalWeekdend$MeanNoStepsWeekend <- round(df_intervalWeekend$MeanNoStepsWeekend)

par(mfrow=c(2,1)) 
plot(df_intervalWeekday$MeanNoStepsWeekday~df_intervalWeekday$time, main = "Average steps per weekday", xlab = "No 5th minute of the day", 
     ylab = "Time of the day", xaxt="n", type = "l")
axis(1, at=1:289, labels=df_ImputeNa$time[1:289])
plot(df_intervalWeekend$MeanNoStepsWeekend~df_intervalWeekend$time, main = "Average steps per day in the weekend", 
     xlab = "Time of the day", ylab = "Number of steps", xaxt="n", type = "l")
axis(1, at=1:289, labels=df_ImputeNa$time[1:289])



# To old working directory
setwd(oldwd)

# References:
# https://stackoverflow.com/questions/25272457/convert-an-integer-column-to-time-hhmm
# https://stackoverflow.com/questions/11609252/r-tick-data-merging-date-and-time-into-a-single-object
# https://stackoverflow.com/questions/11369961/subset-with-unique-cases-based-on-multiple-columns
# https://stackoverflow.com/questions/26336122/r-replacing-na-values-by-mean-of-hour-with-dplyr