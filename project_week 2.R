# Load all required library ----------
library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)

# Set work direct ----------
setwd("C:/Users/Asus/Desktop/Data Science/Course/Course 5_Reproducible research")

##Q1: Loading and preprocessing the data ----------
# Load data
activ_raw <- read.csv("activity.csv", 
                      header=TRUE, 
                      na.strings = "NA")

# Cleaning data
activ_raw$date <- ymd(activ_raw$date)

activ_clean <- na.omit(activ_raw)

# Quick check
summary(activ_clean)

str(activ_clean)

head(activ_clean)

tail(activ_clean)

##Q2 What is mean total number of steps taken per day? ------

#Histogram 
# Plot histogram
activ_histog <- summarize(group_by(activ_clean,date),
                          daily.step=sum(steps))

plot.steps.day <- ggplot(activ_histog, 
                         aes(x=daily.step)) + 
        geom_histogram(binwidth = 1000, 
                       aes(y=..count.., fill=..count..)) + 
        geom_vline(xintercept=mean.activity, 
                   colour="black", 
                   linetype="dashed", 
                   size=1) +
        geom_vline(xintercept=median.activity, 
                   colour="white" , 
                   linetype="dotted", 
                   size=1) +
        labs(title="Histogram of Number of Steps taken each day", 
             y="Frequency", 
             x="Daily Steps") 
plot.steps.day

# Calculate
mean.activity <- as.integer(mean(activ_histog$daily.step))
mean.activity
# Answer: 10766
median.activity <- as.integer(median(activ_histog$daily.step))
median.activity
# Answer: 10765

##Q3: What is the average daily activity pattern? ------

# Plot average number of steps by 5-minute interval
activ_series <- activ_clean %>% 
        group_by(interval) %>% 
        summarize(mean.step=mean(steps))

plot.step.interval <- ggplot(activ_series, 
                             aes(x=interval,y=mean.step)) + 
        geom_line(color="blue") + 
        labs(title="Average Number of Steps Taken vs 5-min Interval", 
             y="Average Number of Steps", 
             x="5-min Interval Times Series")
plot.step.interval

# Average across all the days in the dataset, contains the maximum number of steps?
optimal <- which.max(activ_series$mean.step)
optimal.step <- activ_series$interval[optimal]
sprintf("Maximum number of steps is coming from %gth 5-min interval", 
        optimal.step)
# "Maximum number of steps is coming from 835th 5-min interval"

##Q4: Imputing missing values ------

# Calculate missing values 
sum(is.na(activ_raw))

# Devise a strategy 
activ_fix <- activ_raw
activ_fix$steps[is.na(activ_fix$steps)] <- mean(activ_fix$steps,na.rm=TRUE)
activ_fix$steps <- as.numeric(activ_fix$steps)
activ_fix$interval <- as.numeric(activ_fix$interval)
colSums(is.na(activ_fix))

# Create a new dataset 
summary(activ_fix)

# 4 Make a histogram 

# Summarize data by date
activ_fix2 <- summarize(group_by(activ_fix,date),
                        daily.step=sum(steps))

mean.impute<- as.integer(mean(activ_fix2$daily.step))
mean.impute
## [1] 10766
median.impute <- as.integer(median(activ_fix2$daily.step))
median.impute
## [1] 10766
# Plot histogram
plot.steps.day <- ggplot(activ_fix2,
                         aes(x=daily.step)) + 
        geom_histogram(binwidth = 1000, 
                       aes(y=..count.., fill=..count..)) + 
        geom_vline(xintercept=mean.impute, 
                   colour="red", 
                   linetype="dashed", 
                   size=1) +
        geom_vline(xintercept=median.impute, 
                   colour="green" , 
                   linetype="dotted", 
                   size=1) +
        labs(title="Histogram of Number of Steps taken each day (Fix missing)", 
             y="Frequency", 
             x="Daily Steps")
plot.steps.day

##Q5: Are there differences in activity patterns between weekdays and weekend? ------

# 1 Create a new factor variable 

activ_fix$day <- ifelse(weekdays(activ_fix$date) %in% 
                                      c("Saturday","Sunday"), "weekday", "weekend")

# 2 Make a panel plot 

impute.df <- activ_fix %>% 
        group_by(interval,day) %>% 
        summarise(mean.step=mean(steps))

# Plot Average steps across weekday/weekend vs 5-min interval Time Series
plot.weekday.interval <- ggplot(impute.df, 
                                aes(x=interval, 
                                    y=mean.step, 
                                    color=day)) + 
        facet_grid(day~.) +
        geom_line() + 
        labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", 
             y="Average Number of Steps", 
             x="5-min Interval Times Series")
plot.weekday.interval


