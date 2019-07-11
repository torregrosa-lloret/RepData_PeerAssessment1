# Main script 

## Load libraries and source functions
library(ggplot2)
library(dplyr)

## Create directories if they don't exist
if(!file.exists("./data")){dir.create("./data")}  # Create data dir if it doesn't exist
if(!file.exists("./figures")){dir.create("./figures")}  # Create data dir if it doesn't exist
if(!file.exists("./data/activity.csv")){  # Unzip data
        unzip("./activity.zip", exdir = "./data")
} 


## Load and preprocess the data
activity <- read.csv("./data/activity.csv")
### First, converts the data.frame into a tibble object. Second, format field 
### "date" into date format.
activityFormat <- activity %>% tbl_df() %>% mutate(
        date = as.Date(as.character(date)))    


## What is the mean total number of steps taken per days?
### Group data by date
by_date <- group_by(activityFormat, date)

### Summarise data according to the date
totalSteps_by_date <- summarise(by_date, totalSteps = sum(steps, na.rm = TRUE))
summaries_by_date <- data.frame(meanSteps=mean(totalSteps_by_date$totalSteps), 
                        medianSteps=median(totalSteps_by_date$totalSteps))

### Plot and save histogram of the steps taken by day
g <- ggplot(totalSteps_by_date, aes(totalSteps, fill=..count..))
g + geom_histogram(binwidth = 2500) + labs( x = "Total number of steps", 
                                            y = "Count",
                                            title = "Total number of steps taken per day") +
        theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "./figures/TotalNumberStepsDay.png")
dev.off()


## What is the average daily activity pattern?
### Group data by interval
by_interval <- group_by(activityFormat, interval)

### Summarise data according to the interval
averageSteps_by_interval <- summarise(by_interval, 
                                      meanSteps = mean(steps, na.rm = TRUE))
### Compute the maximum interval
maxInterval <- filter(averageSteps_by_interval, 
                      meanSteps == max(meanSteps, na.rm = TRUE))

### Plot and save histogram of the steps taken by interval
g <- ggplot(averageSteps_by_interval, aes(interval, meanSteps, color = meanSteps))
g + geom_line(size = 1) + labs( x = "Interval", y = "Average steps",
                                title = "Average daily activity pattern") +
        theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "./figures/AverageNumberStepsAllDay.png")
dev.off()


## Imputing missing values
sum(!complete.cases(activityFormat)) # Number of rows with NA

### Compute the mean steps for each interval
meanInterval <- summarise(by_interval, 
                          mean = mean(steps, na.rm = TRUE))

### Impute the mean value for each interval 
activityImputed <- activityFormat
incompleteCases <- which(!complete.cases(activityImputed))
for (idx in incompleteCases){
        idxInterval <- which(meanInterval$interval == activityImputed[[idx, 3]])
        activityImputed[idx, 1] <- meanInterval[idxInterval, 2]
}

### Group data by date
by_date_imputed <- group_by(activityImputed, date)

### Summarise data according to the date
totalSteps_by_date_imputed <- summarise(by_date_imputed, totalSteps = sum(steps, na.rm = TRUE))
summaries_by_date_imputed <- data.frame(meanSteps=mean(totalSteps_by_date_imputed$totalSteps), 
                                medianSteps=median(totalSteps_by_date_imputed$totalSteps))

### Plot and save histogram of the steps taken by day
g <- ggplot(totalSteps_by_date_imputed, aes(totalSteps, fill=..count..))
g + geom_histogram(binwidth = 2500) + labs( x = "Total number of steps", 
                                            y = "Count",
                                            title = "Total number of steps taken per day") +
        theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "./figures/TotalNumberStepsDay_NoNA.png")
dev.off()


## Are there differences in activity patterns between weekdays and weekends?
activityWeekDays <- weekdays(activityImputed$date)
weekends <- activityWeekDays == "Saturday" | activityWeekDays == "Sunday"
activityWeekDays[weekends] <- "weekend"
activityWeekDays[!weekends] <- "weekday"
activityImputed$day <- as.factor(activityWeekDays)

### Group data by day
activityImputed_by_day <- group_by(activityImputed, day, interval)

### Summarise data
averageSteps_by_day <- summarise(activityImputed_by_day, 
                                      meanSteps = mean(steps, na.rm = TRUE))

### Plot and save histogram of the steps taken by day
g <- ggplot(averageSteps_by_day, aes(interval, meanSteps, color = meanSteps))
g + geom_line(size = 1) + facet_wrap(day ~ ., nrow=2) +
        labs( x = "Interval", y = "Average steps",
                                title = "Differences in activity patterns between weekdays and weekends") +
        theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "./figures/DifferencesActivityPatternsWeekday.png")
dev.off()

