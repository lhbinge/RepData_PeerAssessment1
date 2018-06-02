
setwd("C:/Users/Laurie/OneDrive/Documents/BING/METRICS/R Coursera/data")

##Loading and preprocessing the data
if(!file.exists("activity.csv")){
    temp <- tempfile()
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl, temp)
    data <- read.csv(unz(temp, "activity.csv"))
    unlink(temp)
} else {
    data <- read.csv("activity.csv")
}
data[,2] <- as.POSIXct(as.character(data$date), format="%Y-%m-%d") 
head(data)
str(data)

##What is mean total number of steps taken per day?
#Ignore the missing values in the dataset
#Calculate the total number of steps taken per day
total_steps <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
total_steps[,1] <- as.POSIXct(as.character(total_steps$Group.1), 
                              format="%Y-%m-%d") 
names(total_steps) <- c("Date", "x")

#Make a histogram of the total number of steps taken each day
library(ggplot2)
qplot(x, data=total_steps, geom="histogram", xlab = "Total Steps Per Day")
#Or: m <- ggplot(total_steps, aes(x=x))
#m + geom_histogram()

#Calculate & report the mean & median of  total number of steps per day
mean(total_steps$x)
median(total_steps$x)

##What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
ave_steps <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
names(ave_steps) <- c("Interval", "x")
qplot(Interval, x, data=ave_steps, geom="line", ylab = "Average Steps")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
max_steps = subset(ave_steps, ave_steps$x==max(ave_steps$x)) 
paste(as.character(max_steps[,1]),"- 840")

##Imputing missing values
#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
sum(is.na(data$steps))

#Devise a strategy for filling in all of the missing values in the dataset. 
#E.g, you could use the mean for that 5-minute interval.
#Create a new dataset that is equal to the original dataset but with the 
#missing data filled in.
new_data <- data
for(i in 1:nrow(data)) {
    if(is.na(data[i,1])) {
        interv <- data[i,3]
        for(k in 1:nrow(ave_steps)) {
            if(ave_steps[k,1]==interv) { repl <- ave_steps[k,2] }
        }
        new_data[i,1] <- repl
    }
}

#Make a histogram of the total number of steps taken each day 
new_total_steps <- aggregate(new_data$steps, by=list(new_data$date), 
                             FUN=sum, na.rm=TRUE)
new_total_steps[,1] <- as.POSIXct(as.character(new_total_steps$Group.1), 
                                  format="%Y-%m-%d") 
names(new_total_steps) <- c("Date", "x")
qplot(x, data=new_total_steps, geom="histogram", xlab = "Total Steps Per Day")

#Calculate & report the mean and median total number of steps taken per day
mean(new_total_steps$x)
median(new_total_steps$x)

#Do these values differ from the estimates in the first part of the assignment? 
#What is the impact of imputing missing data on the estimates 
#of the total daily number of steps?
mean(new_total_steps$x) - mean(total_steps$x)
median(new_total_steps$x) - median(total_steps$x)

##Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help 
#Use the dataset with the filled-in missing values for this part.

#Create a new factor variable with two levels: “weekday” and “weekend” 
data$days <- factor((weekdays(new_data$date) %in% c("Sunday","Saturday")), 
               levels = c(TRUE,FALSE), labels=c("weekend", "weekday"))

               
#Make a panel plot containing a time series plot (i.e. type = "l") 
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). 
new_ave_steps <- aggregate(data$steps, by=list(data$interval,data$days), 
                           FUN=mean, na.rm=TRUE)
names(new_ave_steps) <- c("Interval", "Day", "x")
qplot(Interval, x, data=new_ave_steps, geom="line", facets = Day ~ ., 
      ylab = "Average Number of Steps")


##============================================================================
## Other students

day_total_steps <- tapply(data_non_NA$steps, data_non_NA$date,sum)

five_min_int <- tapply(data_non_NA$steps, data_non_NA$interval,mean)

imputed <- data
imputed[!complete.cases(imputed), "steps"] <- five_min_int[as.character(imputed[!complete.cases(imputed), 
                                                                                "interval"])]
                       
wdays <- weekdays(as.Date(imputed$date))
wdays[wdays == "domingo" | wdays == "s�bado"] <- "weekend"
wdays[wdays != "weekend"] <- "weekday"
imputed <- data.frame(imputed, wdays)

library(reshape2)
five_min_int_wday <- with(imputed, tapply(steps, list(interval, wdays), mean))
melted <- melt(five_min_int_wday, id = row.names(five_min_int_wday))
library(ggplot2)
ggplot(melted, aes(x = Var1, y = value, col = Var2)) + geom_line(size = 0.75) + 
    facet_grid(Var2 ~ .) + xlab("5 minute interval") + ylab("Average number of steps") + 
    ggtitle("Average number of steps by 5 minute interval") + theme(legend.position = "none")                       
