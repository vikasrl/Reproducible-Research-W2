unzip(zipfile="activity.zip")
info <- read.csv("activity.csv")
library(ggplot2)
total.steps <- tapply(info$steps, info$date, FUN=sum, na.rm=T)
qplot(total.steps, binwidth=1000, xlab="Total number of steps(each day)")
mean(total.steps, na.rm=T)
median(total.steps, na.rm=T)
library(ggplot2)
averages <- aggregate(x=list(steps=info$steps), by=list(interval=info$interval),FUN=mean, na.rm=TRUE)
ggplot(info=averages, aes(x=interval, y=steps)) + geom_line() + xlab("Interval of 5-Minutes") + ylab("Number of Steps taken (Average)")
averages[which.max(averages$steps),]
missing <- is.na(info$steps)
table(missing)
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.info <- info
filled.info$steps <- mapply(fill.value, filled.info$steps, filled.info$interval)
total.steps <- tapply(filled.info$steps, filled.info$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="Steps taken each Day!!")
mean(total.steps)
median(total.steps)
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("It is a weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("It is a weekend")
  else
    stop("It is an invalid date")
}
filled.info$date <- as.Date(filled.info$date)
filled.info$day <- sapply(filled.info$date, FUN=weekday.or.weekend)
averages <- aggregate(steps ~ interval + day, info=filled.info, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
xlab("Interval of 5-Minutes") + ylab("Number of steps")
