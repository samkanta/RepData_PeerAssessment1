
  # Loading and preprocessing the data


library(ggplot2)#needed for plots
library(xtable)#needed for visual presentation of table 

unzip(zipfile="activity.zip")
df.1 <- read.csv("activity.csv")

h.noproc <- head(df.1, 5)
h.noproc <- xtable(h.noproc, caption="First 5 rows: non-processed", label="Head Xtable", digits=1)
print(h.noproc, include.rownames = TRUE, caption.placement="top")

df <- na.omit(df.1)
h.proc <- head(df, 5)
h.proc <- xtable(h.proc, caption="First 5 rows: processed", label="HeadP Xtable", digits=1)
print(h.proc, include.rownames = TRUE, caption.placement="top")

## What is mean total number of steps taken per day?

df.steps <- aggregate(steps ~ date, df, sum)
##Raw Histogram
hist(df.steps$steps, col="lightblue", main = "Histogram of Total # Steps Taken Each Day", 
     xlab="Total Number of Steps in a Day")

##New Bins set
qplot(steps, data=df.steps, binwidth = "1", xlab = "Total number of steps taken each day", 
      main = "Steps with binwidth set at 1", na.rm=TRUE) + 
  geom_histogram(colour="darkgreen", aes(fill = ..count..)) 

mean(df.steps$steps)

median(df.steps$steps)

desc <- summary(df.steps) #from the psych library
desc <- xtable(desc, caption="Summary Statistics for Data", 
               label="Description Xtable", digits=1)
print(desc, include.rownames = TRUE, caption.placement="top")

## What is the average daily activity pattern?

library(ggplot2)
df.averages <- aggregate(x=list(steps=df$steps), by=list(interval=df$interval), FUN=mean)
ggplot(data=df.averages, aes(x=interval, y=steps)) + geom_line() + 
  xlab("Intervals set at 5 minutes") + ylab("Average of steps taken")

df.averages[which.max(df.averages$steps),]

## Imputing missing values

df.missing <- is.na(df$steps)
num.missing <- sum(df.missing)
table(df.missing)
table (num.missing)

nafiller <- function(steps, interval){
  filler <- NA
  if (!is.na(steps))
    filler <- c(steps)
  else
    filler <- (df.averages[df.averages$interval==interval, "steps"])
  return(filler)
}
myfill.df <- df
myfill.df$steps <- mapply(nafiller, myfill.df$steps, myfill.df$interval)

head(myfill.df)

myts <- tapply(myfill.df$steps, myfill.df$date)


qplot(myts, binwidth=5, xlab="Total Number of Steps per Day",
      main="Total Number of Steps per Day After Imputation" )

library(psych)
describe(myts)
mean(myts)
median(myts)
summary(myts)


## Are there differences in activity patterns between weekdays and weekends?

week.identify <- function(date){
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("Weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("Weekend")
  else
    stop("Invalid Date")
}  
myfill.df$date <- as.Date(myfill.df$date)
myfill.df$day <- sapply(myfill.df$date, FUN=week.identify)
head(myfill.df$day)

avg <- aggregate(steps ~ interval + day, data=myfill.df, mean)
ggplot(avg, aes(interval, steps))+geom_line()+ facet_grid(day ~ .) + xlab("Intervals at 5 minutes") + ylab("# of Steps")