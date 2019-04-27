# useful packages
library(dplyr)
library(lattice)
# lod data
activity <- read.csv("activity.csv")

# total steps
total_steps<- activity %>% group_by(date) %>% summarize(total_day = sum(steps, na.rm = TRUE))

# histogram
png("prplot1.png")
hist(total_steps$total_day, main = " Histogram of the total number of steps each day", xlab = "Total Number of steps", col = " lightblue")
dev.off()

# Mean and Median
m1 <- mean(total_steps$total_day)
m1
Med1 <- median(total_steps$total_day)
Med1

##Time serie Plot
avg_steps<- activity %>% group_by(interval) %>% summarize(avg_day = mean(steps, na.rm = TRUE))
png("Projplot2.png")
plot(avg_steps$interval,avg_steps$avg_day, type = "l",col="blue",xlab="Time", ylab=" Average", main=" AVerage Steps by days")
u <- which(avg_steps$avg_day==max(avg_steps$avg_day,na.rm = TRUE ))

points(x=avg_steps$interval[u],y=max(avg_steps$avg_day,na.rm=TRUE),type="p",pch=19,col="green")
points(x=avg_steps$interval[u],y=0,type="p",pch=19,col="green")
points(x=0,y=max(avg_steps$avg_day,na.rm=TRUE),type="p",pch=19,col="green")

segments(0,max(avg_steps$avg_day,na.rm = TRUE),avg_steps$interval[u],max(avg_steps$avg_day,na.rm = TRUE),col = "green", lty = 5)
segments(avg_steps$interval[u],0,avg_steps$interval[u],max(avg_steps$avg_day,na.rm = TRUE), col="green", lty = 5)
grid(30,30)
dev.off()

# interval bounds
avg_steps$interval[u]

#Total number of row with NA
totNa <- length(which(is.na(activity$steps)))
totNa

#Strategy to fill out the NA
activity$steps[which(is.na(activity$steps))] <- rep(round(mean(activity$steps, na.rm = TRUE),0),totNa)

#3. Histogram
total_st<- activity %>% group_by(date) %>% summarize(tot_day = sum(steps, na.rm = TRUE))
png("Projplot3.png")
hist(total_st$tot_day, main = " New Histogram of the total number of steps each day", xlab = "Total Number of steps", col = " lightblue")
dev.off()

# Mean 
m2 <- mean(total_st$tot_day)
m2

# Median
Med2 <- median(total_st$tot_day)
Med2

# The Impact of the Imputs
abs(m1-m2) # for the means
abs(Med1-Med2) # for the Median

#Weekday and Weekend factors
activity$week <- ifelse(weekdays(as.Date(levels(activity$date))[activity$date]) %in% c("samedi", "dimanche"), "weekend", "weekday")

#Time serie plot
avg_steps2<- activity %>% group_by(interval,week) %>% summarize(avg_day = mean(steps, na.rm = TRUE))

png("Projplot4.png")
xyplot(avg_steps2$avg_day~avg_steps2$interval|avg_steps2$week, avg_steps2,layout=c(1,2), type="l", xlab = "Time", ylab="Average")
dev.off()

