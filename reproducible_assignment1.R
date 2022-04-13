library(dplyr)
library(ggplot2)

setwd("C:/Users/asus/Desktop/Reproducible Research")

activity_with_na<-read.csv("activity.csv")

str(activity_with_na)

#what is the mean total number of steps taken per day

#1-Calculate the total number of steps taken per day
steps<-activity_with_na%>%
  filter(!is.na(steps))%>%
  group_by(date)%>%
  summarize(daily_steps=sum(steps))

#2-If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

ggplot(steps,aes(daily_steps,fill=..count..))+geom_histogram(binwidth = 2000,col="dark orange")+
  scale_y_continuous(breaks = seq(0,13,by=2))+labs(x="steps taken each day",
                                                   title="Histogram of the total number of steps talen each day")+
  scale_fill_gradient("count",low="green",high = "orange")

dev.copy(png,file="plot1.png",height=480,width=480)

dev.off()

#3-Calculate and report the mean and median of the total number of steps taken per day
mean(steps$daily_steps,na.rm=TRUE)

median(steps$daily_steps,na.rm=TRUE)

#What is the average daily activity pattern?

#1-Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

activity<-activity_with_na
activity[is.na(activity_with_na)]<-0

steps_interval<-activity%>%
  group_by(interval)%>%
  summarize(steps_interval=mean(steps))

ggplot(steps_interval,aes(x=interval,y=steps_interval))+geom_line()+
  labs(title="5-minute interval (all days averaged) number of steps taken",x="5-minute intervals",y="average steps")

dev.copy(png,file="plot2.png",height=480,width=480)

dev.off()

#2-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

steps_interval_max<-activity%>%
  group_by(interval)%>%
  summarize(max_step=max(steps))


steps_interval_max[which.max(steps_interval_max$max_step),]

#Imputing missing values

#1-Calculate and report the total number of missing values in the dataset

sum_of_na<-activity_with_na%>%
  filter(is.na(steps))%>%count(steps)

sum_of_na



#2-Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.

Sys.setlocale("LC_TIME","English")

day_steps_with_na<-activity_with_na%>%mutate(day_of_week=weekdays(as.Date(date)))%>%
  mutate(type_of_day=ifelse(day_of_week=="Saturday"|day_of_week=="Sunday","weekend","weekday"))

day_steps_filled<-day_steps_with_na%>%
  group_by(day_of_week,interval)%>%
  mutate(steps=replace(steps,is.na(steps),mean(steps,na.rm=TRUE)))

#3-Create a new dataset that is equal to the original dataset but with the missing data filled in.

day_steps<-day_steps_filled%>%
  group_by(date)%>%
  summarize(daily_steps=sum(steps))

#4-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
#total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

ggplot(day_steps,aes(daily_steps,fill=..count..))+geom_histogram(binwidth=2000,col="dark orange")+
  scale_y_continuous(breaks=seq(0,20,by=2))+
  labs(x="steps taken each day",title="Histogram of the total number of steps taken each day")+
  scale_fill_gradient("count",low="blue",high="red")
dev.copy(png,file="plot3.png",height=480,width=480)

dev.off()

mean(day_steps$daily_steps)

median(day_steps$daily_steps)

#Are there differences in activity patterns between weekdays and weekends?

#1-Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating 
#whether a given date is a weekday or weekend day.

#This factor was created in question 2 in order to fill missing values by the help of the pattern in day of the week 
#and 5-minute intervals.

#2-Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
#averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

day_steps_interval<-day_steps_filled%>%
  group_by(interval,type_of_day)%>%
  summarize(steps_interval=mean(steps))

ggplot(day_steps_interval,aes(x=interval,y=steps_interval))+geom_line(aes(color=type_of_day))+
  facet_grid(.~type_of_day)+
  labs(title="Comparison of average steps taken on weekend and weekday",y="average steps taken")
  
dev.copy(png,file="plot4.png",height=480,width=480)

dev.off()



