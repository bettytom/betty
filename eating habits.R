#process analytics
#loading data
library(readxl)
habits<-read.csv("eating_habits.csv")
View(habits)

#loading another library to view the events in the data
library(bupaR) # looking at the event log scope 
habits1<-eventlog(habits,case_id = "case_id",
                  activity_id = "variant_index",
                  activity_instance_id = "activity_instance_id",
                  lifecycle_id = "lifecycle_id",
                  timestamp = "timestamp",
                  resource_id = "resource")

summary(habits1)

table(habits1$activity,habits1$resource)  # how many people took part in each activity and which was the highest frequency and the lowest

library(processmapR) #loading a processmap to look for median time intervals for how much time was spent

process_map(habits1,performance(FUN=median,units = "hours"))

# plotting the activities and time spent
plot(habits$activity,habits$timestamp,main="Time spent on each activity",cex.main=2,xlab="Activity_id",ylab="Time",col="blue",font.main=4,
     font.lab=3)
           