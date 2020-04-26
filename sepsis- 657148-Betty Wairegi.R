#load package eventdataR
library(eventdataR)
data("sepsis")  # loading the data
View(sepsis)

sepsis1<-eventlog(sepsis, case_id = "case_id",
                  activity_id = "activity",
                  activity_instance_id = "activity_instance_id",
                  lifecycle_id = "lifecycle",
                  timestamp = "timestamp",
                  resource_id = "resource")   # the event log

summary(sepsis1) # looking at the outlook of the event log

activity_1<-table(sepsis1$activity,sepsis1$resource)
activity_1  # looks like the activity "Admission NC" used more than five resources and is the only one

activities(sepsis1) #loking at how many times the actvities occurred

resources(sepsis1) # the highest frequency of cases for each resource

table(sepsis1$resource,sepsis1$activity) # looking at the activites done by each resource

trace_list(sepsis1) # just checking to see the list of traces in the activities

trace_explorer(sepsis1,coverage = 0.1) # frequency of activities across the traces when coverage of patients is 10%

