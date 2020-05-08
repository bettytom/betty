# predictive analysis
library(mlbench)
data("PimaIndiansDiabetes2")
head(PimaIndiansDiabetes2)
# removing the null values
pidna<-na.omit(PimaIndiansDiabetes2)   
pidim<-pidna
pidim$diabetes<-as.numeric(pidna$diabetes)-1  # this is to label the categorical variables as numeric
head(pidim)

#spiliting our data
train<-pidim[1:300,]
test<-pidim[301:392,]

# regression- linear
reg<-lm(diabetes~.,data = train)
summary(reg)

pred<-predict(reg, newdata=test) 
# this basically tests to see which women are diabetic or not
pred
# this looks at how many predictions were true
real<-table(test$diabetes,pred>0.5)
real

rate<-1-sum(diag(real)/sum(real))
rate

real_0<- table(test$diabetes,pred>0.7)
real_0

# load that first
library(readr)

# TimeSeries
#data_1<-"http://rci.rutgers.edu/~rwomack/UNRATE.csv"
#data_2<-"http://rci.rutgers.edu/~rwomack/CPIAUCSL.csv"  

#emp<-read.csv(data_1,row.names = 1)
#emp2<-read.csv(data_2,row.names = 1)
# i have to do this ,unfortunately it cant load
data("AirPassengers")
AP<-AirPassengers
AP
class(AP)
start(AP)
end(AP)
frequency(AP)
# next we check the patterns of the data
cycle(AP)
plot(AP)
aggregate(AP)
boxplot(AP~cycle(AP))
#
# change data into timeseries
emp$Value<-ts(emp$Value, start =c (1948 , 1),frequency=12)
emp_2$Value<-ts(emp_2$Value,start = c(1948,1),frequency=12)
# checking on the time period
time(emp)
# selecting specific month data
emp.July<-window(emp,start = c(1980,7),frequency = 12)
# decomposition
decompose(emp,type = "additive")
#plotting it
plot(decompose(emp,type = "additive"))
ts.plot(emp,emp_2) # plotting two timeseries data frames
acf(emp) # this finds the correlation in the time series data
acf(AP)
plot(acf(AP))
acf(ts.intersect(emp,AP)) # ts.intersect binds two timeseries data frames
# that have the same frequency and consistency
ts.union(emp,AP) # acts the same as ts.frequency
#
#Forecasting methods #holtwinters
plot(HoltWinters(emp,alpha = 0.001,beta=1,gamma=0))
plot(HoltWinters(AP))
AP_h<-HoltWinters(AP)
plot(AP_h)
AP_pred<-predict(AP_h, n.ahead=20*12)
ts.plot(AP,AP_pred,lty=1:2)
plot(acf(diff(AP))) #This shows the how the time series vary 
#from a season til the next
#
# finally regression
#autoregression
ap.ar<-ar(AP,method = "mle")
ap.reg<-lm(AP~time(AP))
summary(ap.reg)
confint(ap.reg)
ap.pred<-predict(ap.ar,n.ahead=10*12)
ts.plot(AP,ap.pred$pred,lty=1:2)
acf(resid(ap.reg))
# arima model
ap.arm<-arima(AP,order=c(0,0,3))
summary(ap.arm)
arm.pred<-predict(ap.arm,n.ahead=10*12)  # prediction
ts.plot(AP,arm.pred$pred,lty=1:2)        
# looking at the models thoroughly
split.screen(figs = c(3,1))
plot(AP,AP_pred,lty=1:2)
screen(1)
