# hierachical clustering algorithim
#load data
library(readxl)
us.oil<-read_xlsx('utilities.xlsx')
str(us.oil)
head(us.oil)

#scatterplot
plot(Fuel_cost~Sales, us.oil)
with(us.oil,text(Fuel_cost~Sales,labels=City,pos=4,cex=0.3))

plot(RoR~Sales,us.oil)
with(us.oil,text(RoR~Sales,labels=City,pos=4,cex=0.45))

#normalization
z<-us.oil[,-c(1,1)]
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)

#calculate the Eucleadian distance
distance<-dist(z)
distance
print(distance,digits=3)

#a clustering dendogram
hc<-hclust(distance)
plot(hc,labels = us.oil$City,hang = -1)

# clustering the data average
hc<-hclust(distance,method = 'average')
plot(hc,labels = us.oil$City,hang = -1)
# load ggplot
library(ggplot2)
p1<-ggplot(us.oil,aes(x=Sales,fill=City))+geom_histogram(binwidth = 30)
p1
