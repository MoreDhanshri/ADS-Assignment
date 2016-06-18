#setwd("C:\\Users\\cr203945\\Documents\\NEU\\Assignment 1\\")

forecast.indata<-read.csv("C:\\Software\\r files\\forecastNewData2.csv", header = T)

forecast.indata$Date<-strptime(forecast.indata$Day, '%m/%d/%Y')
forecast.indata$year<-as.numeric(format(forecast.indata$Date, '%Y'))
forecast.indata$month<-as.numeric(format(forecast.indata$Date, '%m'))
forecast.indata$day<-as.numeric(format(forecast.indata$Date, '%d'))
w<-as.POSIXlt(forecast.indata$Date)$wday-1
w[w==-1]<-6
forecast.indata$DayOfWeek<-w
forecast.indata$Weekday<-ifelse(w<5, 1,0)
#View(forecast.indata)
forecast.indata$PeakHour<-ifelse((forecast.indata$Hour>=7) & (forecast.indata$Hour<20), 1,0)

forecast.indata$Date<-NULL

setnames(forecast.indata, old=c("Day","Hour","Temp","year","month","day","DayOfWeek"), new=c("Date", "hour","temperature","Year","month","day","Day Of Week"))
forecast.indata$Date<-as.POSIXct(forecast.indata$Date, format="%m/%d/%Y")
class(forecast.indata$Date)
summary(forecast.indata)

########Forecast based on lm.fit6#########


#forecast.indata$Date<-(unclass(forecast.indata$Date))
#class(forecast.indata$Date)
library(forecast)
forecast6 <- predict(lm.fit6, forecast.indata)
write.csv(forecast6,file = "forecastOutput_999999999.csv")
par(mfrow=c(1,2))
boxplot(forecast6)
boxplot(powerData$kWh)

