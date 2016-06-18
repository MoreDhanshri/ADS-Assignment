setwd("C:\\Software\\r files")


raw.data<-read.csv("C:\\Software\\r files\\NewData.csv", header = T)
raw.data<-raw.data[raw.data$Units=="kWh",]
raw.data$Date<-strptime(raw.data$Date, '%m/%d/%Y')
raw.data$year<-as.numeric(format(raw.data$Date, '%Y'))
raw.data$month<-as.numeric(format(raw.data$Date, '%m'))
raw.data$day<-as.numeric(format(raw.data$Date, '%d'))
w<-as.POSIXlt(raw.data$Date)$wday-1
w[w==-1]<-6
raw.data$DayOfWeek<-w
raw.data$Weekday<-ifelse(w<5, 1,0)
#power<-data.frame(raw.data[,5:292])
raw.data.final<- data.frame(raw.data$Account, raw.data$Date,raw.data$month,raw.data$day, raw.data$year,raw.data$DayOfWeek,raw.data$Weekday)


# splitting hours and binning it in 24 hours
raw<-data.frame(raw.data[,5:292])
g = split(seq(ncol(raw)), (seq(ncol(raw)) - 1) %/% 12 )
t2<-sapply(g, function(cols) rowSums( raw[, cols] ))

##Converting to single column

singleColt <- matrix(t(t2),ncol = 1)
raw1repeat <- raw.data.final[rep(seq_len(nrow(raw.data.final)),each=24),]
#View(raw1repeat1)
hour<-c(0:23)
raw1repeat1<- cbind(raw1repeat,hour)
raw1final<-cbind(raw1repeat1,singleColt)
raw1final$PeakHour<-ifelse((raw1final$hour>=7) & (raw1final$hour<20), 1,0)
raw1final<-cbind(raw1repeat1,singleColt)
raw1final$PeakHour<-ifelse((raw1final$hour>=7) & (raw1final$hour<20), 1,0)
#View(raw1final) ##final data of part1

temperature.data<-read.csv("C:\\Software\\r files\\finalTemp3.csv", header = T)  ####change this to add aggregation and fetching the


date11<- strptime(temperature.data$hourofday,"%m/%d/%Y")

t.str<-strptime(temperature.data$hourofday,"%m/%d/%Y %H:%M")
time1<-as.POSIXlt(t.str,format(t.str,"%m%d%Y %H:%M:%S%z"))
temperature.data$Hour<-time1$hour
date11<- strptime(temperature.data$hourofday,"%m/%d/%Y")
temperature.data$Date<-date11


mergedraw1<-merge(raw1final,temperature.data,by.x = c("raw.data.Date","hour"),by.y=c("Date","Hour"),all.x = TRUE)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))  ##replace the N/A records with mean of the day


#install.packages("plyr")
library(plyr)
#mergedraw1 <- ddply(mergedraw1, ~ hour, transform, temperature = impute.mean(temperature))
mergedraw21 <- ddply(mergedraw1, ~ hour, transform, temperature = impute.mean(temperature))
mergedraw1$X<-  NULL
colnames(mergedraw1)[8] <- "kWh"

#mergerexample<-ddply(mergedraw1, "hour", transform, temperature = impute.mean(temperature))
#View(mergerexample)



#install.packages("data.table")
library(data.table)
setnames(mergedraw1, old=c("raw.data.Date","raw.data.Account","raw.data.month","raw.data.day","raw.data.year","raw.data.DayOfWeek","singleColt"), new=c("Date", "Account","month","day","year","Day Of Week","Kkwh"))
mergedraw1<- mergedraw1[c( "Account","Date","kWh","month","day","year","hour","Day Of Week","PeakHour","temperature")]
mergedraw1$kWh<-NULL
mergedraw1$`Day Of Week`<-NULL
mergedraw1$DayOfWeek<-w
mergedraw1$Weekday<-ifelse(w<5, 1,0)
mergedraw12<-cbind(mergedraw1,singleColt)
names(mergedraw12)[names(mergedraw12) == 'singleColt'] <- 'kWh'



##########################################################################################
powerData<-mergedraw12
boxplot(powerData,xlab=names(powerData))

boxplot(powerData$temperature)


#uif_RunTime<-quantile(fitness$RunTime,.75)+1.5*IQR(fitness$RunTime)
#lif_Weight<-quantile(fitness$Weight, .25)-1.5*IQR(fitness$Weight)


#tna<- powerData$temperature[powerData$temperature]

powerData[!complete.cases(powerData),]

mean(powerData$temperature, na.rm=TRUE)
median(powerData$temperature, na.rm=TRUE)

powerData$temperature[is.na(powerData$temperature)] <- median(powerData$temperature, na.rm=TRUE)

lq_temperature<-quantile(powerData$temperature, .25)-1.5*IQR(powerData$temperature)
powerData$temperature[powerData$temperature<lq_temperature]<-mean(powerData$temperature)
powerData$temperature[powerData$temperature<=0]<-mean(powerData$temperature)

boxplot(powerData$temperature)

##### analysis for power
mean(powerData$kWh, na.rm=TRUE)
median(powerData$kWh, na.rm=TRUE)
powerData$kWh[is.na(powerData$kWh)] <- median(powerData$kWh, na.rm=TRUE)

boxplot(powerData$kWh)

lq_kWh<-quantile(powerData$kWh, .25)-1.5*IQR(powerData$kWh)
powerData$kWh[powerData$kWh<lq_kWh]<-mean(powerData$kWh<lq_kWh)


powerData$kWh[is.na(powerData$kWh)]<-mean(powerData$kWh, na.rm = TRUE)

lq_kwh<-quantile(powerData$kWh, .25)-1.5*IQR(powerData$kWh)
uq_kwh<-quantile(powerData$kWh,.75)+1.5*IQR(powerData$kWh)
mean(powerData$kWh )
median(powerData$kWh)
powerData$kWh[powerData$kWh<=0]<-mean(powerData$kWh )
powerData<-data.frame(powerData)



write.csv(powerData,file = "sampleformat.csv")
######################## Linear Regression ######################


powerData$Account<-NULL
powerData$year<-NULL
#### splitting data######
smp_size <- floor(0.75 * nrow(powerData))


#Set the seed to make your partition reproductible
set.seed(300)
train_ind <- sample(seq_len(nrow(powerData)), size = smp_size)

#Split the data into training and testing
train <- powerData[train_ind, ]
test <- powerData[-train_ind, ]

#install.packages("leaps")
library(leaps)

##########Exhaustive Feature selection ###############

##### Searching all subset models up to size 8 by default
regfit.full=regsubsets(train$kWh~.,data=train)
reg8.summary =summary (regfit.full)
names(reg8.summary)
reg8.summary$rss
reg8.summary$adjr2

##### Searching all subset models up to size number of variables
regfit.full=regsubsets(train$kWh~.,data=train ,nvmax=11)
#View(regfit.full)
reg.summary =summary (regfit.full)
names(reg.summary)
reg.summary$rss
reg.summary$adjr2
#reg.summary$outmat
## Plotting and choosing the subset
par(mfrow=c(2,2)) 
plot(reg.summary$rss ,xlab="Number of Variables 11",ylab="RSS", type="l") 
plot(reg.summary$adjr2 ,xlab="Number of Variables 11", ylab="Adjusted RSq",type="l")
plot(reg8.summary$rss ,xlab="Number of Variables 8",ylab="RSS", type="l") 
plot(reg8.summary$adjr2 ,xlab="Number of Variables 8 ", ylab="Adjusted RSq",type="l")


regr_op<-coef(regfit.full ,6)
write.csv(regr_op, file="RegressionOutput.csv")
coef(regfit.full ,7)

#### Forward selection
regfit.fwd=regsubsets(train$kWh~.,data=train ,nvmax=8, method="forward") 
F=summary(regfit.fwd)
names(F)
F
F$rss
F$adjr2

par(mfrow=c(1,2)) 
plot(F$rss ,xlab="Number of Variables for forward selection",ylab="RSS", type="l") 
plot(F$adjr2 ,xlab="Number of for forward selection", ylab="Adjusted RSq",type="l")

coef(regfit.fwd,5)

#### Backward selection
regfit.bwd=regsubsets(train$kWh~.,data=train ,nvmax=8, method="backward") 
B=summary(regfit.bwd)
names(B)
B
B$rss
B$adjr2
coef(regfit.bwd,5)

####### final model
lm.fit5<-lm(kWh ~ Date+month+day+hour+PeakHour, data = train)
summary(lm.fit5)
lm.fit6<-lm(kWh ~ Date+month+day+hour+PeakHour+temperature, data = train)
summary(lm.fit6)
lm.fit6

lm.fit7<-lm(kWh ~ Date+month+day+hour+PeakHour+temperature+Weekday, data = train)
summary(lm.fit7)


lm.fitall = lm(kWh ~ ., data = train)
summary(lm.fitall)

#Measures of predictive accuracy
#install.packages("zoo")

library(forecast)
pred5 = predict(lm.fit5, test)
accuracy(pred5, train$kWh)

pred6 = predict(lm.fit6, test)
acc<-t(accuracy(pred6, train$kWh))


write.csv(acc, file="PerformanceMatrix.csv")

