library(tree)
setwd("D:\\NEU\\ADS\\assignments\\Assignment2\\RScripts\\forecast")


data1<-read.csv("D:\\NEU\\ADS\\assignments\\Assignment2\\RScripts\\FinalDataAssignment2.csv", header = T)
data1$X<-NULL
data1$Account<-NULL
data1$year<-NULL
data1$Date<-as.Date(data1$Date)
str(data1)

head(data1[!complete.cases(data1),])


set.seed (100)
train = sample (1:nrow(data1), nrow(data1)/2)
tree2 = tree(kWh~.,data1,subset=train)
tree1 = tree(kWh~month+day+hour+PeakHour+temperature,data1,subset=train)
summary (tree1)
par(mfrow=c(1,1)) 
plot (tree1)
text (tree1, pretty = 0)

cv.data1 = cv.tree (tree1)
plot (cv.data1$size, cv.data1$dev, type='b')


prune.tree1 =prune.tree(tree1, best = 6)
plot(prune.tree1)
text(prune.tree1, pretty = 0)

test<-read.csv("forecastInput_2.csv", header = T)

yhat<-predict (tree1, newdata =test)
test$kWh<-yhat



write.csv(test, file="forecastRegressionTree_Output2.csv")
