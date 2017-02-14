library(tree)
setwd("D:\\Northeastern University\\ADS\\Assignment\\Assignment2\\Assignment 2 solution\\Part 2 NN and Tree\\")


data1<-read.csv("Hourly_Filled_Data.csv", header = T)
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

yhat=predict (tree1, newdata =data1 [-train,])
test=data1 [-train,"kWh"]
plot(yhat,test)
abline (0,1)
mean((yhat -test)^2)

yhat=predict (prune.tree1, newdata =data1 [-train,])
test=data1 [-train,"kWh"]
plot(yhat,test)
abline (0,1)
mean((yhat -test)^2)

accuracy(yhat, test)
acc<-t(accuracy(yhat, test))


write.csv(acc, file="PerformanceMatrix_tree.csv")
