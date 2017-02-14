# Neural Network model
library(neuralnet)
setwd("D:\\Northeastern University\\ADS\\Assignment\\Assignment2\\Assignment 2 solution\\Part 2 NN and Tree\\")


data.neural<-read.csv("Hourly_Filled_Data.csv", header = T)
#data.neural<-subset(data.neural, data.neural[,"kWh"]!=0) 
View(data.neural)

#use dataset infert
?infert
?neuralnet
library(neuralnet)

#### splitting data######
smp_size <- floor(0.75 * nrow(data.neural))


#Set the seed to make your partition reproductible
set.seed(2000)
train_neural <- sample(seq_len(nrow(data.neural)), size = smp_size)

#Split the data into training and testing
train.neural <- data.neural[train_neural, ]
test.neural <- data.neural[-train_neural, ]

nn <- neuralnet(kWh ~ month+day+hour+PeakHour+temperature+Weekday,
                data=train.neural, act.fct = "logistic", learningrate = 0.01,
                hidden = 2,err.fct = "sse",linear.output = TRUE)
nn
plot(nn)
nn$net.result
nn$weights
nn$result.matrix
nn$covariate
train.neural$kWh
nn$net.result[[1]]
#mean(100*abs(nn$net.result[[1]]-data.neural$kWh)/data.neural$kWh)

nn1 <- ifelse(nn$net.result[[1]]>0.5,1,0)
nn1
misClassificationError = mean(train.neural$kWh != nn1)
misClassificationError
outputVsPred = cbind(data.neural$kWh,nn1)
outputVsPred


nn.bp <- neuralnet(formula = kWh ~ month+day+hour+PeakHour+temperature+Weekday,
                   data=train.neural,hidden = c(4,4),learningrate=0.01, algorithm = "backprop",
                   err.fct = "sse",linear.output = FALSE)
nn.bp

#train.neural <-matrix(dat=train.neural,ncol = 6,byrow = TRUE)

##Prediction

new.output <- compute(nn.bp, covariate = matrix(c(11, 25, 19, 1, 48.90000000, 1,
                                                  11, 24, 18, 1, 50.00000000, 1,
                                                  11, 23, 17, 1, 48.90000000, 1,
                                                  11, 22, 16, 1, 50.00000000, 1,
                                                  11, 21, 15, 1, 48.90000000, 1),
                                                byrow = TRUE, ncol = 6))
#mean(100*abs(array(new.output$net.result)-test.neural)/test.neural)

new.output$net.result


#confidence intervals
ci = confidence.interval(nn, alpha = 0.05)
ci
summary(data.neural)

##Visualize the results

par(mfrow=c(2,2))
gwplot(nn, selected.covariate = "month",min=1,max =11)
gwplot(nn, selected.covariate = "day",min=1,max = 31)
gwplot(nn, selected.covariate = "hour",min=0,max = 23)
gwplot(nn, selected.covariate = "PeakHour",min=0,max =1 )
gwplot(nn, selected.covariate = "temperature",min=1.5,max = 91.9)
gwplot(nn, selected.covariate = "Weekday",min=0,max = 1)