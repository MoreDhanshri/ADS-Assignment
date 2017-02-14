library(tree)
setwd("D:\\Northeastern University\\ADS\\Assignment\\Assignment2\\Assignment 2 solution\\Part 2 NN and Tree\\")


data1<-read.csv("Hourly_Filled_Data.csv", header = T)
data1$X<-NULL
data1$Account<-NULL
data1$year<-NULL
data1$Date<-as.Date(data1$Date)
str(data1)

data1[!complete.cases(data1),]

smp_size <- floor(0.75 * nrow(data1))


#Set the seed to make your partition reproductible
set.seed(300)
train_ind <- sample(seq_len(nrow(data1)), size = smp_size)

#Split the data into training and testing
train <- data1[train_ind, ]
test <- data1[-train_ind, ]


# net.sqrt <- neuralnet(kWh ~ month+day+hour+PeakHour+temperature+Weekday, train, hidden=1, 
#                       algorithm = "backprop", learningrate = 0.01, linear.output = TRUE)
net.sqrt <- neuralnet(kWh ~ month+day+hour+PeakHour+temperature+Weekday, train, 
                      hidden=2, stepmax=1e6)


print(net.sqrt)

#Plot the neural network
plot(net.sqrt)


testFormat<-test[,c("month","day","hour","PeakHour","temperature","Weekday")]


#Test the neural network on training data

net.results <- compute(net.sqrt, testFormat) 

#see the properties of net.sqrt
ls(net.results)

# results
result<-net.results$net.result

ME=mean(100*abs(array(result)-test$kWh)/test$kWh)

# #Lets display a better version of the results
# cleanoutput <- cbind(test,sqrt(testdata),
#                      as.data.frame(net.results$net.result))
# colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
# print(cleanoutput)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

### mape
mape <- function(y, yhat)
  mean(abs((y - yhat)/y))

## MPE

MPE = function(y,yhat){
  n = length(y)
  mpe = 100/n * sum((y-yhat)/y)
  return(mpe)
}

# #Function for Mean Absolute Percentage Error
# mape <- function(error)
# mean(abs((error)/y))

# Example data
actual <- test$kWh
predicted <- net.results$net.result

# Calculate error
error <- actual - predicted

# Example of invocation of functions
rmse.v<-rmse(error)
mae.v<-mae(error)
mape.v<-mape(actual, predicted)
mpe.v<-MPE(actual, predicted)

acc<-t(accuracy(yhat, test))


write.csv(acc, file="PerformanceMatrix_tree.csv")

df <- cbind(ME,rmse.v,mae.v,mape.v,mpe.v)
df <- data.frame(df)
setnames(df,old=c("ME","rmse.v","mae.v","mape.v","mpe.v"), new=c("Mean Error", "Root Mean Squared Error","Mean absolute Error","Mean Absolute Percentage Error","MPE"))
t(df)
write.csv(t(df),file="D:\\Northeastern University\\ADS\\Assignment\\Assignment2\\Assignment 2 solution\\Part 1\\PerformanceMatrix_NeuralNetwork.csv")
