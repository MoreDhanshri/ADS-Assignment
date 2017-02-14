library(neuralnet)
setwd("D:\\NEU\\ADS\\assignments\\Assignment2\\RScripts\\forecast")


data1<-read.csv("D:\\NEU\\ADS\\assignments\\Assignment2\\RScripts\\FinalDataAssignment2.csv", header = T)

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

test<-read.csv("forecastInput_2.csv", header = T)
testFormat<-test[,c("month","day","hour","PeakHour","temperature","Weekday")]


#Test the neural network on training data

net.results <- compute(net.sqrt, testFormat) 

#see the properties of net.sqrt
ls(net.results)

# results
result<-net.results$net.result

test$kWh<-result

write.csv(test,file="forecastNeuralNet_Output2.csv")

