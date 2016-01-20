rm(list = ls())

con2 <- file("mining/project/train.csv", "r")
train <- read.csv(con2)

con <- file("mining/project/test.csv", "r")
test <- read.csv(con)

close(con)
close(con2)

library(lubridate)

library(neuralnet)



con2 <- file("mining/project/train.csv", "r")
train <- read.csv(con2)

con <- file("mining/project/test.csv", "r")
test <- read.csv(con)


#make hour a separate variable

train$hour <- hour(train$datetime)

test$hour <- hour(test$datetime)

#make day a separate variable

train$day <- wday(train$datetime)

test$day <- wday(test$datetime)

#make year a separate variable

train$year <- year(train$datetime)

test$year <- year(test$datetime)

#write code to look for weather 4

train$weather[train$weather == 4]

test$weather[test$weather == 4]

#get rid of weather 4

train$weather[train$weather==4] <- 3

test$weather[test$weather==4] <- 3

train$season <- as.factor(train$season)

test$season <- as.factor(test$season)

train$workingday <- as.factor(train$workingday)

test$workingday <- as.factor(test$workingday)

train$weather <- as.factor(train$weather)

test$weather <- as.factor(test$weather)

train$year <- as.factor(train$year)

test$year <- as.factor(test$year)

train$day <- as.factor(train$day)

test$day <- as.factor(test$day)

train$hour <- as.factor(train$hour)

test$hour <- as.factor(test$hour)

trainmat <- model.matrix(count~season+workingday+weather+year+hour+day,data=train)

testmat <- model.matrix(~season+workingday+weather+year+hour+day,data=test)

trainmat <- as.data.frame(trainmat)

testmat <- as.data.frame(testmat)

####################################

#scale count

count <- train$count/1000

#add count to trainmat

trainmat <- cbind(trainmat,count)

#Write formula
formula <- count ~ season2+season3+season4+workingday1+weather2+weather3+year2012+hour1+hour2+hour3+hour4+hour5+hour6+hour7+hour8+hour9+hour10+hour11+hour12+hour13+hour14+hour15+hour16+hour17+hour18+hour19+hour20+hour21+hour22+hour23+day2+day3+day4+day5+day6+day7

#train your data.  note that this is a neural network with 5 hidden layers 
formula <- count ~ season2+season3+season4+workingday1+weather2+weather3+year2012+hour1+hour2+hour3+hour4+hour5+hour6+hour7+hour8+hour9+hour10+hour11+hour12+hour13+hour14+hour15+hour16+hour17+hour18+hour19+hour20+hour21+hour22+hour23+day2+day3+day4+day5+day6+day7

#train your data.  note that this is a neural network with 5 hidden layers 
fit<-neuralnet(formula, data=trainmat, hidden = c(7,8,9,8,7), threshold = 0.04,
          stepmax = 1e+06,
          learningrate.limit = NULL,
          learningrate=0.001,
          learningrate.factor = list(minus = 0.5, plus =1.2), lifesign = "full",
          lifesign.step = 1000, algorithm = "rprop+", 
           likelihood = TRUE)


str(testmat)
testmat <- testmat[,2:37]

#Get predictions

predict <- compute(fit,testmat)

#Assign predictions to variable because compute produces more than we need

predict<- predict$net.result

#Rescale

predict<- predict*1000

#Since this model is a bit overfitted, we'll check to make sure we don't have any values that are really low.  Since we will, we'll set the minimum prediction to an arbitrary number, 3.8

#Check for any negative variables

predict[predict<3]

# We'll set the minimum prediction here to 3.8

predict[predict<3] <- 3.8

submit <- data.frame(datetime = test$datetime, count=predict)


write.csv(submit, file = "ann.csv", row.names=F)

write.csv(test, file = "testb.csv")
predict
newcount <- train$count[1:6493,]
library(ROCR)
pred <- prediction(predict, count[1:6493,])

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

