


library(e1071)






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

trainexp$weather[trainexp$weather == 4]

testexp$weather[testexp$weather == 4]

#get rid of weather 4\
trainexp<-train

trainexp$weather[trainexp$weather==4] <- 3

testexp$weather[testexp$weather==4] <- 3

train$season <- as.numeric(train$season)

test$season <- as.numeric(test$season)
############################newnewnew###################   
train$temp <- as.factor(train$temp)

test$temp <- as.factor(test$temp)


train$humidity <- as.factor(train$humidity)

test$humidity <- as.factor(test$humidity)





###############################################

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

trainmat <- model.matrix(count~season+workingday+weather+temp+humidity+year+hour+day,data=train)

testmat <- model.matrix(~season+workingday+weather+temp+humidity+year+hour+day,data=test)

trainmat <- as.data.frame(trainmat)

testmat <- as.data.frame(testmat)

####################################

#scale count

count <- train$count/1000

#add count to trainmat

trainmat <- cbind(trainmat,count)

#Write formula
testexp<-test
trainexp$season <- as.numeric(trainexp$season)
testexp$season <- as.numeric(testexp$season)
trainexp$workingday<-as.numeric(trainexp$workingday)
testexp$workingday<-as.numeric(testexp$workingday)
trainexp$weather <- as.numeric(trainexp$weather)
testexp$weather <- as.numeric(testexp$weather)
trainexp$temp <- as.numeric(trainexp$temp)
testexp$temp <- as.numeric(testexp$temp)
trainexp$humidity <- as.numeric(trainexp$humidity)
testexp$humidity <- as.numeric(testexp$humidity)
trainexp$year <- as.numeric(trainexp$year)
testexp$year <- as.numeric(testexp$year)
trainexp$hour <- as.numeric(trainexp$hour)
testexp$hour <- as.numeric(testexp$hour)
trainexp$day <- as.numeric(trainexp$day)

testexp$day <- as.numeric(testexp$day)
trainexp$atemp <- as.numeric(trainexp$atemp)
testexp$atemp <- as.numeric(testexp$atemp)
trainexp$registered <- as.numeric(trainexp$registered)
testexp$registered <- as.numeric(testexp$registered)
trainexp$casual <- as.numeric(trainexp$casual)
testexp$casual <- as.numeric(testexp$casual)


formula <- count~ season+workingday+weather+humidity


svm01 <- svm(formula, data=trainexp)
predict <- predict(svm01,temp)

ftable(predict(svm01), testexp)

predict[predict<3] <- 3.8

submit <- data.frame(datetime = test$datetime, count=predict)
write.csv(submit, file = "resultsvm2.csv", row.names=F)




