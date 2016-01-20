
library(randomForest)

formula <- count~season+workingday+weather+humidity+year+hour+day

xtest1<- test[,c(2,4,5,8,10,11,12)]

fit<-randomForest(formula, data=subset)
predict <- predict(fit,xtest)

ftable(predict(svm01), testexp)

predict[predict<3] <- 3.8

submit <- data.frame(datetime = test$datetime, count=predict)
write.csv(submit, file = "random.csv", row.names=F)