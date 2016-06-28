
#Assignment 2, Part 1, Question 2, Prediction

# Regression Trees
install.packages("tree")
library(tree)
library(MASS)
library(ISLR)

#Hourly_filled_data.csv
data<-read.csv(file.choose(),header=TRUE)
Tree_dataframe<-data.frame(data)
View(Tree_dataframe)

#Set the seed to make your partition reproductible
set.seed(1)
train_ind <- sample(1:nrow(Tree_dataframe), nrow(Tree_dataframe)/2)
#Split the data into training and testing
train <- Tree_dataframe[train_ind, ]
test <- Tree_dataframe[-train_ind, ]

fit <- tree(KWh~Temperature+Peakhour+hour+month+DayofWeek+WeekDay, train)
summary(fit)

KWh_tree<-predict(fit, test)
newKWh_tree<-data.frame(KWh_tree)
View(newKWh_tree)

library(forecast)
abc<-accuracy(KWh_tree, train$KWh)
View(abc)

#write.csv()

######## plotting and pruning
plot (fit)
text(fit, pretty = 0)

#cross-validation
cv_tree <- cv.tree(fit)
plot(cv_tree$size, cv_tree$dev, type="b")

prune_tree <- prune.tree(fit, best = 5)
plot(prune_tree)
text(prune_tree, pretty = 0)

#unpruned tree on test data
unpruned_test <- predict(fit, test)
testdata1 <- Tree_dataframe [-train_ind,"KWh"]
plot(unpruned_test, testdata1)
abline(0,1)
mean((unpruned_test -testdata1)^2)

#prune tree on test data
pruned_test = predict (prune_tree, test)
testdata2=Tree_dataframe [-train_ind,"KWh"]
plot(pruned_test,testdata2)
abline (0,1)
mean((pruned_test -testdata2)^2)
testdata<-data.frame(pruned_test)
View(testdata)

#----------------------------------------
## Artificial Neural Network

set.seed(500)
library(MASS)
#data <- Boston

data<-read.csv(file.choose(),header=TRUE)

#just to check the missing data
apply(data,2,function(x) sum(is.na(x)))

#index <- sample(1:nrow(data),round(0.75*nrow(data)))
#train <- data[index,]
#test <- data[-index,]
#lm.fit <- glm(KWh~Temperature+Peakhour, data=train)
#summary(lm.fit)

#pr.lm <- predict(lm.fit,test)
#MSE.lm <- sum((pr.lm - test$KWh)^2)/nrow(test)

#deleting the account, date and year columns
data <- data[,-c(1,2,6)]
View(data)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

head(data)

data$WeekDay <- as.numeric(data$WeekDay)
data$Peakhour <- as.numeric(data$Peakhour)
data$day<-as.numeric(data$day)
data$hour<-as.numeric(data$hour)
data$month<-as.numeric(data$month)
data$DayofWeek<-as.numeric(data$DayofWeek)

scaled <- as.data.frame(scale(data, center = mins, scale =maxs - mins))
str(scaled)

train_ <- scaled[index,]
test_ <- scaled[-index,]

#model for neural network
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("KWh ~", paste(n[!n %in% "KWh"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=5, threshold= 0.6,linear.output=F)
plot(nn)

pr.nn <- compute(nn,test_[,2:8])
View(pr.nn)

pr.nn_ <- pr.nn$net.result*(max(data$KWh)-min(data$KWh))+min(data$KWh)
View(pr.nn_)

#The predicted output KWh values
test.r <- (test_$KWh)*(max(data$KWh)-min(data$KWh))+min(data$KWh)
#test.r_frame<-data.frame(test.r)
#View(test.r_frame)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
View(MSE.nn)

RMSE.nn<-sqrt(MSE.nn)

#error
error<- test.r - pr.nn_

#rmse
rmse <- function(error)
{
  sqrt(mean(error^2))
}

RMSE.nn<-rmse(error)

#mae
mae <- function(error)
{
  mean(abs(error))
}
MAE.nn<-mae(error)

#mape
mape<-function(error)
  mean(abs((error/test.r)))
MAPE.nn<-mape(error)

#Binding the values into one csv
error_rt<-cbind("Regression Tree",abc)
error_nn<- cbind("Neural Network",RMSE.nn,MAE.nn,MAPE.nn)
final_error<-cbind(error_rt,error_nn)
View(t(final_error))
write.csv(t(final_error),"PerformanceMetrics.csv")



