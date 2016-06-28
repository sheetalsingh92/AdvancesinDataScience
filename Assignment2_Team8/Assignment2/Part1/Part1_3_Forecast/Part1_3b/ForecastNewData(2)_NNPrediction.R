## Artificial Neural Network on multiple models

set.seed(500)
library(MASS)
#data <- Boston

#Hourly_filled_dataset
data<-read.csv(file.choose(),header=TRUE)

#just to check the missing data
apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
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


#----ForecastNewData2.csv----
firstnewforecast<-read.csv(file.choose(),header=TRUE)
View(firstnewforecast)

newforecast <- firstnewforecast[,-c(1,4)]
View(newforecast)
str(newforecast)
newforecast$WeekDay <- as.numeric(newforecast$WeekDay)
newforecast$Peakhour <- as.numeric(newforecast$Peakhour)
newforecast$Day<-as.numeric(newforecast$Day)
newforecast$Hour<-as.numeric(newforecast$Hour)
newforecast$month<-as.numeric(newforecast$month)
newforecast$Day.of.Week<-as.numeric(newforecast$Day.of.Week)
newforecast$Temperature<-as.numeric(newforecast$Temperature)

predictforecast.nn <- compute(nn,newforecast[,1:7])
View(predictforecast.nn)

pr.nn_forecast <- predictforecast.nn$net.result*(max(data$KWh)-min(data$KWh))+min(data$KWh)
View(pr.nn_forecast)

View(firstnewforecast)
Forecast_KWh<-cbind(firstnewforecast[1],firstnewforecast[5],firstnewforecast[9],KWh= pr.nn_forecast)
View(Forecast_KWh)

write.csv(Forecast_KWh, file="ForecastNewData2_NN_Prediction.csv")

