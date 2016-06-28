#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, a, b

#Assignment2_NonZeroDataset.csv
data1<- read.csv(file.choose(),header = T)
head(data1)
summary(data1)

fit<-with(data1,lm(KWh~DayofWeek+WeekDay+hour+month+day))
summary(fit)

#Assignment2_WithZeroesDataset.csv
data2<-read.csv(file.choose(),header = T)
prediction<-predict.lm(fit, data2)
View(data2)

head(prediction)
summary(prediction)
dataframe<-data.frame(prediction)
View(dataframe)

write.csv(dataframe, file="Predicted_KWh.csv", row.names=FALSE)


##Predicted_KWh.csv
data3<-read.csv(file.choose(),header = T)
predicted_KWh<-data.frame(data3$prediction)
View(predicted_KWh)
View(data3)

##dataset with only KWh values from zeroes dataset
KWhValues<-data.frame(KWh=data2$KWh)
#View(KWhValues)

zero_locations <- which(KWhValues == 0, arr.ind=TRUE)
KWhValues[zero_locations] <- predicted_KWh[zero_locations]
View(KWhValues)

#new.Freq <- with(KWhValues$KWh, ifelse(Var1 <= 0, -Freq, Freq))

final_replaced<- cbind(data2[1:2],kWh=KWhValues,data2[4:11])

View(final_replaced)

write.csv(final_replaced, file="Hourly_filled_data.csv", row.names=FALSE)
