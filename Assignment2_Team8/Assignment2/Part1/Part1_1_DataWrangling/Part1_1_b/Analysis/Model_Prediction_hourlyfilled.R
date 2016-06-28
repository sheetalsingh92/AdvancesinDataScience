#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, b, a
#Model Building for part a and b using the file created by model made without temperature

#Hourly_filled_data.csv
data1ba<- read.csv(file.choose(),header = T)
head(data1ba)
summary(data1ba)
View(data1ba)

fit<-with(data1ba,lm(KWh~Peakhour+Temperature+DayofWeek+WeekDay+hour+month+day))
summary(fit)

files<-summary(fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)

#Hourly_filled_data.csv
data2ba<-read.csv(file.choose(),header = T)
prediction<-predict.lm(fit, data2ba)
View(data2ba)

#Regression Coefficients - MAPE, RMSE etc.
abc<-accuracy(prediction, data1ba$KWh)
View(abc)
write.csv(abc, "Error_output_HourlyFilledData.csv", row.names=FALSE)


