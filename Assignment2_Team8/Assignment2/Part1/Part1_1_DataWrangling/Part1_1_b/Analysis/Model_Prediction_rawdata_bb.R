#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, b, b
#Model Building for part a and b using the file created by model made without temperature

#Hourly_filled_data.csv
data1bb<- read.csv(file.choose(),header = T)
head(data1bb)
summary(data1bb)

fit<-with(data1bb,lm(KWh~Peakhour+Temperature+DayofWeek+WeekDay+hour+month+day))
summary(fit)

files<-summary(fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)

#Part1_Q1_rawdata.csv
data2bb<-read.csv(file.choose(),header = T)
prediction<-predict.lm(fit, data2bb)
View(data2bb)

#Regression Coefficients - MAPE, RMSE etc.
abc<-accuracy(prediction, data1bb$KWh)
View(abc)
write.csv(abc, "Error_output_rawdata_bb.csv", row.names=FALSE)


