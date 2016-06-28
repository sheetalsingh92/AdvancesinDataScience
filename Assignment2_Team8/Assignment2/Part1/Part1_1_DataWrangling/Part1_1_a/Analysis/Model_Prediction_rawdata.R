#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, a, b

#Assignment2_NonZeroDataset.csv
data1<- read.csv(file.choose(),header = T)
head(data1)
summary(data1)

fit<-with(data1,lm(KWh~Peakhour+Temperature+DayofWeek+WeekDay+hour+month+day))
summary(fit)

files<-summary(fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)
write.csv(coef, file="Coefficient_Dataset_1ab.csv")

#Assignment2_WithZeroesDataset.csv
data2<-read.csv(file.choose(),header = T)
prediction<-predict.lm(fit, data2)
View(data2)

head(prediction)
summary(prediction)
dataframe<-data.frame(prediction)
View(dataframe)
write.csv(dataframe, file="Predicted_Dataset_1ab.csv")

abc<-accuracy(prediction, data1$KWh)
View(abc)
write.csv(abc, "Regression_Dataset_1ab.csv")

#final<- cbind(data2[1:2],kWh=prediction,data2[4:11])
#final_zeroreplaced_dframe <- data.frame(final)
#View(final_zeroreplaced_dframe)
#write.csv(final_zeroreplaced_dframe,"zeroesreplacedbynewdata.csv")

