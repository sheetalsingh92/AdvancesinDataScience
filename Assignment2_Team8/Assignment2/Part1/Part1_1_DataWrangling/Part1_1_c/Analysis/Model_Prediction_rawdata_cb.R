#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, c, b
#Using file made by zoo file (Applying to raw/zeroes dataset)

#ZeroesReplacedByZoo_1_ca_cb.csv
data1cb<- read.csv(file.choose(),header = T)
head(data1cb)
summary(data1cb)

fit<-with(data1bb,lm(KWh~Peakhour+Temperature+DayofWeek+WeekDay+hour+month+day))
summary(fit)

files<-summary(fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)

#Assignment2_WithZeroesDataset.csv
data2cb<-read.csv(file.choose(),header = T)
prediction<-predict.lm(fit, data2cb)
View(data2cb)

#Regression Coefficients - MAPE, RMSE etc.
abc<-accuracy(prediction, data1cb$KWh)
View(abc)
write.csv(abc, "Error_Output_rawdata_cb.csv",row.names=FALSE)


