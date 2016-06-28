#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, c, a
#Using file made by zoo file (Applying to filled dataset)

#ZeroesReplacedByZoo_2_1_ca_cb.csv
data1ca<- read.csv(file.choose(),header = T)
head(data1ca)
summary(data1ca)

fit<-with(data1ca,lm(KWh~Peakhour+Temperature+DayofWeek+WeekDay+hour+month+day))
summary(fit)

files<-summary(fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)

#ZeroesReplacedByZoo_2_1_ca_cb.csv
data2ca<-read.csv(file.choose(),header = T)
prediction<-predict.lm(fit, data2ca)
View(data2ca)

#Regression Coefficients - MAPE, RMSE etc.
library("forecast")
abc<-accuracy(prediction, data1ca$KWh)
View(abc)
write.csv(abc, "Error_Output_nareplaced_ca.csv",row.names=FALSE)

