# Reading from Hourly_Filled_Data.csv 
data1<- read.csv(file.choose(),header = T)
head(data1)
summary(data1)

install.packages("tree")
library(tree)
library(MASS)
library(ISLR)

fit <- tree(KWh~Temperature+hour, data1)
summary(fit)


# For newforcastdata2 --- { Regression Treeeee...............}
data2<-read.csv(file.choose(),header = T)
colnames(data2)<-c("Date","month","day","year","hour","DayofWeek","WeekDay","Peakhour","Temperature")
#View(data2)
KWh<-predict(fit, data2)
View(KWh)
final <- cbind(data2,KWh)
View(final)
write.csv(final,file="part1_3_forecastRegnewdata2_b.csv")

# For newforcastdata --- { Regression Treeeee...............}
data3<-read.csv(file.choose(),header = T)
colnames(data3)<-c("Date","month","day","year","hour","DayofWeek","WeekDay","Peakhour","Temperature")
#View(data3)

KWh<-predict(fit, data3)
View(KWh)
final1 <- cbind(data3,KWh)
View(final1)
write.csv(final1,file="part1_3_forecastRegnewdata_b.csv")