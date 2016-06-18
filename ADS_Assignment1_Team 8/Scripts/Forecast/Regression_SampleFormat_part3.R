#First we include 2 libraries - MASS and ISLR

library(MASS)
install.packages("ISLR")
library(ISLR)

#Then we read the csv file we cleansed in part 1. 
data<- read.csv(file.choose(),header = T)
df <- data.frame(data)
attach(df)
names(df)
View(df)

#75% of the sample size
smp_size <- floor(0.75 * nrow(df))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

#Split the data into training and testing
train <- df[train_ind, ]
test <- df[-train_ind, ]

#Fit a linear regression model 
lm.fit = lm(KWh ~ hour+Temperature+month+day+Peakhour+DayofWeek+WeekDay, data = train)

#lapply(df[c("value", "ordereddata_peakhours", "Temperature")], unique)
#To see, whether the variable is a factor or not, use the following code:
#l<-sapply(df,function(df)is.factor(df))
#m<-df[,names(which(l=="TRUE"))]
#ifelse(n<-sapply(m,function(df)length(levels(df)))==1,"DROP","NODROP")
#value <- factor(value)

#Summary of the fit
summary(lm.fit)
files<-summary(lm.fit)

a<-files$coefficients[,1]

coef<-coefficients(files)
table1<-data.frame(coef)
View(table1)

#acc<-rbind(a, Account=26435791004)
#colnames(a)<-c("account")
write.csv(a, file="Coefficients_one.csv")

sink("Coefficients_one.csv")
summary(files)
sink()
summary(a)
#Measures of predictive accuracy
install.packages("forecast")
library(forecast)
pred = predict(lm.fit, test)
abc<-accuracy(pred, train$KWh)
View(abc)

write.csv(abc, file="PerformanceMetrics(SampleFormat).csv")

forecast_input <- read.csv(file.choose(),header = T)
View(forecast_input)
table2<-data.frame(forecast_input)
View(table2)

hour1<-table1[2,1]
temp1<-table1[3,1]
month1<-table1[4,1]
weekday1<-table1[7,1] 

c1<-table1[1,1]

hour2<-table2$Hour
temp2<-table2$Temperature
month2<-table2$month
weekday2<-table2$WeekDay

KWh<-(hour1*hour2+temp1*temp2+month1*month2+weekday1*weekday2+c1)

dh <-cbind(table2[1:10],KWh)
View(dh)

write.csv(dh,file= "part3b.csv")



