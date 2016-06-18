library(MASS)
install.packages("ISLR")
library(ISLR)

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
summary(lm.fit)

#lapply(df[c("value", "ordereddata_peakhours", "Temperature")], unique)
#To see, whether the variable is a factor or not, use the following code:
#l<-sapply(df,function(df)is.factor(df))
#m<-df[,names(which(l=="TRUE"))]
#ifelse(n<-sapply(m,function(df)length(levels(df)))==1,"DROP","NODROP")

value <- factor(value)

#Summary of the fit
files<-summary(lm.fit)

a<-files$coefficients[,1]

coef<-coefficients(files)
View(coef)

#acc<-rbind(a, Account=26435791004)

#colnames(a)<-c("account")
write.csv(a, file="Coefficients_two.csv")

sink("Coefficients_one.csv")
summary(files)
sink()
#summary(a)
#Measures of predictive accuracy
install.packages("forecast")
library(forecast)
pred = predict(lm.fit, test)
abc<-accuracy(pred, train$KWh)
View(abc)


write.csv(abc, file="Regression_PerfMetrics_newData.csv")
