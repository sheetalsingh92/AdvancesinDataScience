library(MASS)
install.packages("ISLR")
library(ISLR)

#Using the file that was cleansed with zeroes removed- Assignment2, Part1, a 
data<- read.csv(file.choose(),header = T)
df <- data.frame(data)
View(df)
names(df)

#75% of the sample size
smp_size <- floor(0.75 * nrow(df))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

#Split the data into training and testing
train <- df[train_ind, ]
test <- df[-train_ind, ]

#Fit a linear regression model 
#lm.fit = lm(KWh ~ ., data = train)
#lm.fit = lm(value ~ Temperature+Hour , data=train)
lm.fit = lm(KWh ~ Peakhour+WeekDay+month+hour+DayofWeek+Date, data = train)
summary(lm.fit)

#Making the csv file for the coefficients
files<-summary(lm.fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)
write.csv(abc, file="Coefficient_Assign2Part1ba_newData.csv")

#Measures of predictive accuracy
install.packages("forecast")
library(forecast)
pred = predict(lm.fit, test)
newKWh<-data.frame(pred)
View(newKWh)
abc<-accuracy(pred, train$KWh)
View(abc)

#bigmodel <- lm(KWh ~ Peakhour+WeekDay+month+hour+DayofWeek+Date, data = train)
#newdata = data.frame(wt = runif(20, min = 1.5, max = 6))

#cbind(
#  newdata,
#  train = predict(bigmodel, newdata = newdata)
#)

write.csv(abc, file="Regression_Assign2Part1ba_newData.csv")
