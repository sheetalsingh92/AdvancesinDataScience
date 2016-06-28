#Assignment2, Part1: Algorithm Implementation, 1: Data wrangling and cleansing and Multiple linear regression, a, a

#Assignment2_NonZeroDataset.csv
data1aa<- read.csv(file.choose(),header = T)
head(data1aa)
summary(data1aa)
View(data1aa)

#75% of the sample size
smp_size <- floor(0.75 * nrow(data1aa))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data1aa)), size = smp_size)

#Split the data into training and testing
train <- data1aa[train_ind, ]
test <- data1aa[-train_ind, ]

#fit<-lm(KWh~., data=data1aa)
fit<-with(train,lm(KWh ~ Temperature+Peakhour+DayofWeek+WeekDay+hour+month+day))
summary(fit)

files<-summary(fit)
a<-files$coefficients[,1]
coef<-coefficients(files)
View(coef)
write.csv(coef, file="Coefficient_Dataset_1aa.csv")

#data2<-read.csv(file.choose(),header = T)
install.packages("forecast")
library(forecast)
KWh<-predict.lm(fit, test)
newKWh<-data.frame(KWh)
View(newKWh)
write.csv(newKWh, file="Predicted_Dataset_1aa.csv")

#see here, check once again
abc<-accuracy(KWh, train$KWh)
View(abc)
write.csv(abc, file="Regression_Dataset_1aa.csv")
