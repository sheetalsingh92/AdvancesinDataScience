#Importing all the packages used
install.packages("lubridate")
install.packages("zoo")
install.packages("tree")
install.packages("forecast")

#File1: windforecast_wf1.csv, windforecast_wf2.csv, windforecast_wf3.csv, windforecast_wf4.csv, windforecast_wf5.csv, windforecast_wf6.csv, windforecast_wf7.csv
windforecast_wf_file<-read.table(file.choose(), header=T, fileEncoding="UTF-8", sep=",")
#Converting date into specific format
library(lubridate)

#Function to format "date" column to year, month, date and hour 
formatDate_wf<-ymd_h(as.character(windforecast_wf_file$date))
#Adding hour to date so that date can be changed with hours, multiplying by 3600 to convert hour integer to seconds and adding in the time format
addHour_wf<-formatDate_wf + windforecast_wf_file$hors*3600

#Filling the NA values with the na.locf method of filling missing values
library(zoo)

#Filling for column "u"
mergesdata_u<- zoo(windforecast_wf_file$u)
X_u<-na.locf(mergesdata_u, na.rm=TRUE)
X_u<-as.numeric(X_u)
#Filling for column "v"
mergesdata_v<- zoo(windforecast_wf_file$v)
X_v<-na.locf(mergesdata_v, na.rm=TRUE)
X_v<-as.numeric(X_v)
#Filling for column "ws"
mergesdata_ws<- zoo(windforecast_wf_file$ws)
X_ws<-na.locf(mergesdata_ws, na.rm=TRUE)
X_ws<-as.numeric(X_ws)
#Filling for column "wd"
mergesdata_wd<- zoo(windforecast_wf_file$wd)
X_wd<-na.locf(mergesdata_wd, na.rm=TRUE)
X_wd<-as.numeric(X_wd)

#Binding date to the windforecast_wf1_file1 data
dateChangedData_wf<-data.frame(Date=addHour_wf,u=X_u,v=X_v,ws=X_ws,wd=X_wd)

#Taking mean of the data values u, v, ws and wd for common date+hour
aggregated_u<-aggregate(dateChangedData_wf$u ~ dateChangedData_wf$Date, dateChangedData_wf, mean )
aggregated_v<-aggregate(dateChangedData_wf$v ~ dateChangedData_wf$Date, dateChangedData_wf, mean )
aggregated_ws<-aggregate(dateChangedData_wf$ws ~ dateChangedData_wf$Date, dateChangedData_wf, mean )
aggregated_wd<-aggregate(dateChangedData_wf$wd ~ dateChangedData_wf$Date, dateChangedData_wf, mean )
xx<-cbind(aggregated_u,aggregated_v,aggregated_ws,aggregated_wd)
rr<-data.frame(xx[1],xx[2],xx[4],xx[6],xx[8])
colnames(rr)<-c("Date","u","v","ws","wd")

#Separated Date column into year, month, day and hour for prediction variables
year_wf<-year(rr$Date)
month_wf<-month(rr$Date)
day_wf<-day(rr$Date)
hour_wf<-hour(rr$Date)

#Converting back to the date hour format like an integer number with class as date
dateHour_wf<-format(rr$Date,"%Y%m%d%H")

#Final data with the unique rows with Date and corresponding u,v,ws,wd,Year,Month,Day and Hour 
finaldf<-data.frame(Date=dateHour_wf,rr[2:5],Year=year_wf,Month=month_wf,Day=day_wf,Hour=hour_wf)

#write.csv(finaldf, file="cleaneddata_wf1.csv")

a_finaldf<-data.frame(finaldf[1:13175,])
b_finaldf<-data.frame(finaldf[13176:26244,])

# File2: train.csv
train_file<-read.table(file.choose(), header=T, fileEncoding="UTF-8", sep=",")

#Adding only required columns for the windforecast files

#Run only when using windforecast_wf1.csv as File 1 
  #date_wp<-data.frame(Date=train_file$date, wp1=train_file$wp1)
#Run only when using windforecast_wf2.csv as File 1 
  #date_wp<-data.frame(Date=train_file$date, wp2=train_file$wp2)
#Run only when using windforecast_wf3.csv as File 1 
  #date_wp<-data.frame(Date=train_file$date, wp3=train_file$wp3)
#Run only when using windforecast_wf4.csv as File 1 
 #date_wp<-data.frame(Date=train_file$date, wp4=train_file$wp4)
#Run only when using windforecast_wf5.csv as File 1 
 #date_wp<-data.frame(Date=train_file$date, wp5=train_file$wp5)
#Run only when using windforecast_wf6.csv as File 1 
 #date_wp<-data.frame(Date=train_file$date, wp6=train_file$wp6)
#Run only when using windforecast_wf6.csv as File 1 
 #date_wp<-data.frame(Date=train_file$date, wp7=train_file$wp7)

#Merging the data by combining dateChangedData_wf and the respective predicted wp column
merged.data <- merge(finaldf, date_wp , by.x="Date", all.x = TRUE)

#Sorting out for the date ranges from 2009/07/01 to 2010/12/31
a_train<-merged.data[1:13175,]

#Sorting out for the date ranges from 2011/01/01 to 2012/06/28
b_test<-merged.data[13176:26244,]


#----Training Data------

#Replacing 0s with NA - wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
a_train$wp7[a_train$wp7 == 0] <- NA
#Handling NAs with na.locf - wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
mergeddata_wp<- zoo(a_train$wp7)
wp_nonna_a<-na.locf(mergeddata_wp, na.rm=TRUE)
wp_nonna_a<-as.numeric(wp_nonna_a)
#Adding the na.locf values to the wp1 column
a_train$wp7<-wp_nonna_a

#-----Testing data------

#The NA is replaced by 9999 so as to differentiate it at the time of na.locf
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
b_test$wp7[is.na(b_test$wp7)]<-9999

#Replacing 0s with NA - wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
b_test$wp7[b_test$wp7 == 0] <- NA
#Handling NAs with na.locf - wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
mergeddata_wp_b<- zoo(b_test$wp7)
wp_nonna_b<-na.locf(mergeddata_wp_b, na.rm=TRUE)
wp_nonna_b<-as.numeric(wp_nonna_b)

#Until here, zeroes replaced by na.locf, previous NAs replaced by 9999
#Changing 9999 back to the NA
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
b_test$wp7[b_test$wp7==9999]<-NA

#Binding date to the windforecast_wf1_file1 data
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
mergeddata_wp_final<-data.frame(Date=b_finaldf$Date,u=b_test$u, v=b_test$v, ws=b_test$ws, wd=b_test$wd, 
Year=b_test$Year, Month=b_test$Month, Day=b_test$Day, Hour=b_test$Hour, wp7=b_test$wp7)

#Regression, Neural Network and Regression trees to build prediction models

#-----1. Linear Regression Model------

#Training Data
training_Data <- a_train

#Building the model with training_Data
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
fit<-with(training_Data,lm(wp7~u+v+ws+wd+Hour+Year+Month+Day))
summary(fit)

#Testing Data
testing_Data<-mergeddata_wp_final
prediction<-predict.lm(fit, testing_Data)
 #wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
dataframe<-data.frame(wp7=prediction)

library(forecast)
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
acc_LR<-accuracy(prediction, training_Data$wp7)

#-----2. Regression Tree------

training_Data_RT <- a_train

library(tree)
library(MASS)
library(ISLR)
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
fit <- tree(wp6~u+v+ws+wd+Hour+Year+Month+Day, training_Data_RT)
summary(fit)

#Putting the file with NA values as the testing_Data_RT
testing_Data_RT<-mergeddata_wp_final
prediction<-predict(fit, testing_Data_RT)

#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
dataframe<-data.frame(wp7=prediction)

library(forecast)
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
acc_RT<-accuracy(prediction, training_Data_RT$wp7)

#-------3. Neural Network--------

library(MASS)

training_Data_NN <- a_train

index <- sample(1:nrow(training_Data_NN),round(0.75*nrow(training_Data_NN)))

training_Data_NN <- training_Data_NN[,-c(1)]

maxs <- apply(training_Data_NN, 2, max) 
mins <- apply(training_Data_NN, 2, min)

scaled <- as.data.frame(scale(training_Data_NN, center = mins, scale =maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

#model for neural network
library(neuralnet)
n <- names(train_)
#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
f <- as.formula(paste("wp7 ~", paste(n[!n %in% "wp7"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=5, threshold= 0.6,linear.output=F)
plot(nn)

#-------Predicting on the mergeddata_wp_final------

testing_Data_NN <- mergeddata_wp_final
testing_Data_NN <- testing_Data_NN[,-c(1)]
predictwp.nn <- neuralnet::compute(nn, testing_Data_NN[,1:8])

#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
pr.nn_nn <- predictwp.nn$net.result*(max(training_Data_NN$wp7)-min(training_Data_NN$wp7))+min(training_Data_NN$wp7)

#wp1, wp2, wp3, wp4, wp5, wp6, wp7 depending on number of the csv file you took
test.r <- (test_$wp7)*(max(training_Data_NN$wp7)-min(training_Data_NN$wp7))+min(training_Data_NN$wp7)
test.r_nn<-data.frame(test.r)
colnames(test.r_nn)<- c("wp7")


MSE.nn <- sum((test.r - pr.nn_nn)^2)

#error
error<- test.r - pr.nn_nn

#rmse
rmse <- function(error)
{
  sqrt(mean(error^2))
}
RMSE.nn<-rmse(error)

#mae
mae <- function(error)
{
  mean(abs(error))
}
MAE.nn<-mae(error)

#mape
mape<-function(error)
  mean(abs((error/test.r)))
MAPE.nn<-mape(error)

#----CSV for each windforecast file with the information of performance factors

error_lr<-cbind("Linear Regresion",acc_LR)
error_rt<-cbind("Regression Tree",acc_RT)
error_nn<- cbind("Neural Network",RMSE.nn,MAE.nn,MAPE.nn)
final_error<-cbind(error_lr,error_rt,error_nn)
View(t(final_error))
write.csv(t(final_error),"PerformanceMetrics_wf7.csv")


#-------The best model is the neural network model because the R-Squared and MAPE values are least for the neural network------ 

#Run only one at a time

#Run when taking windforecast_wf1.csv
 #wp1_Values<-data.frame(test.r_nn)

#Run when taking windforecast_wf2.csv
 #wp2_Values<- data.frame(test.r_nn)

#Run when taking windforecast_wf3.csv
 #wp3_Values<- data.frame(test.r_nn)

#Run when taking windforecast_wf4.csv
 #wp4_Values<- data.frame(test.r_nn)

#Run when taking windforecast_wf5.csv
 #wp5_Values<- data.frame(test.r_nn)

#Run when taking windforecast_wf6.csv
 #wp6_Values<- data.frame(test.r_nn)

#Run when taking windforecast_wf7.csv
 wp7_Values<- data.frame(test.r_nn)

#The predicted values of wp1, wp2, wp3, wp4, wp5, wp6 and wp7  
Benchmark<-cbind(wp1_Values, wp2_Values, wp3_Values, wp4_Values, wp5_Values, wp6_Values, wp7_Values)
  
#Replacing the NA locations of file in 
na_locations <- which(is.na(mergeddata_wp_final$wp7), arr.ind = TRUE)
mergeddata_wp_final$wp7[na_locations] <- Benchmark$wp7[na_locations]
wp7_Values_final<-mergeddata_wp_final$wp7
 
#Replaced wind values with the predicted values
BenchmarkFinal<-cbind(wp1=wp1_Values_final, wp2=wp2_Values_final, wp3=wp3_Values_final, wp4=wp4_Values_final, wp5=wp5_Values_final, wp6=wp6_Values_final, wp7=wp7_Values_final)

#The file with the unique identifier like in the benchmark.csv 
id<- seq.int(nrow(BenchmarkFinal))

FinalOutput<-cbind(id, mergeddata_wp_final[1] ,BenchmarkFinal)
View(FinalOutput)

benchmarkSample<-read.table(file.choose(), header=T, fileEncoding="UTF-8", sep=",")
names(benchmarkSample)[2] <- "Date"
View(benchmarkSample)

#merging<- sqldf("Select wp1,wp2,wp3,wp4,wp5,wp6,wp7 from FinalOutput join benchmarkSample using (Date)")
#mergedd<- inner_join(FinalOutput, benchmarkSample, by="Date")

mergedFinalData<- merge(benchmarkSample, FinalOutput, by = "Date")
View(mergedFinalData)

mergedFinalData<-mergedFinalData[-c(2:9)]
FinalOutputData<-cbind(id=mergedFinalData$id.y, Date=mergedFinalData$Date, wp1=mergedFinalData$wp1.y, wp2=mergedFinalData$wp2.y, wp3=mergedFinalData$wp3.y, wp4=mergedFinalData$wp4.y,
              wp5=mergedFinalData$wp5.y, wp6=mergedFinalData$wp6.y, wp7=mergedFinalData$wp7.y)
View(FinalOutputData)
write.csv(FinalOutputData, file="Benchmark.csv")


