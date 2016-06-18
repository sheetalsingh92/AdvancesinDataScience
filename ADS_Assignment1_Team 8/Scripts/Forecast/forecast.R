data<- read.csv(file.choose(),header = T)
df <- data.frame(data)
install.packages("lubridate")
library(lubridate)
?lubridate
forecast_Year<-year(as.Date(df$Day, format="%m/%d/%Y"))
forecast_Month<-month(as.Date(df$Day, format="%m/%d/%Y"))
forecast_Day<-day(as.Date(df$Day, format="%m/%d/%Y"))
forecastdata<-cbind(df[1],forecast_Month,forecast_Day,forecast_Year,df[2])
View(forecastdata)

ordereddata_weekday <-(wday(as.Date(forecastdata$Day, format="%m/%d/%Y")))-1
ordereddata1<-cbind(forecastdata,ordereddata_weekday)
View(ordereddata1)

ordereddata_weekend<-NA
ordereddata2<-cbind(ordereddata1,ordereddata_weekend)
ordereddata_weekend<- ifelse( ordereddata1$ordereddata_weekday == 0 | ordereddata1$ordereddata_weekday == 6 , 0 , 1)
##warnings()
ordereddata2<-cbind(ordereddata1,ordereddata_weekend)
View(ordereddata2)

ordereddata_peakhours <-NA
finalforecastdata<-cbind(ordereddata1,ordereddata_weekend,ordereddata_peakhours)
ordereddata_peakhours <- ifelse( as.numeric(ordereddata1$Hour) > 6 &  as.numeric(ordereddata1$Hour) < 20 , 1 , 0)
finalforecastdata<-cbind(ordereddata1,ordereddata_weekend,ordereddata_peakhours,df[3])
colnames(finalforecastdata)<-c("Date","month","Day","Year", "Hour","Day of Week","WeekDay","Peakhour","Temperature")
View(finalforecastdata)

write.csv(finalforecastdata,file="finalforecastInput.csv")