data<- read.csv(file.choose(),header = T)
df <- data.frame(data)
data_rawdata<-df[df$Units!= 'Power Factor' & df$Units!= 'kVARh', ]

attach(data_rawdata)
aggregate_12<-sapply(seq(5,292,by=12),function(i) rowSums(data_rawdata[,i:(i+11)]))
View(aggregate_12)
nrow(aggregate_12)

data_rawdata_dframe<-cbind(data_rawdata[1:4],aggregate_12)
View(data_rawdata_dframe)
install.packages("reshape")
library(reshape)
data_rawdata_meltdata <- melt(data_rawdata_dframe, id=c("Account","Date","Channel","Units"))
View(data_rawdata_meltdata)

install.packages("doBy")
library(doBy)
ordereddataframe <- data_rawdata_meltdata[order(data_rawdata_meltdata$Date),] 
View(ordereddataframe)
ordereddata<-data.frame(ordereddataframe)
attach(ordereddata)
library(data.table)

dataframe_nonzero<-ordereddata[ which( ! ordereddata$value %in% 0.000) , ]
View(dataframe_nonzero)

install.packages("lubridate")
library(lubridate)
ordereddata_year<-year(as.Date(dataframe_nonzero$Date, format="%m/%d/%Y"))
ordereddata_month<-month(as.Date(dataframe_nonzero$Date, format="%m/%d/%Y"))
ordereddata_day<-day(as.Date(dataframe_nonzero$Date, format="%m/%d/%Y"))
ordereddata_weekday <-(wday(as.Date(dataframe_nonzero$Date, format="%m/%d/%Y")))-1

##dd<-as.Date(newdata$Date)
ordereddata1<-cbind(dataframe_nonzero[1:6],ordereddata_year,ordereddata_month,ordereddata_day,ordereddata_weekday)
View(ordereddata1)


ordereddata_weekend<-NA
ordereddata2<-cbind(ordereddata1,ordereddata_weekend)
ordereddata_weekend<- ifelse( ordereddata1$ordereddata_weekday == 0 | ordereddata1$ordereddata_weekday == 6 , 0 , 1)
##warnings()
ordereddata2<-cbind(ordereddata1,ordereddata_weekend)
View(ordereddata2)


ordereddata_peakhours <-NA
ordereddata3<-cbind(ordereddata1,ordereddata_weekend,ordereddata_peakhours)
ordereddata_peakhours <- ifelse( as.numeric(ordereddata1$variable) > 6 &  as.numeric(ordereddata1$variable) < 20 , 0 , 1)
##Dates <- as.Date(newdata$Date, "%m/%d/%Y")
FinalDate <- as.Date(ordereddata3$Date, format="%m/%d/%Y")
Hour<-as.numeric(ordereddata3$variable)-1
ordereddata3<-cbind(ordereddata2,ordereddata_peakhours,FinalDate,Hour)
View(ordereddata3)


install.packages("weatherData")
library(weatherData)
weather_data<-getWeatherForDate("KBOS","2014-1-1","2014-12-31", opt_detailed = TRUE,opt_all_columns=TRUE)
View(weather_data)
weather_data_aggregate<-aggregate(list(temperature = weather_data$TemperatureF), list(Time = cut(weather_data$Time, "1 hour")), mean)

View(weather_data_aggregate)
weather_hour <- format(as.POSIXct(weather_data_aggregate$Time),"%H")
Hour<- as.numeric(weather_hour)
View(Hour)
finalweatherdata = cbind(weather_data_aggregate,Hour)
View(finalweatherdata)


final_rawdata<-data.frame(ordereddata3)
View(final_rawdata)
FinalDate <- as.Date(finalweatherdata$Time)
final_weatherdata<-cbind(finalweatherdata,FinalDate)
colnames(final_weatherdata)<-c("Time","Temperature","Hour","FinalDate")
View(final_weatherdata)

mergeddata<-merge(x = final_rawdata,final_weatherdata,by=c("FinalDate","Hour"),all.x=TRUE)

View(mergeddata)
z <-mean(mergeddata$Temperature, na.rm = TRUE)
mergeddata[is.na(mergeddata)] <- z
final<-cbind(mergeddata[2:14],mergeddata$Temperature)
View(final)
newdataformat<-cbind(final[2:3],KWh=final$value,month=final$ordereddata_month,day = final$ordereddata_day,year= final$ordereddata_year,hour= final$Hour,DayofWeek = final$ordereddata_weekday, WeekDay = final$ordereddata_weekend, Peakhour = final$ordereddata_peakhours,Temperature = mergeddata$Temperature) 
View(newdataformat)
newdataformat_dframe <- data.frame(newdataformat)
View(newdataformat_dframe)
write.csv(newdataformat_dframe,file="Assignment2_part1_Q1Nonzerodataset_aa.csv")