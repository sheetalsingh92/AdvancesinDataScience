#read hourly filled data file
data<- read.csv(file.choose(),header = T)
head(data)
summary(data)
df<-data.frame(data)
View(df)
tree.train<- tree(KWh~Temperature+hour+month+day+year+Peakhour+DayofWeek+WeekDay,df_final,subset=train)
summary(tree.train)

#read forecastinputnewdata file
data2<-read.csv(file.choose(),header = T)
colnames(data2)<-c("Date","month","day","year","hour","DayofWeek","WeekDay","Peakhour","Temperature")
View(data2)
prediction<-predict(tree.train, data2)
View(prediction)

mean_KWh<- mean(prediction)
View(mean_KWh)

KWh_Class<-ifelse(prediction>mean_KWh,"Above_Normal","Optimal")
df_final<-data.frame(Day=data2$Date,Hour=data2$hour,Temperature=data2$Temperature,KWh_Class)
View(df_final)
write.csv(df_final,"forecastOutput_999999999_ClassificationTree.csv",row.names=FALSE)