## read hourly filled data file
data<- read.csv(file.choose(),header = T)
head(data)
summary(data)
df<-data.frame(data)
library(nnet)

mean_KWh<- mean(df$KWh)
View(mean_KWh)

KWh_Class<-ifelse(df$KWh>mean_KWh,"Above_Normal","Optimal")
df_final<-data.frame(df,KWh_Class)
View(df_final)

train<- sample(1:nrow(df_final),5359)
test<-setdiff(1:nrow(df_final),train)

#ideal<-class.ind(df_final$KWh_Class)

seedsANN = nnet(class.ind(KWh_Class)~Temperature+hour+month+day+year+Peakhour+DayofWeek+WeekDay, df_final[train,], size=10,  softmax=TRUE)

predict(seedsANN,df_final[train,-12], type="class")
output<-table(predict(seedsANN, df_final[test,-12], type="class"),df_final[test,]$KWh_Class)



##--------------------------------------------------
install.packages("neuralnet")
library(neuralnet)
neuralnet <- neuralnet(KWh_Class~Temperature+hour+month+day+year+Peakhour+DayofWeek+WeekDay, data=infert, 
                       hidden=3, err.fct="sse", linear.output=FALSE)
neuralnet$result.matrix
plot(neuralnet)
##-------------------------------------------------

write.csv(output,"ClassificationPerformanceMetrics.csv")