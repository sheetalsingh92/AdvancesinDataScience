## read hourly filled data file
data<- read.csv(file.choose(),header = T)
head(data)
summary(data)
df<-data.frame(data)

mean_KWh<- mean(df$KWh)
View(mean_KWh)

KWh_Class<-ifelse(df$KWh>mean_KWh,"Above_Normal","Optimal")
df_final<-data.frame(df,KWh_Class)
write.csv(df_final,"Hourly_filled_data_withkwhclass.csv",row.names=FALSE)
View(df_final)
a <-data.frame(df_final)
data(a,package="AER")
df_final["abs"]<-NA
df_final$abs[df_final$KWh_Class == "Above_Normal"]<-1
df_final$abs[df_final$KWh_Class == "Optimal"]<-0
df_final$abs<-factor(df_final$abs,level=c(0,1),labels=c("No","Yes"))
table(df_final$abs)

smp_size<-floor(0.75*nrow(df_final))
set.seed(123)
train_ind<-sample(seq_len(nrow(df_final)),size=smp_size)
train <- df_final[train_ind,]
test <- df_final[-train_ind,]

fit<-glm(abs~Temperature+hour+month+day+year+Peakhour+DayofWeek+WeekDay,data=train,family=binomial(link="logit"))
summary(fit)

test.probs<-predict(fit,test,type='response')
pred<- rep("No",length(test.probs))
pred[test.probs>=0.5]<-"Yes"
install.packages("caret")
library(caret)

logisticregression_table<-table(pred,test$KWh_Class)

Error <- (((logisticregression_table[1,2]) + (logisticregression_table[2,1])) /((logisticregression_table[2,1]) + (logisticregression_table[1,2]) + (logisticregression_table[1,1])+(logisticregression_table[2,2])))
ErrorFin <- Error*100
logisticregression <-cbind("Error",ErrorFin)

l_regression<-cbind("Logistic Regression",logisticregression_table)




#-------------------------------------------------------------------------
##Classification Tree


## read hourly filled data file
data<- read.csv(file.choose(),header = T)
head(data)
summary(data)
df<-data.frame(data)

mean_KWh<- mean(df$KWh)
View(mean_KWh)

KWh_Class<-ifelse(df$KWh>mean_KWh,"Above_Normal","Optimal")
df_final<-data.frame(df,KWh_Class)
View(df_final)

library(tree)
tree <- tree(KWh_Class~Temperature+hour+month+day+year+Peakhour+DayofWeek+WeekDay,df_final)
summary(tree)
plot(tree)
text(tree,pretty=0)

#-------Without pruning-----------
set.seed(2)
train <- sample(1:nrow(df_final),200)
df_final.test<-df_final[-train,]
KWh_Class.test <- KWh_Class[-train]
tree.train<- tree(KWh_Class~Temperature+hour+month+day+year+Peakhour+DayofWeek+WeekDay,df_final,subset=train)
summary(tree.train)
tree.pred = predict(tree.train,df_final.test,type="class")
classification_tree<-table(tree.pred,KWh_Class.test)
View(classification_tree)

Error <- (((classification_tree[1,2]) + (classification_tree[2,1])) /((classification_tree[2,1]) + (classification_tree[1,2]) + (classification_tree[1,1])+(classification_tree[2,2])))
ErrorFin <- Error*100
classification_tree_fin <-cbind("Error",ErrorFin)


ctree<-cbind("Classification Tree",classification_tree)
View(ctree)


confusionMatrix(KWh_Class.test,tree.pred)
write.csv(csv_final,"classification_tree_withoutpruning.csv")

## Error rate is 16.38%

#-----Pruning of data-----------
set.seed(3)
cv.df_final <- cv.tree(tree, FUN = prune.misclass)
names(cv.df_final)
cv.df_final

prune.df_final=prune.misclass(tree,best=9)
summary(prune.df_final)
plot(prune.df_final)
text(prune.df_final,pretty=0)

prune.pred <-predict(prune.df_final,df_final.test,type="class")
confusionMatrix(KWh_Class.test,prune.pred)
table(prune.pred,KWh_Class.test)

##error rate is 11.34%


finalprediction<-cbind(l_regression,ctree)
t(finalprediction)
write.csv(t(finalprediction),"ClassificationPerformanceMatrix.csv")






