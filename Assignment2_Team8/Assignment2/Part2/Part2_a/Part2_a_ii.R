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
csv_final<- rbind(classification_tree_fin,"",classification_tree)
confusionMatrix(KWh_Class.test,tree.pred)
table(tree.pred,KWh_Class.test)
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


