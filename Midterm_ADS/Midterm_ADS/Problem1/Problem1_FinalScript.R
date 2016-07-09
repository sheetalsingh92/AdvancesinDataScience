#package to create confusion matrix
install.packages("caret")
install.packages('e1071',dependencies=TRUE)
library(e1071)
library(caret)

#package and library to create ROC curve
install.packages("ROCR")
library(ROCR)


#Import the following csv file for the data input and cleaning
#default of credit card clients.xls
#skip funciton skips the first row of the data which is the roman numerals headings
data<- read.csv(file.choose(),header =TRUE, skip=1)
df <- data.frame(data)
View(df)


#count the occurence of specific payment status for every customer
levels=unique(do.call(c,df[,c(7:12)]))
out<-sapply(levels,function(x)rowSums((df[,c(7:12)])==x))

#Assign level names i.e Payment Status(-2,-1....) as column name
colnames(out)<-levels

#create a data frame with new values
df<-data.frame(df[1:6],out,df[13:25])

#exclude rows where all bill amount and payment amount is zero
df<-df[apply(df[,13:24],1,function(x) !all(x==0)),]

#assign names to columns of the data frame
colnames(df)<-c("ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE","STATUS2","STATUS_negative_1","STATUS0","STATUS_negative_2","STATUS1","STATUS3","STATUS4","STATUS8","STATUS7","STATUS5","STATUS6","BILL_SEPT","BILL_AUG","BILL_JULY","BILL_JUNE","BILL_MAY","BILL_APRIL","PAY_SEPT","PAY_AUG","PAY_JULY","PAY_JUNE","PAY_MAY","PAY_APRIL","default_response")

write.csv(df,"problem1_input_cleanedfile.csv",row)
df$default_response<-as.factor(df$default_response)

##---------Logistic Regression--------##



# Take 75% of the data as the sample data
smp_size<-floor(0.60*nrow(df))
set.seed(123)

#Divide the data into train and test data. Sample data is basically train data
train_ind<-sample(seq_len(nrow(df)),size=smp_size)
train <- df[train_ind,]

# Rest 25% is test data
test <- df[-train_ind,]


# Use glm function to construct a logistic regression model for factor Status using all other variables
fit<-glm(default_response~.,data=train,family=binomial(link="logit"))
summary(fit)

#Use predict() function to predict the probabilities of the outcome
test.probs<-predict(fit,test,type='response')

#Divide the predicted status based on probability values
pred<- rep(1,length(test.probs))
pred[test.probs<=0.5]<-0

#create the error table
logisticregression_table<-table(pred,test$default_response)

#calculate the error percentage
Error_logistic_regression <- (((logisticregression_table[1,2]) + (logisticregression_table[2,1])) /((logisticregression_table[2,1]) + (logisticregression_table[1,2]) + (logisticregression_table[1,1])+(logisticregression_table[2,2])))
Error_logistic_regression *100

#create the confusion matrix
confusionMatrix(test$default_response,pred)


#create ROC curve
prediction <- prediction(test.probs, test$default_response)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")


#----------Classification Tree----##

#---create the regression tree
library(tree)
tree <- tree(default_response~.,df)
summary(tree)
plot(tree)
text(tree,pretty=0)

#create the training and test data
set.seed(2)
smp_size<-floor(0.60*nrow(df))
set.seed(123)
train<-sample(seq_len(nrow(df)),size=smp_size)
df.test<-df[-train,]
default_response.test <- df$default_response[-train]

#Use tree function to construct a classification tree model for factor Status using all other variables
tree.train<- tree(as.factor(default_response)~.,df,subset=train)
summary(tree.train)


#Use predict() function to predict the probabilities of the outcome
tree.pred = predict(tree.train,df.test,type="class")


#create classification tree error table
classification_tree<-table(tree.pred,default_response.test)



#calculate error percentage 
Error <- (((classification_tree[1,2]) + (classification_tree[2,1])) /((classification_tree[2,1]) + (classification_tree[1,2]) + (classification_tree[1,1])+(classification_tree[2,2])))
ErrorFin <- Error*100
#classification_tree_fin <-cbind("Error",ErrorFin)


#create confusion matrix
confusionMatrix(default_response.test,tree.pred)


#create ROC curve
prediction <- prediction(tree.pred, default_response.test)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")




#---------Neural Network---------##

#library for neural network functions
library(nnet)
install.packages("NeuralNetTools")
library(NeuralNetTools)
#library to create confusion matrix
library(caret)

#create train and test data 
set.seed(2)
smp_size<-floor(0.60*nrow(df))
set.seed(123)
train<-sample(seq_len(nrow(df)),size=smp_size)
test<-df[-train,]


#Use nnet  function to construct a neural network model for factor default_response using all other variables
seedsANN = nnet(default_response~.,df[train,], hidden=3,size=3,rang = 0.1,
                decay = 5e-4, maxit = 350,MaxNWts = 2000)

#Use predict() function to predict the probabilities of the outcome
pr<-predict(seedsANN, test)

#plot the neural network
plotnet(seedsANN,alpha=0.5)
#Divide the predicted status based on probability values
pred<- rep(1,length(pr))
pred[pr<=0.25]<-0

#create the error table 
neural_network_table<-table(pred,test$default_response)

#create confusion matrix
confusionMatrix(test$default_response,pred)

#calculate error percentage
Error_neural_network <- (((neural_network_table [1,2]) + (neural_network_table [2,1])) /(( neural_network_table [2,1]) + (neural_network_table [1,2]) + (neural_network_table [1,1])+( neural_network_table [2,2])))
 Error_neural_network *100

#create ROC curve
install.packages("ROCR")
library(ROCR)
prediction <- prediction(pr, test$default_response)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#create Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")




















