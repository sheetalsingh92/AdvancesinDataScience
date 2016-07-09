#Import dataset 1 - ad.data 
table.data<-read.table(file.choose(), header=F, fileEncoding="UTF-8", sep=",")

#Import dataset 2 - ad.names
table.names<-read.csv(file.choose(),header=TRUE)

#Putting the data into a data frame
names_rm1<-data.frame(table.names)

#Installing the library to convert the row names to first column
install.packages("data.table")
library(data.table)
new<-setDT(names_rm1, keep.rownames = TRUE)[]

#Removing the first row ad [nonad|classes]
names_rm2 <- new[-c(1),]

#Removing the additional rows with no binary values (pipeline)
names_rm3<-names_rm2[!(names_rm2$X..w..c4.5.alladA.names.file....automatically.generated.=="")]

#binding the three rows in the beginning - height, width, ratio
names_rm4<-rbind(names_rm2[c(1:3),],names_rm3)

#Changing the name of the column by removing substrings - colons and zeroes in the end
clean_names_rm2<-sub(":.*","",names_rm4$rn)
removingcolon<-as.data.frame(clean_names_rm2)

#Transpose with column name as the first rows
transpose_names<-t(removingcolon)
colnames(transpose_names) <- transpose_names[1, ] 

#Changing transpose_names matrix to dataframe
df1<-as.data.frame(transpose_names)

#Replacing "?" with the NA and changing the table.data to a data frame
df2<-as.data.frame(sapply(table.data,sub,pattern='\\?',replacement=NA))

#Removing the last column ad from df2 dataset
df2$V1559<-NULL

#Column names of transpose_names as Column names of table.data
colnames(df2) <- colnames(df1)


#Changing the 473rd, 534th and 956th column name because of multibyte error due to some absurd characters
colnames(df2)[colnames(df2)=='origurl*target+\xfc\xbe\x99\x96\x84\xbcion']<-'origurl*target'
colnames(df2)[colnames(df2)=='origurl*\xfc\xbe\x99\x96\x84\xbcion+0']<-'origurl*534'
colnames(df2)[colnames(df2)=='origurl*\xfc\xbe\x99\x96\x84\xbcion']<-'origurl*956'

#Replacing the NA values in height with mean of the height column
height<-as.numeric(as.character((df2$height)))
install.packages("gtools")
library(gtools)
mean_height<-mean(height, na.rm=TRUE)
height<-na.replace(height, mean_height)
df2$height<-height
#View(df2$height)

#Replacing the NA values in width with mean of the width column
width<-as.numeric(as.character((df2$width)))
mean_width<-mean(width, na.rm=TRUE)
width<-na.replace(width, mean_width)
df2$width<-width

#Finding out the third column's NA values by dividing height by width  
aratio<-as.numeric(as.character((df2$aratio)))
aratio_rep<-na.replace(aratio, 0)
df2$aratio<-aratio_rep
na_locations <- which(df2$aratio==0, arr.ind = TRUE)
df2$aratio[na_locations] <- df2$width[na_locations]/df2$height[na_locations]


#Adding the status column that would tell id the advertisement is ad or nonad
df3<-cbind(df2,Status=table.data$V1559)


#Remove the rows with NA values in "local" column
omitted_na<-na.omit(df3)

write.csv(omitted_na,"inputforinternetad_final.csv",row.names=FALSE)

#Logistic Regression-----------##

#Read the cleaned csv file
data<- read.csv(file.choose(),header = T)

#store the read input into a dataframe
df<-data.frame(data)


# Take 75% of the data as the sample data
smp_size<-floor(0.75*nrow(df))
set.seed(123)

#Divide the data into train and test data. Sample data is basically train data
train_ind<-sample(seq_len(nrow(df)),size=smp_size)
train <- df[train_ind,]

# Rest 25% is test data
test <- df[-train_ind,]

# Use glm function to construct a logistic regression model for factor Status using all other variables
fit<-glm(Status~.,data=train,family=binomial(link="logit"))
summary(fit)

#Use predict() function to predict the probabilities of the outcome
test.probs<-predict(fit,test,type='response')

#Divide the predicted status based on probability values
pred<- rep("nonad.",length(test.probs))
pred[test.probs<=0.5]<-"ad."

#package to create confusion matrix
install.packages("caret")
library(caret)

#create the error table
logisticregression_table<-table(pred,test$Status)

#create the confusion matrix
confusionMatrix(test$Status,pred)

#calculate the error percentage
Error_logistic_regression <- (((logisticregression_table[1,2]) + (logisticregression_table[2,1])) /((logisticregression_table[2,1]) + (logisticregression_table[1,2]) + (logisticregression_table[1,1])+(logisticregression_table[2,2])))
Error_logistic_regression *100

#create ROC curve
install.packages("ROCR")
library(ROCR)
prediction <- prediction(test.probs, test$Status)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#create Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")



##--------Classification Trees........##
library(tree)

#---create the regression tree
tree <- tree(Status~.,df)
summary(tree)
plot(tree)
text(tree,pretty=0)

#create the training and test data
set.seed(2)
smp_size<-floor(0.90*nrow(df))
set.seed(123)
train<-sample(seq_len(nrow(df)),size=smp_size)
df.test<-df[-train,]
Status.test <- df$Status[-train]

#Use tree function to construct a classification tree model for factor Status using all other variables
fit<- tree(Status~.,df,subset=train)
summary(fit)

#Use predict() function to predict the probabilities of the outcome
tree.pred = predict(fit,df.test,type="class")
#tree.pred = predict(fit,df.test,type="vector")
#create classification tree table
classification_tree<-table(tree.pred,Status.test)

#create confusion matrix
confusionMatrix(Status.test,tree.pred)


#calculate error percentage
Error_classification_tree <- (((classification_tree[1,2]) + (classification_tree[2,1])) /((classification_tree[2,1]) + (classification_tree[1,2]) + (classification_tree[1,1])+(classification_tree[2,2])))
Error_classification_tree *100


#create ROC curve
install.packages("ROCR")
library(ROCR)
str(tree.pred)
prediction <- prediction(tree.pred[,1], Status.test$Status)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#create Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")






##-----Neural Network-----------##

#library for neural network functions
library(nnet)
install.packages("NeuralNetTools")
library(NeuralNetTools)
#library to create confusion matrix
library(caret)

#read the cleaned csv file(inputforad.csv file)
data<- read.csv(file.choose(),header = T)
df<-data.frame(data)


#create status_updated column with '0'and '1' values corresponding to the status column
df["status_updated"]<-NA
df$status_updated[df$Status == "ad."]<-0
df$status_updated[df$Status == "nonad."]<-1


#create train and test data 
set.seed(2)
smp_size<-floor(0.50*nrow(df))
set.seed(123)
train<-sample(seq_len(nrow(df)),size=smp_size)
test<-df[-train,]


#Use nnet  function to construct a neural network model for factor Status-updated using all other variables
seedsANN = nnet(status_updated~.,df[train,], size=3,rang = 0.1,
                decay = 5e-4, maxit = 350,MaxNWts = 5000)

#Use predict() function to predict the probabilities of the outcome
pr<-predict(seedsANN, test)


#Divide the predicted status based on probability values
pred<- rep(1,length(pr))
pred[pr<=0.5]<-0


#create the error table 
neural_network_table<-table(pred,test$status_updated)


#create confusion matrix
confusionMatrix(test$status_updated,pred)



#calculate error percentage
Error_neural_network <- (((neural_network_table [1,2]) + (neural_network_table [2,1])) /(( neural_network_table [2,1]) + (neural_network_table [1,2]) + (neural_network_table [1,1])+( neural_network_table [2,2])))
 Error_neural_network *100


#create ROC curve
install.packages("ROCR")
library(ROCR)
prediction <- prediction(pr, test$status_updated)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#create Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")





