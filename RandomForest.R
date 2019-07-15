install.packages('caTools')
install.packages('randomForest')
library(devtools)
library(caTools)
library(randomForest)
library(broom)
#Loading the datasets
df_train = read.csv('/home/user11da1/pml-training.csv')
df_test = read.csv('/home/user11da1/pml-testing.csv')

#basic descriptions of the data
head(df_train)
str(df_train)
colnames(df_train)
df_train$classe
head(df_test)

#Removing the NA columns 
rmcols1=sapply(df_train, function(x) any(is.na(x)))
rmcols2=sapply(df_train, function(x) "" %in% levels(x))
rmcols = rmcols1 | rmcols2

cleaned = df_train[,-which(rmcols)]

#Columns of the new cleaned data
str(cleaned)

#some columns are not important factor for the classification , so we can remove them also
cleaned = cleaned[,-1:-7]

#PCA of the dataset

cleaned_pca = prcomp(cleaned[,1:52],center=TRUE,scale. = TRUE)
summary(cleaned_pca)
str(cleaned_pca)   #PCA is not needed here since the almost all the characters has good significance

#Splitting the training data to train and test
set.seed(07)
sample  = sample.split(cleaned$classe,SplitRatio = 0.7)
TrainSet = subset(cleaned,sample==TRUE)
TestSet  = subset(cleaned,sample==FALSE)
dim(TrainSet)
dim(TestSet)

#Random Forest Classification
rfmodel = randomForest(classe~.,data=TrainSet,importance=FALSE)
rfpredict = predict(rfmodel,newdata=TestSet,type='class')
table(rfpredict,TestSet$classe)

#Accuracy on test split data
accuracy = (1673+1129+1018+955+1079)/(1673+1+9+1129+1+8+1018+1+10+955+3+1079)
accuracy  #Accuracy is .994

#Predicting for the test data set

rfpredict_test = predict(rfmodel,newdata=df_test,type='class')
rfpredict_test
