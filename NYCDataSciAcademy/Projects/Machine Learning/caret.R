##TRYING SIMPLE GBM

# install.packages("caret")
# install.packages("mlbench")
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(Metrics)
library(plyr)
library(dplyr)

train<-read.csv('train.csv')
test<-read.csv('test.csv')
train.index <- train[,1]
test.index <- test[,1]
train.loss <- train[,ncol(train)]
bulk <- rbind(train[,-ncol(train)], test)
bulk$id <- NULL

#colnames(bulk)
##Converting categories to numeric
#this is done by first splitting the binary level, multi-level, and
#continuous variables
#colnames(all.train)
bin.train <- bulk[,1:72]
cat.train <- bulk[,73:116]
cont.train <- bulk[,117:130]
##Combine levels
#combining multiple levels using combine.levels
#minimum 5%
temp <- sapply(cat.train, combine.levels, minlev = 0.02)
temp <- as.data.frame(temp)
str(temp)
#cbind binary and reduced categorical levels
# comb.train <- cbind(bin.train, cat.train)
comb.train <- cbind(bin.train, temp)
##Dummify all factor variables
dmy <- dummyVars(" ~ .", data = comb.train, fullRank=T)
temp <- as.data.frame(predict(dmy, newdata = comb.train))
dim(temp)
##Combine dummified with cont vars
bulk <- cbind(temp, cont.train)
dim(bulk)
#split dataset into new train and new test with combine
train.e = bulk[1:nrow(train),]
test.e = bulk[(nrow(train)+1):nrow(bulk),]
#re-attach index
train.e <- cbind(train.index, train.e)
test.e <- cbind(test.index, test.e)
#re-attach log transformed loss
train.e$loss <- log(train.loss)
train <- train.e
test <- test.e

###writing to file
#write.csv(all.cd.train, "allcdtrain.csv")
###reading saved file
#all.cd.train <- read.csv("allcdtrain.csv", row.names = "X")
# #exploring correlation
# # calculate correlation matrix
# correlationMatrix <- cor(all.cd.train[,-219])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
#
#
# ###THE PART BELOW TAKES VERY LONG TO FINISH!!!
# #exploring feature ranking
# # prepare training scheme
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(loss~., data=all.cd.train, method="gbm", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)
### trying script from Lecture
#split dataset
set.seed(0)
trainIdx <- createDataPartition(train$loss,
                                p = .8,
                                list = FALSE,
                                times = 1)
subTrain <- train[trainIdx,] %>% select(-loss)
subTest <- train[-trainIdx,] %>% select(-loss)
lossTrain <- train$loss[trainIdx]
lossTest <- train$loss[-trainIdx]
dim(subTrain)
dim(lossTrain)

#removing index
train.subindex <- subTrain$train.index
test.subindex <- subTest$train.index
subTrain$train.index <- NULL
subTest$train.index <- NULL

# lmFit <- train(x = subTrain,
#                y = lossTrain,
#                method = "lm")
# #lmFit <- readRDS("lm_model")
# ###Checking variableimportance
# lmImp <- varImp(lmFit, scale = FALSE)
# lmImp
# ####
##select variables from here. proceeding with gbm with all variables
####
#How to change the optimizer in caret.
#It defaults to trying to minimize RMSE.
#You can change that to MAE (which is what kaggle wants) with the below code.
x_mae <-function (data, lev = NULL, model = NULL,...)
{
  require(Metrics)
  m <- try(Metrics::mae(exp(data$obs), exp(data$pred)),silent=TRUE)
  out<-c(m)
  names(out) <- c("MAE")
  out
}
#Model training
fitCtrl <- trainControl(method = "repeatedcv",
                        number = 5,
                        verboseIter = TRUE,
                        summaryFunction= x_mae)
gbmGrid <- expand.grid( n.trees = seq(100,600,100),
                        interaction.depth = c(3,7),
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
gbmFit <- train(x = subTrain,
                y = lossTrain,
                method = "gbm",
                trControl = fitCtrl,
                tuneGrid = gbmGrid,
                metric = 'MAE',
                maximize = FALSE)

#predict and create submit file
# Predict Hazard for the test set
submission <- data.frame(Id=test.index)
colnames(submission)              
submission$loss<- exp(predict(gbmFit, test))
write_csv(submission, "gbm_minlev2.csv")
dim(submission)
#
# #Draw the ROC curve
# gbm.probs <- predict(gbm.tune,testX,type="prob")
# head(gbm.probs)
#
# gbm.ROC <- roc(predictor=gbm.probs$PS,
# response=testData$Class,
# levels=rev(levels(testData$Class)))
# gbm.ROC$auc
# #Area under the curve: 0.8731
# plot(gbm.ROC,main="GBM ROC")