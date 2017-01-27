# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(xgboost)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

train_1 <- read_csv("train.csv")
test <- read_csv("test.csv")

#Converting Char variables to Numeric

feature.names <- names(train_1)[c(2:131)]

for (f in feature.names) {
  if (class(train_1[[f]])=="character") {
    cat("VARIABLE : ",f,"\n")
    levels <- unique(c(train_1[[f]], test[[f]]))
    train_1[[f]] <- as.integer(factor(train_1[[f]], levels=levels))
    test[[f]] <- as.integer(factor(test[[f]],  levels=levels))
  }
}

#converting Response to Log

train_1$log_loss <- log(train_1$loss)

tra1 <- train_1[,feature.names]

dtrain <- xgb.DMatrix(data = data.matrix(tra1[,]),
                      label = train_1$log_loss)

watchlist <- list(train = dtrain)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- as.numeric(sum(abs(labels - preds)))/length(labels)
  return(list(metric = "mae error", value = err))
}

param <- list(  objective = "reg:linear",
                booster = "gbtree",
                eval_metric = evalerror,
                eta = 0.01,
                max_depth = 8,
                subsample = 0.8,
                colsample_bytree = 0.8,
                min_child_weight = 25
)

clf <- xgb.train(   params = param,
                    data = dtrain,
                    nrounds = 500,
                    verbose = 2,
                    watchlist = watchlist,
                    maximize = FALSE
)

test1 <- test[,feature.names]
dim(test)

pred1 <- exp(predict(clf, data.matrix(test1)))
submission <- data.frame(id=test$id, loss=pred1)
cat("saving the submission file\n")
write_csv(submission, "XgBoost_Run_DS.csv")