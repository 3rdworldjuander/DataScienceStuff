#### Random Forest ####
library(randomForest)
library(dplyr)
set.seed(0)

all.train<-read.csv('train.csv', row.names = "id")
all.test<-read.csv('test.csv', row.names = "id")

t_index = sample(1:nrow(all.train), 7*nrow(all.train)/10)

#removing variables with levels >53
#cat109, cat110, cat113, cat116
sub.train <- all.train %>% select(c(-cat109, -cat110, -cat113, -cat116))
str(sub.train[,99:127])

#fitting initial randomForest
rf <- randomForest(loss ~ ., data = sub.train, subset = t_index, importance = TRUE)

###crashed before finishing