library(data.table)  
library(randomForest)
library(foreach)

source('/media/hdd/kaggle/rossman/code/make_data.R')
source('/media/hdd/kaggle/rossman/code/GXB_GRIDCV.R')

RMPSE2 <- function(preds, dtrain) {
  elab<-as.numeric(dtrain)
  epreds<-as.numeric(preds)
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}

PSE <- function(preds, dtrain) {
  elab<-as.numeric(dtrain)
  epreds<-as.numeric(preds)
  err <- (epreds/elab-1)^2
  return(err)
}

#################################################
#################################################

glb_hld <- sample(1:nrow(training_enahnced), 40000)
holdout <<- training_enahnced[glb_hld,  c('Sales', feature.names)]
holdout_log <- holdout
holdout_log$Sales <- log(holdout$Sales)
training_enahnced <- training_enahnced[order(train_gbl$Date),]
#h <- training_enahnced[train$Date >= as.Date('2015-07-31'), ]
h <- which(train_gbl$Date > as.Date('2015-06-01'))

training_data <- training_enahnced[-c(glb_hld, h), c('Sales', feature.names)]
training_data$Sales <- log(training_data$Sales)
testing_data <- training_enahnced[h, c('Sales', feature.names)]
testing_data$Sales <- log(testing_data$Sales)

H20 <- h2o.init(nthreads=-1,max_mem_size='20G', ice_root = '/media/hdd/kaggle/h2olock')
## Load data into cluster from R
training_data$Store=as.factor(training_data$Store)
testing_data$Store=as.factor(testing_data$Store)
trainHex <- as.h2o(training_data)
testHex <- as.h2o(testing_data)
features<-feature.names
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features, y="Sales", training_frame=trainHex, ntrees=60, max_depth=30, nbins_cats = 1115)
#, max_depth = 30,nbins_cats = 1115


## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rfHex,testHex))
## Return the predictions to the original scale of the Sales data
pred <- expm1(predictions[,1])
RMPSE2(pred, exp(testing_data$Sales))
summary(pred)


submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write.csv(submission, "h2o_rf.csv",row.names=F)
