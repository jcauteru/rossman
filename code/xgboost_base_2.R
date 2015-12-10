library(readr)
library(lubridate)
library(randomForest)

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

range_cap <- list(eta_min=.001, eta_max=.3, 
                  max_depth=8, subsample=.9, colsample_bytree=.9,
                  alpha=.01, lambda=.01, rounds_max=5000, 
                  rounds_min=100, rounds_step=500)

search <- CREATE_GRID(range_cap)

# ETA Tune:
eta_tune_res <- SEARCH_GRID_ETA(search$eta_round, testing_data, 
                                training_data, 1)

#################################################
#################################################

GB_GRID_TRAIN <- function(test_data, train_data, target_index){
  
  param_in <- list( objective           = "reg:linear", 
                    booster = "gbtree",
                    eta                 = .09, # 0.06, #0.01,
                    max_depth           = 10,
                    subsample           = 0.7, # 0.7
                    colsample_bytree    = 0.7 # 0.7
  )
  
  rounds <- 600
  
  dval<-xgb.DMatrix(data=data.matrix(test_data[, -target_index]),
                    label=test_data[, target_index])
  dtrain<-xgb.DMatrix(data=data.matrix(train_data[, -target_index]),
                      label=train_data[, target_index])
  
  watchlist<-list(val=dval,train=dtrain)
  
  clf <- xgb.train(params = param_in, data = dtrain, 
                   nrounds = rounds, verbose = 1,
                   early.stop.round = 100,
                   watchlist = watchlist,
                   maximize = FALSE, feval=RMPSE
  )
  
  return(clf)
  
}

mod <- GB_GRID_TRAIN(holdout_log, training_data, 1)
pred1 <- exp(predict(mod, data.matrix(testing_data[,feature.names])))
errors <- PSE(pred1, exp(testing_data[,c('Sales')]))


importance_matrix <- xgb.importance(model = mod)
xgb.plot.importance(importance_matrix)

preds <- read.csv("rf1.csv", header = T, stringsAsFactors = F)
pred1 <- exp(predict(mod, data.matrix(testing_enahnced[,feature.names])))

by(errors, exp(testing_data$StoreType), mean)


#pred1[testing_enahnced$Open == 0 & testing_enahnced$DayOfWeek == 7] <- 0
submission <- data.frame(Id=testing_enahnced$Id, Sales=pred1)
preds$Sales2 <- preds$Sales
merged  <- merge(submission, preds[, c('Id', 'Sales2')], all.x=T)
splicer <- rowMeans(merged[, 2:3])
submission <- data.frame(Id=merged$Id, Sales=splicer)
cat("saving the submission file\n")
write_csv(submission, "xgb_feat_eng.csv")

## Getting what wrong:
SGOBS_ER <- function(preds, act){
  res <- ((act - preds)/act)^2
}

raw_df <- training_enahnced[h ,feature.names]

# RF TRY:
myNtree = 500
myMtry = 5
myImportance = TRUE

casualFit <- randomForest(log(Sales)~. , data=training_enahnced[h ,c('Sales', feature.names)], 
                          ntree=myNtree, mtry=myMtry, importance=myImportance)

