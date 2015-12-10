library(readr)
library(xgboost)
library(lubridate)
source('/media/hdd/kaggle/rossman/code/makeTs.R')
setwd('/media/hdd/kaggle/rossman/data')

#################################################
########## READ IN DATA SETS ####################
train <- read.csv("train.csv", header = T, stringsAsFactors = F)
test  <- read.csv("test.csv", header = T, stringsAsFactors = F)
store <- read.csv("store.csv", header = T, stringsAsFactors = F)

train <- merge(train,store)
test <- merge(test,store)
rm(store)

#################################################
########## Process time in T and TE #############
train$Date <- as.Date(train$Date, format="%Y-%m-%d")
test$Date <- as.Date(test$Date, format="%Y-%m-%d")

train$month <- as.integer(month(train$Date))
train$year <- as.integer(year(train$Date))
train$day <- as.integer(day(train$Date))

test$month <- as.integer(month(test$Date))
test$year <- as.integer(year(test$Date))
test$day <- as.integer(day(test$Date))

train[is.na(train)] <- 0
test[is.na(test)] <- 0
train <- train[ which(train$Sales!='0'),]
train <- train[ which(train$Open!='0'),]

# Remove State Holiday #
train <- train[, -c(which(names(train) == 'StateHoliday'))]
test <- test[, -c(which(names(test) == 'StateHoliday'))]
#################################################
#################################################



#################################################
#################################################

#################################################
########## HASH TRICK THE CHARS  ################
feature.names <- names(train)[c(1,2,6:20)]



#################################################
#################################################

#################################################
########## ADD TIME SERIES COLS TO TT ###########
all_stores <- unique(train$Store)
varx_tr <- c(2, 7, 18, 20)
varx_te <- c(3, 6, 17, 19)

### TESTING BLOCK ###
# test subsett: tt_sstt <- subet_tt(all_stores[20], train, test)
# test make_ex_mat: ex_mat <- make_ex_mat(tt_sstt$training, varx_tr)
#                   te_mat <- make_ex_mat(tt_sstt$testing, varx_te)
# test ts_make tts <- training_ts_vectors(tt_sstt$zt_mat, ex_mat, te_mat)
# test main tt_main <- make_ts_vects_main(all_stores[34], train, test, varx_tr, varx_te)
######################
run_ts_spread <- FALSE

if (run_ts_spread == TRUE){
training_list <- list()
testing_list <- list()

for (store in all_stores){   
    print('##################################################################')
    print('##################################################################')
    print('##################################################################')
    print(paste('on store', store, 'of', length(all_stores)))
    print('##################################################################')
    print('##################################################################')
    print('##################################################################')
    
    main <- make_ts_vects_main(store, train, test, varx_tr, varx_te)
    training_list[[store]] <- main$training_w_ts
    
    if (main$testing_w_ts != F){
      testing_list[[store]] <- main$testing_w_ts
    }
    
  }
}

training_enahnced <- do.call(rbind, training_list)
write.csv(training_enahnced, file='training_enhanced.csv', row.names=F)

testing_enahnced <- do.call(rbind, testing_list)
write.csv(testing_enahnced, file='testing_enhanced.csv', row.names=F)


#################################################
#################################################
feature.names <- c(feature.names, c('V1', 'V2'))
tra<-training_enahnced[,feature.names]
h<-sample(nrow(training_enahnced),10000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(training_enahnced$Sales+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(training_enahnced$Sales+1)[-h])

watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 10, #changed from default of 8
                subsample           = 0.9, # 0.7
                colsample_bytree    = 0.7 # 0.7
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3500, #300, #280, #125, #250, # changed from 300
                    verbose             = 0,
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMPSE
)
pred1 <- exp(predict(clf, data.matrix(testing_enahnced[,feature.names]))) - 1
# zero out closed
submission <- data.frame(Id=testing_enahnced$Id, Sales=pred1)
cat("saving the submission file\n")
write_csv(submission, "xgb_enh.csv")
