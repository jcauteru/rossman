library(readr)
library(xgboost)
library(lubridate)
library(randomForest)
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

train[is.na(train)] <- 0
test[is.na(test)] <- 0
train <- train[ which(train$Sales!='0'),]
#train <- train[ which(train$Open!='0'),]
# Remove State Holiday #
#train <- train[, -c(which(names(train) == 'StateHoliday'))]
#test <- test[, -c(which(names(test) == 'StateHoliday'))]

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

#################################################
#################################################

# Monthly AVG cust
SC_MNTH_MEAN <- aggregate(Customers~Store+month, data=train, FUN=mean)
SC_TOT_MEAN <- aggregate(Customers~Store, data=train, FUN=mean)
SC_TOT_SUM <- aggregate(Customers~Store, data=train, FUN=sum)
SC_DOW_MEAN <- aggregate(Customers~Store+DayOfWeek, data=train, FUN=mean)

# DemiDec of above
demi_dec_range <- seq(0, 1, .05)
total_cust_dd <- quantile(SC_TOT_SUM$Customers, probs = demi_dec_range, na.rm = FALSE, names = TRUE, type = 7)
mean_daily_cust_dd <- quantile(SC_DOW_MEAN$Customers, probs = demi_dec_range, na.rm = FALSE, names = TRUE, type = 7)
mean_over_cust_dd <- quantile(SC_TOT_MEAN$Customers, probs = demi_dec_range, na.rm = FALSE, names = TRUE, type = 7)

SC_TOT_SUM$total_cust_dd <- as.integer(cut(SC_TOT_SUM$Customers, total_cust_dd))
SC_TOT_SUM$Customers <- log(SC_TOT_SUM$Customers)
names(SC_TOT_SUM) <- c('Store', 'LG_CUST', 'CUST_DD')

SC_DOW_MEAN$total_cust_dd <- as.integer(cut(SC_DOW_MEAN$Customers, mean_daily_cust_dd))
names(SC_DOW_MEAN) <- c('Store', 'DayOfWeek', 'DOW_M_CUST', 'DOW_M_CUST_DD')

SC_TOT_MEAN$total_cust_dd <- as.integer(cut(SC_TOT_MEAN$Customers, mean_over_cust_dd))
names(SC_TOT_MEAN) <- c('Store', 'M_CUST', 'M_CUST_DD')

# Monthly AVG Sales
SS_DOW_MEAN <- aggregate(Sales~Store+DayOfWeek, data=train, FUN=mean)
SS_DOW_MIN <- aggregate(Sales~Store+DayOfWeek, data=train, FUN=min)
SS_DOW_MAX <- aggregate(Sales~Store+DayOfWeek, data=train, FUN=max)
SS_DOW_VAR <- aggregate(Sales~Store+DayOfWeek, data=train, FUN=sd)
SS_DOW_TOT <- aggregate(Sales~Store, data=train, FUN=sum)

total_sales_dd <- quantile(SS_DOW_TOT$Sales, probs = demi_dec_range, na.rm = FALSE, names = TRUE, type = 7)
mean_daily_sales_dd <- quantile(SS_DOW_MEAN$Sales, probs = demi_dec_range, na.rm = FALSE, names = TRUE, type = 7)

SS_DOW_TOT$total_cust_dd <- as.integer(cut(SS_DOW_TOT$Sales, total_sales_dd))
SS_DOW_TOT$Sales <- log(SS_DOW_TOT$Sales)
names(SS_DOW_TOT) <- c('Store', 'LG_TOT_SALES', 'DOW_TT_SALE_DD')

SS_DOW_MEAN$total_cust_dd <- as.integer(cut(SS_DOW_MEAN$Sales, mean_daily_sales_dd))
names(SS_DOW_MEAN) <- c('Store', 'DayOfWeek', 'M_SALES', 'M_SALES_DD')

# Merge All Info
TBM <- list(SC_MNTH_MEAN, SC_TOT_MEAN, SC_TOT_SUM, SC_DOW_MEAN, 
            SS_DOW_MEAN, SS_DOW_MIN, SS_DOW_MAX, SS_DOW_VAR, SS_DOW_TOT)
t_names <- c()
for (t in TBM){
  t_names <- c(t_names, names(t))
  train <- merge(train, t, all.x = T)
  test <- merge(test, t, all.x = T)
}
t_names <- unique(t_names)

## ADD Holiday Controls
train$is_weekend <- train$DayOfWeek %in% c(6, 7)
test$is_weekend <- test$DayOfWeek %in% c(6, 7)

## ADD Outlier Controls

## ADD Store B Controls
train$is_stb <- train$StoreType == 'b'
test$is_stb <- test$StoreType == 'b'

# Zero Fill NA
train[is.na(train)] <- 0
test[is.na(test)] <- 0
#################################################
########## HASH TRICK THE CHARS  ################

feature.names <- names(train)[c(1,2,5,7:33)]

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


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
  
  training_enahnced <- do.call(rbind, training_list)
  write.csv(training_enahnced, file='training_enhanced.csv', row.names=F)
  
  testing_enahnced <- do.call(rbind, testing_list)
  write.csv(testing_enahnced, file='testing_enhanced.csv', row.names=F)
  
  feature.names <- c(feature.names, c('V1', 'V2'))
}

if (run_ts_spread == FALSE){
  training_enahnced <- train
  testing_enahnced <- test
}




#################################################
#################################################

training_enahnced <- training_enahnced[order(train$Date),]
#h <- training_enahnced[train$Date >= as.Date('2015-07-31'), ]

tra<-training_enahnced[,feature.names]
#h<-sample(nrow(training_enahnced),10000)
h <- which(train$Date > as.Date('2015-06-01'))

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(training_enahnced$Sales+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(training_enahnced$Sales+1)[-h])

watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 10, #changed from default of 8
                subsample           = 0.9, # 0.7
                colsample_bytree    = 0.7 # 0.7
                num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000, #300, #280, #125, #250, # changed from 300
                    verbose             = 0,
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMPSE
)
pred1 <- exp(predict(clf, data.matrix(testing_enahnced[,feature.names]))) - 1
#pred1[testing_enahnced$Open == 0 & testing_enahnced$DayOfWeek == 7] <- 0
submission <- data.frame(Id=testing_enahnced$Id, Sales=pred1)
cat("saving the submission file\n")
write_csv(submission, "xgb_feat_eng.csv")

## Getting what wrong:
SGOBS_ER <- function(preds, act){
  res <- ((act - preds)/act)^2
}
raw_df <- training_enahnced[h ,feature.names]
raw_df$Sales_PRED
raw_df$Sales_PRED <- exp(predict(clf, data.matrix(raw_df))) - 1
raw_df$ERRORS <- SGOBS_ER(raw_df$Sales_PRED, raw_df$Sales)

# RF TRY:
myNtree = 500
myMtry = 5
myImportance = TRUE

casualFit <- randomForest(Sales~. , data=training_enahnced[h ,c('Sales', feature.names)], 
                          ntree=myNtree, mtry=myMtry, importance=myImportance)


