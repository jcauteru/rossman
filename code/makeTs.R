library(readr)
library(xgboost)
library(lubridate)
library(randomForest)

library(zoo)
library(forecast)
library(MTS)
library(RSNNS)

source('/media/hdd/kaggle/rossman/code/VARXpredJoe.R')

# ERROR FUCNTION
RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}

subet_tt <- function(store_num, training_data, testing_data){
  
  data_for_one_train <- training_data[training_data$Store == store_num, ]
  data_for_one_train <- data_for_one_train[order(data_for_one_train$Date), ]
  data_for_one_test <- testing_data[testing_data$Store == store_num, ]
  data_for_one_test <- data_for_one_test[order(data_for_one_test$Date), ]
  
  cust_ts <- zoo(x=data_for_one_train$Customers)
  sales_ts <- zoo(x=data_for_one_train$Sales)
  
  zt_mat <- matrix(nrow = length(cust_ts), ncol = 2)
  zt_mat[, 1] <- cust_ts
  zt_mat[, 2] <- sales_ts
  
  fd = F
  if(nrow(data_for_one_test) == 0){
    data_for_one_test <- data_for_one_train[1:200, ]
    fd=T
  }
  
  return(list(training = data_for_one_train, testing=data_for_one_test,
              cust_ts = cust_ts, sales_ts = sales_ts, 
              zt_mat = zt_mat, false_dat=fd))
  
}

make_ex_mat <- function(dat, vars){
  
  # Exogenoius Matrix
  exog_sub <- dat[, vars]
  exog <- data.matrix(exog_sub)
  exog[is.na(exog)] <- 0
  
  return (exog)
  
}

rnn_vectors <- function(main, target, pred_main){

  targets <- as.matrix(target/max(target))
  input <- as.matrix(main/max(main))
  
  model <- elman(input, targets, 
                 size = c(8, 8), learnFuncParams = c(0.1), maxit = 500, 
                 linOut = FALSE)
  
  fitted <- model$fitted.values*max(target)
  preds <- predict(model, as.matrix(pred_main/max(pred_main)))*max(target)
  
  return(list(training=fitted, testing=preds))
  
}


training_ts_vectors <- function(zt, exog, exog_test, ts_names=c('cust_ts', 'sales_ts')){
  order_estimation <- VARXorder(zt, exog)
  p <- order_estimation$hqor[1]
  varx_run <- VARX(zt, p, exog)
  
  # fitted VARX:
  preds_train <- VARXpred_joe(varx_run, exog, hstep = nrow(exog))
  preds_test <- VARXpred_joe(varx_run, exog_test, hstep = nrow(exog_test))
  
  # fitten RNN:
  first <- rnn_vectors(preds_train[, 1], zt[, 1], preds_test[, 1])
  second <- rnn_vectors(preds_train[, 2], zt[, 2], preds_test[, 2])
  
  # contrsut matrix
  training_rnn <- as.data.frame(preds_train, names = ts_names)
  training_rnn[, paste(ts_names[1], '_rnn', sep = '')] <- first$training
  training_rnn[, paste(ts_names[2], '_rnn', sep = '')] <- second$training
  
  testing_rnn <- as.data.frame(preds_test, names = ts_names)
  testing_rnn[, paste(ts_names[1], '_rnn', sep = '')] <- first$testing
  testing_rnn[, paste(ts_names[2], '_rnn', sep = '')] <- second$testing
  
  
  return(list(train=training_rnn, test=testing_rnn))                 
  
}

make_ts_vects_main <- function(st_num, training, testing, tr_var, te_var){
  data_result <- subet_tt(st_num, training, testing)
  
  training_exog <- make_ex_mat(data_result$training, tr_var)
  testing_exog <- make_ex_mat(data_result$testing, te_var)
  
  result <- training_ts_vectors(data_result$zt_mat, training_exog, 
                      testing_exog)

  st_train <- cbind(data_result$training, result$train)
  st_test <- cbind(data_result$testing, result$test)
  
  if (data_result$false_dat == T){
    st_test = F
  }
  
  return(list(training_w_ts=st_train, testing_w_ts=st_test))
}


