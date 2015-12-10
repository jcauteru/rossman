library(readr)
library(xgboost)
library(lubridate)
library(randomForest)

library(zoo)
library(forecast)
library(MTS)
library(RSNNS)

source('/media/hdd/kaggle/rossman/code/VARXpredJoe.R')

setwd('/media/hdd/kaggle/rossman/data')
cat("reading the train and test data\n")
train <- read.csv("train.csv", header = T, stringsAsFactors = F)
test  <- read.csv("test.csv", header = T, stringsAsFactors = F)
store <- read.csv("store.csv", header = T, stringsAsFactors = F)

train <- merge(train,store)
test <- merge(test,store)

# Make NAs be zero, take only stores with positive sales
train[is.na(train)]   <- 0
train <- train[train$Open=='1' & train$Sales!='0',]

test[is.na(test)]   <- 0

##### Process Training and Testing 
## DATES TO NUMERIC
train$Date = as.Date(train$Date, format="%Y-%m-%d")
train$month <- as.integer(month(train$Date))
train$year <- as.integer(year(train$Date))
train$day <- as.integer(day(train$Date))

test$month <- as.integer(month(as.Date(test$Date, format="%Y-%m-%d")))
test$year <- as.integer(year(as.Date(test$Date, format="%Y-%m-%d")))
test$day <- as.integer(day(as.Date(test$Date, format="%Y-%m-%d")))

# How many stores are in training data nd how many are in test
train_on <- unique(train$Store)
test_on <- unique(test$Store)



# Daily sales trend in one year
run = F
if (run==T){
  data_2014 <- train[year(as.Date(train$Date, format = '%Y-%m-%d')) == 2014, ]
  for_dplot <- aggregate(Customers~Date, data = data_2014[train$Store==12, ], FUN = sum)
  for_dplot2 <- aggregate(Sales~Date, data = data_2014[train$Store==12, ], FUN = sum)
  par(mfrow=c(2, 1))
  plot(for_dplot$Customers, type = 'l')
  plot(for_dplot2$Sales, type = 'l')
  dev.off()
}

full_date_range <- seq(min(train$Date), max(train$Date), "day")
# Analysis of Most current date in training data
# Test on a non-filled TS store
full <- unique(train$Store[train$year == 2014 & train$month >=7])
not_full <- base::setdiff(unique(train$Store), full)

data_for_one_train <- train[train$Store == full[80], ]
data_for_one_train <- data_for_one_train[order(data_for_one_train$Date), ]
data_for_one_test <- test[test$Store == full[50], ]
data_for_one_test <- data_for_one_test[order(data_for_one_test$Date), ]

cust_ts <- zoo(x=data_for_one_train$Customers, order.by=data_for_one_train$Date)
sales_ts <- zoo(x=data_for_one_train$Sales, order.by=data_for_one_train$Date)

# Time Series Matrix
zt_mat <- matrix(nrow = length(cust_ts), ncol = 2)
zt_mat[, 1] <- cust_ts
zt_mat[, 2] <- sales_ts

# Exogenoius Matrix
ex_vars_train <- c(2, 7, 19, 21)
exog_sub <- data_for_one_train[, ex_vars_train]
exog_train <- data.matrix(exog_sub)
exog_train[is.na(exog_train)] <- 0

order_estimation <- VARXorder(zt_mat, exog_train)
p <- order_estimation$hqor[1]
lag <- order_estimation$hqor[2]

varx_run <- VARX(zt_mat, p, exog_train)

# test:
ex_vars_train <- c(3, 6, 18, 20)
exog_sub <- data_for_one_test[, ex_vars_train]
exog <- data.matrix(exog_sub)
exog[is.na(exog)] <- 0

preds <- VARXpred_joe(varx_run, exog, hstep = nrow(exog))
ind <- length(varx_run$data[, 2]) + seq(1, nrow(exog))

fitted <- VARXpred_joe(varx_run, exog_train, hstep = nrow(exog_train))

plot(varx_run$data[, 2], type = 'l')
lines(x=seq(1, nrow(exog_train)), y=fitted[, 2], col='blue')
lines(x=ind, y=preds[, 2], col='green')

targets <- varx_run$data[, 2]/max(varx_run$data[, 2])
input <- fitted[, 2]/max(fitted[, 2])

model <- elman(input, targets, 
               size = c(8, 8), learnFuncParams = c(0.1), maxit = 500, 
               linOut = FALSE)

par(mfrow=c(2, 1))
plot(targets*max(varx_run$data[, 1]), type = 'l', col='red', ylab='')
plot(targets*max(varx_run$data[, 1]), type = 'l', col='red', ylab='')
lines(model$fitted.values*max(varx_run$data[, 1]), col = "green")
lines(x=seq(1, nrow(exog_train)), y=fitted[, 1], col='blue')
lines(model$fitted.values*mean(max(fitted[, 1]), max(varx_run$data[, 2]))
                               , col = "pink")
dev.off()
