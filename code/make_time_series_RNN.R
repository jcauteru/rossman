library(readr)
library(xgboost)
library(lubridate)
library(randomForest)

library(zoo)
library(forecast)
library(autoencoder)
library(RSNNS)

# RSNNS: https://cran.r-project.org/web/packages/RSNNS/RSNNS.pdf

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

data_for_one <- train[train$Store == not_full[30], ]
cust_ts <- zoo(x=data_for_one$Customers, order.by=full_date_range)

targets <- data_for_one$Customers/max(data_for_one$Customers)
avg = 3
targets_adj <- targets[avg:length(targets)]
ma10 <- matrix(1, length(targets_adj))
ma10 <- rollmeanr(targets, avg)
#ma10[, 2] <- data_for_one$DayOfWeek/7
#ma10[, 3] <- data_for_one$month/12
patterns <- splitForTrainingAndTest(ma10, targets_adj, ratio = 0.15)
model <- elman(patterns$inputsTrain, patterns$targetsTrain, 
               size = c(8, 8), learnFuncParams = c(0.1), maxit = 500, 
               inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest, 
               linOut = FALSE)
plot(targets_adj, type = 'l') 
lines(model$fitted.values, col = "green")

ttmat <- matrix(nrow=length(patterns$inputsTest), ncol = 1)
ttmat[, 1] <- patterns$inputsTest
preds <- predict(model, ttmat)
ind <- length(model$fitted.values) + seq(1, (length(model$targets_adj) - 
                                   length(model$fitted.values)))
lines(x=ind, y=preds, col = "blue")
