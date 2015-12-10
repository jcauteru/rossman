
library(readr)
library(xgboost)
library(lubridate)
source('/media/hdd/kaggle/rossman/code/makeTs.R')
setwd('/media/hdd/kaggle/rossman/data')

preds1  <- read.csv("xgb_feat_eng.csv", header = T, stringsAsFactors = F)
names(preds1) <- c('Id', 'P1_Sales')
preds2 <- read.csv("rf1.csv", header = T, stringsAsFactors = F)
names(preds2) <- c('Id', 'P2_Sales')
preds3 <- read.csv("h2o_rf.csv", header = T, stringsAsFactors = F)
names(preds3) <- c('Id', 'P3_Sales')

all_preds <- merge(merge(preds1, preds2, by='Id'), preds3, by='Id')

head(all_preds)
allp_diff <- abs(all_preds$P2_Sales - all_preds$P1_Sales)
rf_diff <- abs(all_preds$P2_Sales - all_preds$P3_Sales)

weighted <- (all_preds$P1_Sales + all_preds$P2_Sales + all_preds$P3_Sales)/3
weighted[rf_diff > 400] <- all_preds$P2_Sales[rf_diff > 400]

submission <- data.frame(Id=preds1$Id, Sales=weighted)
write_csv(submission, "weighted_combo3.csv")
