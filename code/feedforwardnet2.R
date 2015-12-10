# http://cs229.stanford.edu/proj2012/BussetiOsbandWong-DeepLearningForTimeSeriesModeling.pdf
# feedforward networks were considered at this stage.  Unless otherwise stated, the networks were 
# trained with 70% of the data, 15% was used for validation stopping, and 15% for
#testing (chosen at random).  This was repeated 10 times per experiment to average out sample error

# Experiment with different sets of features

library(h2o)
rossmandir <- "~/Google Drive/Kaggle/Rossman/"
setwd(rossmandir)
store <- read.csv(paste0(rossmandir, "store.csv"))
test <- read.csv(paste0(rossmandir, "test.csv"))
train <- read.csv(paste0(rossmandir, "train.csv"))

# To rbind test and train:  
# 1) add  Id [41091+] to train
train$Id <- 41091:(41090+length(train$Store))
# 2) add sales to test (Predict this), 
test$Sales <- NA
# 3) add Customers to test (Predict this?)
test$Customers <- NA

#Training model by merging store details on training set
testdata <- merge(test,store, by.x="Store", by.y="Store")
data <- merge(train,store, by.x="Store", by.y="Store")
data <- rbind(data,testdata)

for (i in c(1,2,6,7,8,9,10,11,12,16,19)){
  data[,i] <- as.factor(data[,i])
}

# Convert Date to datetime type # YYYY-MM-DD
data$Date <- strptime(data$Date, format = "%Y-%m-%d")
# Get rid of closed days, 
data <- data[data$Open != 0,]
# Get rid of data with no store id
data <- data[-which(is.na(data$Store)),]

# Convert competition open since to datetime, then convert to binary (CompetitionOpen?) based on date
data$CompetitionOpenSinceMonth2 <- NA
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 1)] <- "01"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 2)] <- "02"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 3)] <- "03"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 4)] <- "04"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 5)] <- "05"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 6)] <- "06"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 7)] <- "07"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 8)] <- "08"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 9)] <- "09"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 10)] <- "10"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 11)] <- "11"
data$CompetitionOpenSinceMonth2[which(data$CompetitionOpenSinceMonth == 12)] <- "12"

data$CompetitionOpenSince <- strptime(paste0(data$CompetitionOpenSinceYear, 
                                             "-", data$CompetitionOpenSinceMonth2,
                                             "-","01"),format="%Y-%m-%d")

#Convert Promo2SinceWeek and SinceYear to date
data$Promo2SinceWeek <- as.character(data$Promo2SinceWeek)
data$Promo2SinceWeek[which(data$Promo2SinceWeek == 1)] <- "01"
data$Promo2SinceWeek[which(data$Promo2SinceWeek == 5)] <- "05"
data$Promo2SinceWeek[which(data$Promo2SinceWeek == 6)] <- "06"
data$Promo2SinceWeek[which(data$Promo2SinceWeek == 9)] <- "09"

# 67659 Year Only, Convert NA week to week 01
data$Promo2SinceWeek[which(is.na(data$Promo2SinceWeek))] <- "01"
data$Promo2Since <- strptime(paste0(data$Promo2SinceWeek, 
                                    "-", data$Promo2SinceYear),format="%U-%Y")

##If current date >= Promo2Active = 1, else 0
data$Promo2Active <- data[,"Date"]-data[,"Promo2Since"] >= 0
data$Promo2Active[which(data$Promo2Active == TRUE)] <- 1
data$Promo2Active[which(data$Promo2Active == FALSE)] <- 0
data$Promo2Active[which(is.na(data$Promo2Active))] <- 0
data[which(data$Promo2Active==1),"Promo2ActiveDays"] <- data[which(data$Promo2Active==1),"Date"]-data[which(data$Promo2Active==1),"Promo2Since"]
data[which(data$Promo2Active==1),"Promo2ActiveDays"] <- data[which(data$Promo2Active==1),"Promo2ActiveDays"]/10000
# If Promo2 never active, make = -100
data[which(is.na(data$Promo2Active==0)),"Promo2ActiveDays"] <- -100

data$month <- as.integer(format(data$Date, "%m"))
data$year <- as.integer(format(data$Date, "%y"))
data$day <- as.integer(format(data$Date, "%d"))


# CompetitionOpen?
##If current date >= CompActive = 1, else 0
data$CompActive <- data[,"Date"]-data[,"CompetitionOpenSince"] >= 0
data$CompActive[which(data$CompActive == TRUE)] <- 1
data$CompActive[which(data$CompActive == FALSE)] <- 0
data$CompActive[which(is.na(data$CompActive))] <- 0
data[which(data$CompActive==1),"CompActiveDays"] <- data[which(data$CompActive==1),"Date"]-data[which(data$CompActive==1),"CompetitionOpenSince"]
data[which(data$CompActive==1),"CompActiveDays"] <- data[which(data$CompActive==1),"CompActiveDays"]/10000


#NA handling, sales and customers should have 35093 NA's, for test, 
data[which(is.na(data[,"CompetitionDistance"])),"CompetitionDistance"] <- 5445.554 #Mean
data[which(is.na(data[,"CompetitionOpenSince"])),
     "CompetitionOpenSince"] <- "2010-02-01 EST" #median(data[-which(is.na(data$CompetitionOpenSince)),"CompetitionOpenSince"])

data[which(is.na(data[,"CompActiveDays"])),
     "CompActiveDays"] <- median(data[-which(is.na(data$CompActiveDays)),"CompActiveDays"])
data[which(is.na(data[,"Promo2Since"])),
     "Promo2Since"] <- "2012-10-04 EDT" #median(data[-which(is.na(data$Promo2Since)),"Promo2Since"])
data[which(is.na(data[,"Promo2ActiveDays"])),
     "Promo2ActiveDays"] <- median(data[-which(is.na(data$Promo2ActiveDays)),"Promo2ActiveDays"])

# If Comp never active, make = -100
data[which(is.na(data$CompActive==0)),"CompActiveDays"] <- -100
# If Promo2 never active, make = -100
data[which(is.na(data$Promo2Active==0)),"Promo2ActiveDays"] <- -100

#Dummy for whether promo2 refreshed that month?
data$Id <- as.numeric(data$Id)
#"Customers" not in test so ignored for now
#Dates as numeric : Date CompetitionOpenSince, Promo2Since
data$Date2 <- as.numeric(data$Date)
data$CompetitionOpenSince2 <- as.numeric(data$CompetitionOpenSince)
data$Promo2Since2 <- as.numeric(data$Promo2Since)


# Got rid of "CompetitionOpenSince2","Promo2Since2", "Promo2ActiveDays",
# Train: ID >41090, split on date, val Date > 06-15-2015train <= 06-152015
trainingset <- data[data$Id > 41090 & data$Date <= strptime("06-15-2015", format="%m-%d-%Y"),
                    c("Id", "Store", "DayOfWeek", "Date2", "Open","Promo","StateHoliday",
                      "SchoolHoliday","StoreType","Assortment","CompetitionDistance","PromoInterval",
                      "Promo2Active","month","year","day","CompActive","CompActiveDays", "Sales")] 

valset <- data[data$Id > 41090 & data$Date > strptime("06-15-2015", format="%m-%d-%Y"),
               c("Id", "Store", "DayOfWeek", "Date2", "Open","Promo","StateHoliday",
                 "SchoolHoliday","StoreType","Assortment","CompetitionDistance","PromoInterval",
                 "Promo2Active","month","year","day","CompActive","CompActiveDays", "Sales")] 

testset <- data[data$Id <= 41090,
                c("Id", "Store", "DayOfWeek", "Date2", "Open","Promo","StateHoliday",
                  "SchoolHoliday","StoreType","Assortment","CompetitionDistance","PromoInterval",
                  "Promo2Active","month","year","day","CompActive","CompActiveDays", "Sales")] 

factorcols <- c("Store", "DayOfWeek", "Date2", "Open","Promo","StateHoliday",
                "SchoolHoliday","StoreType","Assortment","CompetitionDistance","PromoInterval",
                "Promo2Active","month","year","day","CompActive","CompActiveDays")
contcols <- c("Date2","CompetitionDistance", "CompetitionOpenSince2","Promo2Since2", "Promo2ActiveDays","CompActiveDays","Sales")


for (col in factorcols){
  trainingset[,col] <- as.factor(trainingset[,col])
  testset[,col] <- as.factor(testset[,col])
  valset[,col] <- as.factor(valset[,col])
}

train_pro <-  trainingset[trainingset$Promo2Active == 1, ] 
train_nopro <- trainingset[trainingset$Promo2Active == 0, ]
test_pro <- testset[testset$Promo2Active == 1, ]
test_nopro <- testset[testset$Promo2Active == 0, ]
val_pro <- valset[valset$Promo2Active == 1, ]
val_nopro <- valset[valset$Promo2Active == 0, ]


# Periodic Variables!! Transform from x to -> [y1,y2] = [sin(2pi*x / w),cos(2pi*x / w)]
# DayOfWeek w=0/6, , DayofMonth = 0/30.42, Month= 0/11, 
# get rid of compopensince and promosince and only keepy promoactive and comp active
# Weights = 50, 30, 10
# Features to add: store mean for type of day: 
h2oServer <- h2o.init(startH2O = TRUE, max_mem_size = '5g', ice_root="./ICE")

train_hexpromo <- as.h2o(h2oServer, train_pro) 
train_hexnopromo <- as.h2o(h2oServer, train_nopro)
test_hexpromo <- as.h2o(h2oServer, test_pro)
test_hexnopromo <- as.h2o(h2oServer, test_nopro)
val_hexpromo <- as.h2o(h2oServer, val_pro)
val_hexnopromo <- as.h2o(h2oServer, val_nopro)

#Train vars not 17, 22, 1, or 20 - 2:16,18,19,21, NO INTERACTIONS
x_vars <- names(train_hexpromo)[c(2:18)] #1 is id, 22 is sales
sales <- 19
id <- 1

# 1:2 "Id", "Store"
factor_interactions1 <- h2o.interaction(train_hexpromo, factors = c(factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)
factor_interactions2 <- h2o.interaction(train_hexnopromo, factors = c(factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)
factor_interactions3 <- h2o.interaction(test_hexpromo, factors = c(factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)
factor_interactions4 <- h2o.interaction(test_hexnopromo, factors = c(factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)
factor_interactions5 <- h2o.interaction(val_hexpromo, factors = c(factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)
factor_interactions6 <- h2o.interaction(val_hexnopromo, factors = c(factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)

train_hexpromo <- h2o.cbind(train_hexpromo, factor_interactions1)
train_hexnopromo <- h2o.cbind(train_hexnopromo, factor_interactions2)
test_hexpromo <- h2o.cbind(test_hexpromo, factor_interactions3)
test_hexnopromo <- h2o.cbind(test_hexnopromo, factor_interactions4)
val_hexpromo <- h2o.cbind(val_hexpromo, factor_interactions5)
val_hexnopromo <- h2o.cbind(val_hexnopromo, factor_interactions6)

#Train vars not 17, 22, 1, or 20 - 2:16,18,19,21,
x_vars <- names(train_hexpromo)[c(2:18,23:h2o.ncol(train_hexpromo))] #1 is id, 22 is sales
sales <- 19
id <- 1
# http://h2o.ai/docs/master/model/deep-learning/


args(h2o.deeplearning)
promomodel <- h2o.deeplearning(x=x_vars, y="Sales",train_hexpromo, validation_frame=val_hexpromo,
                               hidden=c(50,30,10), epochs=10, activation="Tanh",loss="MeanSquare", nfolds = 10)
nopromomodel <- h2o.deeplearning(x=x_vars, y="Sales",train_hexnopromo, validation_frame=val_hexnopromo,
                               hidden=c(50,30,10), epochs=10, activation="Tanh",nfolds = 2)

promoscores <- cbind(test_pro$Id,as.data.frame(h2o.predict(promomodel, test_hexpromo)))
nopromoscores <- cbind(test_nopro$Id, as.data.frame(h2o.predict(nopromomodel, test_hexnopromo)))
names(promoscores) <- c("Id","Sales")
names(nopromoscores) <- c("Id","Sales")
scores <- as.data.frame(rbind(promoscores, nopromoscores))
submit <- as.data.frame(test[test$Open==0,"Id"])
submit$Sales <- 0
names(scores) <- c("Id","Sales")
names(submit) <- c("Id","Sales")
submit <- rbind(scores, submit)

write.csv(as.data.frame(submit),"deepnet1.csv")



#http://learn.h2o.ai/content/hands-on_training/deep_learning.html

#train_hexpromo <- h2o.uploadFile(h2oServer, path = "train_pro.csv", header = TRUE, sep = ",")
#train_hexnopromo <- h2o.uploadFile(h2oServer, path = "train_nopro", header = TRUE, sep = ",")
#test_hexpromo <- h2o.uploadFile(h2oServer, path = "test_pro", header = TRUE, sep = ",")
#test_hexnopromo <- h2o.uploadFile(h2oServer, path = "test_nopro", header = TRUE, sep = ",")
#val_hexpromo <- h2o.uploadFile(h2oServer, path = "val_pro", header = TRUE, sep = ",")
#val_hexnopromo <- h2o.uploadFile(h2oServer, path = "val_nopro", header = TRUE, sep = ",")

write.csv(train_pro, "train_pro.csv",row.names = FALSE)
write.csv(train_nopro, "train_nopro",row.names = FALSE)
write.csv(test_pro, "test_pro",row.names = FALSE)
write.csv(test_nopro, "test_nopro",row.names = FALSE)
write.csv(val_pro, "val_pro",row.names = FALSE)
write.csv(val_nopro, "val_nopro",row.names = FALSE)
