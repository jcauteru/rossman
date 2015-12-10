library(caret)
library(xgboost)
set.seed(3257)


CREATE_GRID <- function(range_cap, mode='TREE', zero_1_step=.01, inf_step=1, restrict=5){
  
  if (mode == 'TREE'){
    rounds <- seq(range_cap[['rounds_min']], 
                  range_cap[['rounds_max']], 
                  range_cap[['rounds_step']])
    
    eta_step = (range_cap[['eta_min']] - range_cap[['eta_max']])/length(rounds)
    eta <- seq(range_cap[['eta_max']], range_cap[['eta_min']], eta_step)
    eta <- eta[order(eta, decreasing = T)][length(rounds)-length(eta):length(eta)]
    
    eta_round <- data.frame(eta=eta, rounds=rounds)
    
    depth_round <- seq(3, 1*range_cap[['max_depth']], inf_step)
    
    subsample <- seq(.6, 1*range_cap[['subsample']], .1)
    colsample_bytree <- seq(.6, 1*range_cap[['colsample_bytree']], .1)

    sample_round <- expand.grid(list(subsample=subsample, 
                                    colsample_bytree=colsample_bytree))
    
    

    return(list(eta_round=eta_round, depth_round=depth_round, 
                sample_round=sample_round[sample(1:nrow(sample_round), restrict),]))
  }
  
  
}

GB_GRID_TRAIN <- function(params, test_data, train_data, target_index){
  
  param_in <- params$param
  rounds <- params$rounds
  
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

SEARCH_GRID_ETA <- function(search, test_data, train_data, target_index){
  scores <- c()
  mods <- list()
  
  for (r in 1:nrow(search)){
    eta <- search[r, c('eta')]
    rounds <- search[r, c('rounds')]
    print(eta)
    param <- list(objective="reg:linear", booster = "gbtree",
                  eta = eta, max_depth = 10,
                  subsample = 0.7, colsample_bytree = 0.7
    )

    result <- GB_GRID_TRAIN(list(param=param, rounds=rounds), 
                            test_data, train_data, target_index)
    
    pred1 <- exp(predict(result, data.matrix(holdout[,feature.names])))
    error <- RMPSE2(pred1, holdout[,c('Sales')])
    
    scores <- c(error$value, result$bestScore)
    print(paste(error$value, 'for eta:', eta, rounds))
    mods[[as.character(error$value)]] <- result
  }
  
  return(mods[[min(scores)]])
  
}


range_cap <- list(eta_min=.001, eta_max=.3, 
                  max_depth=8, subsample=.9, colsample_bytree=.9,
                  alpha=.01, lambda=.01, rounds_max=5000, 
                  rounds_min=100, rounds_step=500)

#search <- CREATE_GRID(range_cap)