
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
