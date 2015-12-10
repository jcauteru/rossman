VARXpred_joe <- function (m1, newxt = NULL, hstep = 1, orig = 0) 
{
  zt = as.matrix(m1$data)
  xt = as.matrix(m1$xt)
  p = m1$aror
  m = m1$m
  Ph0 = as.matrix(m1$Ph0)
  Phi = as.matrix(m1$Phi)
  Sig = as.matrix(m1$Sigma)
  beta = as.matrix(m1$beta)
  include.m = m1$include.mean
  nT = dim(zt)[1]
  k = dim(zt)[2]
  dx = dim(xt)[2]
  se = NULL
  if (length(Ph0) < 1) 
    Ph0 = matrix(rep(0, k), k, 1)
  if (hstep < 1) 
    hstep = 1
  if (orig < 1) 
    orig = nT
  if (length(newxt) > 0) {
    if (!is.matrix(newxt)) 
      newxt = as.matrix(newxt)
    h1 = dim(newxt)[1]
    hstep = min(h1, hstep)
    nzt = as.matrix(zt[1:orig, ])
    xt = rbind(xt[1:orig, , drop = FALSE], newxt)
    for (i in 1:hstep) {
      tmp = Ph0
      ti = orig + i
      for (i in 1:p) {
        idx = (i - 1) * k
        tmp = tmp + Phi[, (idx + 1):(idx + k)] %*% matrix(nzt[ti - 
                                                                i, ], k, 1)
      }
      if (m > -1) {
        for (j in 0:m) {
          jdx = j * dx
          tmp = tmp + beta[, (jdx + 1):(jdx + dx)] %*% 
            matrix(xt[ti - j, ], dx, 1)
        }
      }
      nzt = rbind(nzt, c(tmp))
    }
    mm = VARpsi(Phi, lag = hstep)
    Si = Sig
    se = matrix(sqrt(diag(Si)), 1, k)
    if (hstep > 1) {
      for (i in 2:hstep) {
        idx = (i - 1) * k
        wk = as.matrix(mm$psi[, (idx + 1):(idx + k)])
        Si = Si + wk %*% Sig %*% t(wk)
        se1 = sqrt(diag(Si))
        se = rbind(se, se1)
      }
    }
    cat("Prediction at origin: ", orig, "\n")
    cat("Point forecasts (starting with step 1): ", "\n")
    #print(round(nzt[(orig + 1):(orig + hstep), ], 5))
    #cat("Corresponding standard errors: ", "\n")
    #print(round(se[1:hstep, ], 5))
    forecasts <- round(nzt[(orig + 1):(orig + hstep), ], 5)
    return(forecasts)
  }
  else {
    cat("Need new data for input variables!", "\n")
  }
}