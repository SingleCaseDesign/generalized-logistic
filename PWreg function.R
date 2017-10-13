
#
# Analysis of single case design by method of Center et al. (1985)  "Piece-wise regression"
# 
# written by P. Verboon, April 2016


PWreg <- function(dat, nA, robust = FALSE){
  
  # Dataframe "dat" has two columns:
  #        [1]  indicates the measurements (interval) 
  #        [2]  dependent variable to be tested
  #    
  #        nA= number of measurement in phase A (pre-intervention)
  #        robust= if TRUE: a robust function is fitted instead of LS
  
  require(MASS)
  
  n <- nrow(dat)
  D <- c(rep(0,nA),rep(1,(n-nA)))                  # dummy to indicate intervention phase
  Tc <- (dat[,1])                               
  if (Tc[1] == 1) { Tc <- Tc - 1}                  # Tc is adjusted to start with 0
  X3 <- D*(Tc-nA)                                  # trend term for phase B  (see Huitema & Kean, 2000)

  
  weights <- rep(1,n)
  
  # fit piecewise model with Huber weights
  if (robust == TRUE) { 
    out <- rlm(dat[,2] ~ D + Tc + X3) 
    weights <- out$w      
  }          
  
# fit piecewise model
         
    out <- lm(dat[,2] ~ D + Tc + X3, weights = weights)
    out0 <- lm(dat[,2] ~  Tc, weights = weights )
    
  rsq0 <- summary(out0)$r.squared
  rsq1 <- summary(out)$r.squared
  ES <- (rsq1 - rsq0)/(1-rsq0)             # compute Effect Size, see Parker & Brossart (2003, p.207)
  coef4 <- coefficients(out)
  res <- c(ES,coef4)

  names(res) <- c(" ES_R2", "Intercept", "level_change", "trend_A","  trend_change")
  output <- list()
  output[[1]] <- res
  output[[2]] <- out
  return(output)
  
}   # END FUNCTION



