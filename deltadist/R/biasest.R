#' Report bias in estimating the mean of a delta distribution
#' Bias is estimated through repated sampling and estimation
#' @param n sample size for each iteration
#' @param p parameter to the delta distribution: the probability that X!=0
#' @param lmean parameter to the delta distribution: the mean of ln(x) : X!=0
#' @param lvar parameter to the delta distribution: the variance of ln(x) : X!=0
#' @param iter the number of iterations to run for bias estimation
#' @param estimator the estimator to test.
deltabias <- function(n, p, lmean, lvar, iter=1000, estimator_mean=delta_mean){

  lognormalmean <- exp(lmean+lvar/2)
  lognormalvar <- (exp(lvar) - 1) * exp(2*lmean + lvar)

  alpha <- p*lognormalmean
  beta <- p*(1-p)*(lognormalmean**2) + p*lognormalvar

  errors <- c()

  for (i in 1:iter){
    sample <- rdeltanorm(n,p,lmean,lvar)
    est_mean <- delta_mean(sample)
    error <- est_mean - alpha
    errors <- c(errors, error)
  }

  output <- list()
  output$bias <- mean(errors)
  output$relbias <- mean(errors) / alpha
  output$mse <- mean(errors*errors)

  return(output)
}

test_estimators <- function(n=100, p=.1, lmean=0, lvar=1, iter=10000){
  print("Testing delta_mean")
  print(deltabias(n,p,lmean,lvar,iter, delta_mean))

  fishmean <- function(x){fishmethods::deltadist(x)[1]}

  print("Testing fishmethods::deltadist")
  print(deltabias(n,p,lmean,lvar,iter, fishmean))
}
