#' As defined for Eq.2 in Pennington 1983
gn <- function(n, t, terms=15){
  res <- 1 + (n-1)*t/n

  for (j in 2:(2+terms)){
    denom <- n**j
    for (a in seq(1, (2*j-3), 2)){
      denom <- denom * (n+a)
    }
    res <- res + ((n-1)**(2*j-1) / denom) * (t**j / factorial(j))
  }
  return(res)
}

#' Eq. 2 in Pennington 1983
mean_c <- function(m,n,ybar,ssq){
  if (m>1){
    return((m/n)*(exp(ybar)*gn(m,ssq/2.0)))
  }
  else if (m==1){
    return(ybar/n)
  }
  else{
    return(0)
  }
}

#' Eq. 2 in Pennington 1983
delta_mean <- function(values, tolerance = 1e-9){

  nonzeroes <- values[abs(values)>tolerance]

  #number of nonzero values
  m <- length(nonzeroes)

  #number of values
  n <- length(values)

  #mean of log nonzeroes
  ybar <- mean(log(nonzeroes))

  #variance of log nonzeroes
  ssq <- var(log(nonzeroes))

  return(mean_c(m,n,ybar,ssq))

}

#' Eq. 4 in Pennington 1983
var_est <- function(m,n,ybar,ssq){
  if (m>1){
    return( (m/n)*exp(2*ybar)*( (m/n)*gn(m,ssq/2)**2 - ((m-1)/(n-1))*gn(m, ssq*(m-2)/(m-1)  )) )
  }
  else if (m==1){
    return( (ybar/n)**2 )
  }
  else{
    return(0)
  }
}

#' Eq. 4 in Pennington 1983
var_delta_mean <- function(values, tolerance = 1e-9){

  nonzeroes <- values[abs(values)>tolerance]

  #number of nonzero values
  m <- length(nonzeroes)

  #number of values
  n <- length(values)

  #mean of log nonzeroes
  ybar <- mean(log(nonzeroes))

  #variance of log nonzeroes
  ssq <- var(log(nonzeroes))

  return(var_est(m,n,ybar,ssq))

}

#' Generates delta distributed values
#' @param n the number of values to genereate
#' @param p the frequency of nonzeroes
#' @param lmean the mean of, ln X : X != 0
#' @param lvar the variance of ln X : X != 0
#' @return vector of delta-distributed values
rdeltanorm <- function(n, p, lmean, lvar){
  normal <- rnorm(n, lmean, sqrt(lvar))
  lognormal <- exp(normal)
  delta <- lognormal
  zeromask <- !as.logical(sample.int(2,100, replace=T, prob = c(1-p, p))-1)
  delta[zeromask] <- rep(0, sum(zeromask))
  return(delta)
}

