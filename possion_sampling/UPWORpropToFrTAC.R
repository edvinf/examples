#' Draw lottery
#' @description
#'  Decides whether to include a given catch in sample, 
#'  with probability proportional to fraction of TAC that catch represents.
#' @param catch weight of catch
#' @param TAC Total allowable catch for stock / species, in the same unit as 'catch'
#' @param N the expected number of samples for the duration of the sampling program
#' @return logical() TRUE if catch is to be included in sample, otherwise FALSE
lottery <- function(catch, TAC, N){
  prob <- N*(catch/TAC)
  return(c(TRUE, FALSE)[sample.int(2, 1, prob = c(prob, 1-prob))])
}

#' Simulate sampling
#' @description 
#'  Simulates the total number of samples selected for repeated application of the lottery program
#' @details 
#'  The provided example of catchsizes ('pilotcatches') will be used to construct catch-populations for each year, by resampling with replacement.
#'  The TAC with assume to be the total population.
#' @param n Number of replications to simulate
#' @param m Number of catches to include in each replication
#' @param e The expected number of samples for each iterations
#' @param pilotcatches examples of catch sizes, taken from the sampled catches in the pilot of the herring lottery.
example_sample_size <- function(n=100, m=1300, e=100, pilotcatches=c(24787.540, 12615.910, 11742.600, 8070.482, 8546.132, 27359.780, 10820.170, 4248.258, 8990.219, 14348.410, 16069.010, 22571.670, 23560.060, 55208.830)){
  
  actual_selections <- c()
  
  for (i in 1:n){
    sim_catches <- pilotcatches[sample.int(length(pilotcatches), size=1300, replace = T)]
    selection <- sapply(sim_catches, FUN = lottery, sum(sim_catches), e)
    actual_selections <- c(actual_selections, sum(selection))
  }
  
  return(actual_selections)
  
  
}

samples_sizes <- example_sample_size(n=100, m=1200)
min(samples_sizes)
max(samples_sizes)
hist(samples_sizes, main=paste("n:100, m:1200"))

samples_sizes <- example_sample_size(n=100, m=2000)
min(samples_sizes)
max(samples_sizes)
hist(samples_sizes, main=paste("n:100, m:2000"))

samples_sizes <- example_sample_size(n=50, m=1200)
min(samples_sizes)
max(samples_sizes)
hist(samples_sizes, main=paste("n:50, m:1200"))


samples_sizes <- example_sample_size(n=50, m=2000)
min(samples_sizes)
max(samples_sizes)
hist(samples_sizes, main=paste("n:50, m:2000"))
