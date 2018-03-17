#'
#'
Mean.1Pop.PredictionInterval <- function(xbar, variance, n, alpha=0.05, popVarKnown=F, one.sided=F){

  confidence = cat("\n", 100*(1-alpha),"%", sep="")

  if(one.sided){ alpha = 2 * alpha }

  testStat = 0
  marginOfError = 0
  interval = NULL

  if(popVarKnown){
    testStat = abs(qnorm(alpha/2, lower.tail=F))
    marginOfError = testStat * sqrt(variance) * sqrt(1 + 1/n)
  }
  else{
    testStat = abs(qt(alpha/2, df = n - 1, lower.tail=F))
    marginOfError = testStat * variance * sqrt(1 + 1/n)
  }

  if(one.sided){
    cat(confidence, "One-Sided Confidence Intervals:\n\n")
    interval = list(upper=c(0, xbar + marginOfError), lower=c(xbar - marginOfError, Inf))
  }
  else{
    cat(confidence, "Two-Sided Confidence Interval:\n\n")
    interval = c(xbar - marginOfError, xbar + marginOfError)
  }

  return(interval)
}
