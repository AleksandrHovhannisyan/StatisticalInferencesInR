#' Construct a 100(1-alpha) percent confidence interval for a single population mean
#'
#' @param xbar The mean of the sample
#' @param variance Either the sample or population variance (the latter only if it is known)
#' @param n The size of the sample
#' @param popVarKnown Boolean denoting whether the population variance is known. False by default.
#' @param alpha The confidence level. Set to 0.05 by default.
#' @param one.sided Boolean denoting whether the function should return one-sided confidence intervals. False by default.
#' @return The confidence interval(s): one-sided lower and upper bounds or just the two-sided confidence interval.
#' @export
Mean.1Pop.CI <- function(xbar, variance, n, popVarKnown=F, alpha=0.05, one.sided=F){

  confidence = cat(100*(1-alpha),"%")

  interval = NULL

  if(one.sided){ alpha = 2 * alpha }

  testStat = 0

  if(popVarKnown){ testStat = abs(qnorm(alpha/2, lower.tail=F)) }
  else{ testStat = abs(qt(alpha/2, df = n - 1, lower.tail = F)) }

  marginOfError = testStat * sqrt(variance / n)

  if(one.sided){
    upper=c(0, xbar + marginOfError)
    lower=c(xbar - marginOfError, Inf)
    interval = list(lowerBound=lower, upperBound=upper)
    cat(confidence, "One-Sided Confidence Intervals:\n")
    interval
    return(interval)
  }
  else{
    interval=c(xbar - marginOfError, xbar + marginOfError)
    cat(confidence, "Confidence Interval:\n")
    interval
    return(interval)
  }
}
