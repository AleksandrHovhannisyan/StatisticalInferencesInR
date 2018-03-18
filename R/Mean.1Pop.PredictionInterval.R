#' Construct a 100(1-alpha) percent prediction interval for the future observation of a single population's mean
#' @param xbar The mean of the sample.
#' @param variance Either the sample or population variance (the latter only if it is known).
#' @param n The size of the sample.
#' @param alpha The significance level. Set to 0.05 by default.
#' @param popVarKnown Boolean denoting whether the population variance is known. False by default.
#' @param one.sided Boolean denoting whether the function should return one-sided confidence intervals. False by default.
#' @return The confidence interval(s): one-sided lower and upper bounds (in a list) or just the two-sided confidence interval (as a simple vector).
#' @export
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

  IntervalOutput(xbar, marginOfError, confidence, "Prediction", one.sided)
}
