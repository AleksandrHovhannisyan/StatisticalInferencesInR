#' One Population Mean - Confidence Interval
#' @description Constructs a 100(1-alpha) percent confidence interval for a single population mean
#' @param xbar the mean of the sample
#' @param variance either the sample or population variance (the latter only if it is known)
#' @param n the size of the sample
#' @param popVarKnown boolean denoting whether the population variance is known. False by default.
#' @param alpha the significance level. Set to 0.05 by default.
#' @param one.sided boolean denoting whether the function should return one-sided confidence intervals. False by default.
#' @return The confidence interval(s): one-sided lower and upper bounds (in a list) or just the two-sided confidence interval (as a simple vector).
#' @export
Mean.1Pop.CI <- function(xbar, variance, n, popVarKnown=F, alpha=0.05, one.sided=F){

  if(one.sided){ alpha = 2 * alpha }

  testStat = 0

  if(popVarKnown){ testStat = abs(qnorm(alpha/2, lower.tail=F)) }
  else{ testStat = abs(qt(alpha/2, df = n - 1, lower.tail = F)) }

  marginOfError = testStat * sqrt(variance / n)

  IntervalOutput(xbar - marginOfError, xbar + marginOfError, GetConfidenceLevel(alpha), "Confidence", one.sided)
}
