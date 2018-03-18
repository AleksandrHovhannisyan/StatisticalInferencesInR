#' Construct a 100(1-alpha) percent confidence interval for the difference between two population means
#'@param xbar1 The sample mean of population 1.
#'@param xbar2 The sample mean of population 2.
#'@param n1 The size of the sample from population 1.
#'@param n2 The size of the sample from population 2.
#'@param var1 The sample or population variance of population 1 (the latter only if it is known).
#'@param var2 The sample or population variance of population 2 (the latter only if it is known).
#'@param alpha The significance level. Set to 0.05 by default.
#'@param popVarKnown Boolean denoting whether the population variance is known. False by default.
#'@param equal Boolean denoting whether the population variances are equal. False by default. Used only in the case when the population variances are unknown.
#'@param one.sided Boolean denoting whether the function should return one-sided confidence intervals. False by default.
#'@return The confidence interval(s): one-sided lower and upper bounds (in a list) or just the two-sided confidence interval (as a simple vector).
#'@export
Mean.2Pop.CI <- function(xbar1, xbar2, n1, n2, var1, var2, alpha=0.05, popVarKnown=F, equal=F, one.sided=F){

  confidence = cat("\n", 100*(1-alpha),"%", sep="")

  xbar = xbar1 - xbar2
  testStat = 0
  marginOfError = 0
  interval = NULL

  if(one.sided){ alpha = 2 * alpha }

  if(popVarKnown){
    testStat = abs(qnorm(alpha/2, lower.tail=F))
    marginOfError = testStat * sqrt(var1/n1 + var2/n2)
  }
  else{

    degreesOfFreedom = 0

    if(equal){
      sp = sqrt(((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2))
      degreesOfFreedom = n1 + n2 - 2
      testStat = abs(qt(alpha/2, df=degreesOfFreedom, lower.tail=F))
      marginOfError = testStat*sp*sqrt((1/n1)+(1/n2))
    }
    else{
      freedomNumerator = (var1/n1 + var2/n2)^2
      freedomDenominator = ( ((var1/n1)^2/(n1-1)) + ((var2/n2)^2/(n2-1)) )
      degreesOfFreedom = freedomNumerator / freedomDenominator
      testStat = abs(qt(alpha/2, df=degreesOfFreedom, lower.tail=F))
      marginOfError = testStat*sqrt((var1/n1 + var2/n2))
    }
  }

  IntervalOutput(xbar, marginOfError, confidence, "Confidence", one.sided)
}
