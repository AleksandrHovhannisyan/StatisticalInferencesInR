#' Conduct a hypothesis test with 100(1-alpha) percent confidence for the mean of a single population
#'
#'@param xbar The mean of the sample.
#'@param mu The hypothesized value of the population mean.
#'@param variance Either the sample or population variance (the latter only if it is known).
#'@param n The size of the sample.
#'@param popVarKnown Boolean denoting whether the population variance is known. False by default.
#'@param alternative A string denoting the form of the alternative hypothesis Acceptable values are "smaller" (<), "greater" (>), and "one.sided" (=/=).
#'@param alpha The significance level. Set to 0.05 by default.
#'@return The pvalue produced by the hypothesis test.
#'@export
Mean.1Pop.Hypothesis = function(xbar, mu, variance, n, popVarKnown=T, alternative="two.sided", alpha=0.05){

  testStat = (xbar - mu) / (variance/sqrt(n))
  pvalue = 0
  cat("Test statistic: ", testStat, "\n", sep="")

  if(popVarKnown){
    if(alternative=="greater"){ pvalue = pnorm(testStat, lower.tail = F) }
    else if(alternative=="smaller"){ pvalue = pnorm(testStat) }
    else { pvalue = 2*pnorm(abs(testStat), lower.tail=F) }
  }
  else{
    if(alternative=="greater"){ pvalue = pt(testStat, n-1, lower.tail=F) }
    else if(alternative=="smaller"){ pvalue = pt(testStat, n-1) }
    else { pvalue = 2*pt(abs(testStat), n-1, lower.tail=F) }
  }

  cat("P-value: ", pvalue, "\n", sep="")

  if(pvalue <= alpha){ cat("Reject null hypothesis.\n") }
  else { cat("Fail to reject null hypothesis.\n") }

  return(pvalue)
}