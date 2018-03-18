#'One Population Mean - Hypothesis Test
#'@description Performs a hypothesis test with 100(1-alpha) percent confidence for the mean of a single population
#'@param xbar the mean of the sample
#'@param mu the hypothesized value of the population mean
#'@param variance either the sample or population variance (the latter only if it is known)
#'@param n the size of the sample
#'@param popVarKnown boolean denoting whether the population variance is known. False by default.
#'@param alternative string denoting the form of the alternative hypothesis Acceptable values are "smaller" (<), "greater" (>), and "one.sided" (=/=).
#'@param alpha the significance level. Set to 0.05 by default.
#'@return The pvalue produced by the hypothesis test.
#'@export
Mean.1Pop.Hypothesis = function(xbar, mu, variance, n, popVarKnown=T, alternative="two.sided", alpha=0.05){

  if(!(alternative %in% c("smaller", "greater", "two.sided"))){
    stop(message=("Parameter alternative must be 'smaller', 'greater', or 'two.sided'"))
  }

  testStat = (xbar - mu) / sqrt(variance / n)
  result = NULL
  pvalue = NULL
  conclusion = "Fail to reject null hypothesis."

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

  FormHypothesisConclusion(alpha, testStat, pvalue)
}
