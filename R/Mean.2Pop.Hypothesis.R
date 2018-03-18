#' Two Population Means - Hypothesis Test
#' @description Performs a hypothesis test with 100(1-alpha) percent confidence for the difference in the means of two populations
#'@param xbar1 the sample mean of population 1
#'@param xbar2 the sample mean of population 2
#'@param n1 the size of the sample from population 1
#'@param n2 the size of the sample from population 2
#'@param var1 the sample or population variance of population 1 (the latter only if it is known).
#'@param var2 the sample or population variance of population 2 (the latter only if it is known).
#'@param differenceInPopulationMeans the hypothesized difference between the population means. Set to 0 by default.
#'@param popVarKnown boolean denoting whether the population variance is known. False by default.
#'@param alternative string denoting the form of the alternative hypothesis Acceptable values are "smaller" (<), "greater" (>), and "one.sided" (=/=).
#'@param equal boolean denoting whether the population variances are equal. False by default. Used only in the case
#'@param alpha the significance level. Set to 0.05 by default.
#'@return The test statistic, p-value, and conclusion of the hypothesis.
#'@export
Mean.2Pop.Hypothesis <- function(xbar1, xbar2, n1, n2, var1, var2, differenceInPopulationMeans=0, popVarKnown=F, alternative="two.sided", equal="F", alpha=0.05){

  if(!(alternative %in% c("smaller", "greater", "two.sided"))){
    stop(message=("Parameter alternative must be 'smaller', 'greater', or 'two.sided'"))
  }

  xbar = xbar1 - xbar2
  testStat = 0
  pvalue = 0

  if(popVarKnown){

    testStat = (xbar - differenceInPopulationMeans) / sqrt(var1/n1 + var2/n2)
    if(alternative=="greater"){ pvalue = pnorm(testStat, lower.tail=F) }
    else if(alternative=="smaller"){ pvalue = pnorm(testStat) }
    else{ pvalue = 2*pnorm(abs(testStat), lower.tail=F) }

  }
  else{

    freedom = 0

    if(equal){
      sp = sqrt((var1*(n1-1) + var2*(n2-1)) / (n1+n2-2))
      freedom = n1 + n2 - 2
      testStat = (xbar - differenceInPopulationMeans) / (sp * sqrt(1/n1 + 1/n2))
    }
    else{
      freedom = ((var1/n1 + var2/n2)^2) / ( ((var1/n1)^2/(n1-1)) + ((var2/n2)^2/(n2-1)) )
      testStat = (xbar - differenceInPopulationMeans) / ( sqrt(var1/n1 + var2/n2) )
    }

    if(alternative=="greater"){ pvalue = pt(testStat, freedom, lower.tail=F) }
    else if(alternative=="smaller"){ pvalue = pt(testStat, freedom) }
    else{ pvalue = 2*pt(abs(testStat), freedom, lower.tail=F) }

  }

  FormHypothesisConclusion(alpha, testStat, pvalue)
}
