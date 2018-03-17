#' Conduct a hypothesis test with 100(1-alpha) percent confidence for the mean of a single population
#'
Mean.1Pop.Hypothesis <- function(xbar, mu, variance, n, popVarKnown=T, alternative="two.sided", alpha){

  testStat <- (xbar - mu) / (variance/sqrt(n))
  cat("Test statistic: ", testStat, "\n")

  if(popVarKnown){

    if(alternative=="greater"){ pvalue <- pnorm(testStat, lower.tail = F) }
    else if(alternative=="smaller"){ pvalue <- pnorm(testStat) }
    else { pvalue <- 2*pnorm(abs(testStat), lower.tail=F) }

  }
  else{

    if(alternative=="greater"){ pvalue <- pt(testStat, n-1, lower.tail=F) }
    else if(alternative=="smaller"){ pvalue <- pt(testStat, n-1) }
    else { pvalue <- 2*pt(abs(testStat), n-1, lower.tail=F) }
  }

  if(pvalue <= alpha){ cat("Reject null hypothesis. P-value: ", pvalue) }
  else { cat("Fail to reject null hypothesis. P-value: ", pvalue) }
}
