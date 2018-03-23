#' Simple Linear Regression - Hypothesis Test
#' @description Performs a hypothesis test with 100(1-alpha) percent confidence for the slope of a simple linear regression model.
#' @param form expression representing the relationship between the variables contained in data (e.g., y ~ x). Variable names in this expression must match the column names of data.
#' @param data data frame object containing the data. Columns must represent the predictor and response variables, in either order.
#' @param beta_hypothesized the hypothesized value of the slope of the true linear model for the data, as specified by the null hypothesis.
#' @param n the number of data points collected.
#' @param alternative string denoting the form of the alternative hypothesis Acceptable values are "smaller" (<), "greater" (>), and "one.sided" (=/=).
#' @param alpha the significance level. Set to 0.05 by default.
#' @return The test statistic, p-value, and conclusion of the hypothesis.
#' @export
Slope.1Regressor.Hypothesis <- function(form, data, beta_hypothesized, n, alternative="two.sided", alpha=0.05){

  if(!(alternative %in% c("smaller", "greater", "two.sided"))){
    stop(message=("Parameter alternative must be 'smaller', 'greater', or 'two.sided'"))
  }

  estimates = GetSlopeEstimates(form, data)
  beta1 = estimates$beta1
  sd = estimates$error.beta1

  testStat = (beta1 - beta_hypothesized) / sd
  pvalue = NULL

  if(alternative=="greater"){ pvalue = pt(testStat, n-2, lower.tail=F) }
  else if(alternative=="smaller"){ pvalue = pt(testStat, n-2) }
  else { pvalue = 2*pt(abs(testStat), n-2, lower.tail=F) }

  FormHypothesisConclusion(alpha, testStat, pvalue)
}
