#' One Population Variance - Confidence Interval
#' @description Constructs a 100(1-alpha) percent confidence interval for a single population variance
#' @param sampleVar the variance of the sample
#' @param n the size of the sample
#' @param alpha the significance level. Set to 0.05 by default.
#' @param one.sided boolean denoting whether the function should return one-sided confidence intervals. False by default.
#' @return The confidence interval(s): one-sided lower and upper bounds (in a list) or just the two-sided confidence interval (as a simple vector).
#' @export
Variance.1Pop.CI <- function(sampleVar, n, alpha=0.05, one.sided=F){

  interval = NULL

  if(one.sided){ alpha = 2 * alpha }

  df = n - 1

  numerator = sampleVar * df

  IntervalOutput(numerator / qchisq(alpha/2, df, lower.tail=F), numerator / qchisq(alpha/2, df), GetConfidenceLevel(alpha), "Confidence", one.sided)
}
