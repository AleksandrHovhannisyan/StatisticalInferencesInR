#' Simple Linear Regression - Slope Confidence Interval
#' @description Constructs a 100(1-alpha) percent confidence interval for the slope of a simple linear regression model.
#' @param form expression representing the relationship between the variables contained in data (e.g., y ~ x). Variable names in this expression must match the column names of data.
#' @param data data frame object containing the data. Columns must represent the predictor and response variables, in either order.
#' @param n the number of data points collected.
#' @param alpha the significance level. Set to 0.05 by default.
#' @param one.sided boolean denoting whether the function should return one-sided confidence intervals. False by default.
#' @return The confidence interval(s): one-sided lower and upper bounds (in a list) or just the two-sided confidence interval (as a simple vector).
#' @export
Slope.1Regressor.CI <- function(form, data, n, alpha=0.05, one.sided=F){

  if(one.sided){ alpha = 2*alpha }

  tvalue = abs(qt(alpha/2, df = n - 2, lower.tail=F))

  model_summary = summary(lm(formula=formula(form), data))

  standardDeviation_Beta1 = model_summary$coefficients[, "Std. Error"][2]

  names(standardDeviation_Beta1) <- NULL

  slopeEstimate = model_summary$coefficients[, "Estimate"][2]

  names(slopeEstimate) <- NULL

  marginOfError = tvalue * standardDeviation_Beta1

  IntervalOutput(slopeEstimate - marginOfError, slopeEstimate + marginOfError, GetConfidenceLevel(alpha), "Confidence", one.sided)
}
