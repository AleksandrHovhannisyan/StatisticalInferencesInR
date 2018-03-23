#' @keywords internal
GetSlopeEstimates <- function(form, data){

  model_summary = summary(lm(formula=formula(form), data))

  standardDeviation_Beta1 = model_summary$coefficients[, "Std. Error"][2]

  names(standardDeviation_Beta1) <- NULL

  slopeEstimate = model_summary$coefficients[, "Estimate"][2]

  names(slopeEstimate) <- NULL

  return( list(error.beta1=standardDeviation_Beta1, beta1=slopeEstimate) )
}
