#' Used internally by all functions producing intervals of any sort (prediction or confidence)
#' @keywords internal
IntervalOutput <- function(lowerValue, upperValue, confidenceLevel, confidenceOrPrediction="Confidence", one.sided){

  interval = NULL

  if(one.sided){
    cat(confidenceLevel, " One-Sided ", confidenceOrPrediction, " Intervals:\n\n", sep="")
    interval = list(lowerBound=c(lowerValue, Inf), upperBound=c(0, upperValue))
  }
  else{
    cat(confidenceLevel, " Two-Sided ", confidenceOrPrediction, " Interval:\n\n", sep="")
    interval = c(lowerValue, upperValue)
  }

  return(interval)
}
