#' Used internally by all functions producing intervals of any sort (prediction, confidence)
#' @keywords internal
IntervalOutput <- function(xbar, marginOfError, confidence, confidenceOrPrediction="Confidence", one.sided){

  interval = NULL

  if(one.sided){
    cat(confidence, " One-Sided ", confidenceOrPrediction, " Intervals:\n\n", sep="")
    interval = list(lowerBound=c(xbar - marginOfError, Inf), upperBound=c(0, xbar + marginOfError))
  }
  else{
    cat(confidence, " Two-Sided ", confidenceOrPrediction, " Interval:\n\n", sep="")
    interval = c(xbar - marginOfError, xbar + marginOfError)
  }

  return(interval)
}
