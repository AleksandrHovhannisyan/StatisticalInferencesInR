#' Used internally by all functions producing intervals of any sort (prediction, confidence)
IntervalOutput <- function(xbar, marginOfError, confidence, one.sided){

  interval = NULL

  if(one.sided){
    cat(confidence, "One-Sided Confidence Intervals:\n\n")
    interval = list(lowerBound=c(xbar - marginOfError, Inf), upperBound=c(0, xbar + marginOfError))
  }
  else{
    cat(confidence, "Two-Sided Confidence Interval:\n\n")
    interval = c(xbar - marginOfError, xbar + marginOfError)
  }

  return(interval)
}
