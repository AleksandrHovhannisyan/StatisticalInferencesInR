#' Used internally by all functions using alpha. Generates a string in the form of 100(1-alpha) percent
#' @keywords internal
GetConfidenceLevel <- function(alpha){
  return(cat("\n", 100*(1-alpha),"%", sep=""))
}
