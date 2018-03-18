#' @keywords internal
FormHypothesisConclusion <- function(alpha, testStat, pvalue){

  rejectNull = pvalue <= alpha
  cat("\n")
  return( list(test.stat=testStat, p.value=pvalue, reject.null=rejectNull) )
}
