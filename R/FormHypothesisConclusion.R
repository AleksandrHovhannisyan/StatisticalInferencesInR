#' @keywords internal
FormHypothesisConclusion <- function(alpha, testStat, pvalue){

  rejectNull = FALSE
  if(pvalue <= alpha){ rejectNull = TRUE }
  cat("\n")
  return( list(test.stat=testStat, p.value=pvalue, reject.null=rejectNull) )
}
