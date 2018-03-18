#' @keywords internal
FormHypothesisConclusion <- function(alpha, testStat, pvalue){

  conclusion = "Fail to reject null hypothesis."
  if(pvalue <= alpha){ conclusion = "Reject null hypothesis." }
  cat("\n")
  return( list(test.stat=testStat, p.value=pvalue, conclusion=conclusion) )
}
