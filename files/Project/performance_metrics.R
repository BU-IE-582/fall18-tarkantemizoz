
#' Performance Metric: Ranked Probability Score
#'
#' @param probs vector of 3, predicted probabilities
#' @param outcomes vector of 3, binary outcomes, should be provided with the same order as probs
#' @export
#' @examples
RPS_single<- function(probs,outcomes){
  probs = cumsum(probs)
  outcomes = cumsum(outcomes)
  RPS = sum((probs-outcomes )^2) / (length(probs)-1)
  return(RPS)
}

RPS_matrix<- function(probs,outcomes){
  probs=as.matrix(probs)
  outcomes=as.matrix(outcomes)
  probs=t(apply(t(probs), 2, cumsum))
  outcomes=t(apply(t(outcomes), 2, cumsum))
  RPS = apply((probs-outcomes)^2,1,sum) / (ncol(probs)-1)
  return(RPS)
}


