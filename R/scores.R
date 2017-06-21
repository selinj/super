

#' purity
#'
#' @export
purity <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  maxes  <- apply(contingency, 2, max)
  purity <- sum(maxes) / sum(contingency)

  return(purity)

}


#' classEntropy
#'
#' @export
classEntropy <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  n_c <- rowSums(contingency)
  n   <- sum(contingency)
  H   <- - sum( n_c/n * log(n_c/n) )

  return(H)

}


#' clusterEntropy
#'
#' @export
clusterEntropy <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  n_k <- colSums(contingency)
  n   <- sum(contingency)
  H   <- - sum( n_k/n * log(n_k/n) )

  return(H)

}


#' classEntropyGivenClusters
#'
#' @export
conditionalClassEntropy <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  inner <- apply(contingency, 1, function(c_i) {
    k_i <- c_i/sum(contingency) * log( c_i/colSums(contingency) )
    k_i[is.nan(k_i)] <- 0
    return(k_i)
  })

  H <- - sum(inner)

  return(H)

}


#' clusterEntropyGivenClasses
#'
#' @export
conditionalClusterEntropy <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  inner <- apply(contingency, 2, function(k_i) {
    c_i <- k_i/sum(contingency) * log( k_i/rowSums(contingency) )
    c_i[is.nan(c_i)] <- 0
    return(c_i)
  })

  H <- - sum(inner)

  return(H)

}


#' homogeneity
#'
#' @export
homogeneity <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  H_C_K       <- conditionalClassEntropy(contingency)
  H_C         <- classEntropy(contingency)
  homogeneity <- ifelse(H_C == 0, 1, 1 -  H_C_K/H_C)

  return(homogeneity)

}


#' completeness
#'
#' @export
completeness <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  H_K_C        <- conditionalClusterEntropy(contingency)
  H_K          <- clusterEntropy(contingency)
  completeness <- ifelse(H_K == 0, 1, 1 - H_K_C/H_K)

  return(completeness)

}


#' vMeasure
#'
#' @export
vMeasure <- function(contingency, c, k) {

  if(missing(contingency)) contingency <- table(c, k)

  h <- homogeneity(contingency)
  c <- completeness(contingency)
  v <- ifelse(h + c == 0, 0, 2 * (h * c) / (h + c))

  return(v)

}
