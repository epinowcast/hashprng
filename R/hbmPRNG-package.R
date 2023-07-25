
## usethis namespace: start
#' @useDynLib hbmPRNG, .registration = TRUE
#' @importFrom Rcpp evalCpp
## usethis namespace: end
NULL

#' hbmPRNG: Hash-Based Matching Pseudo-Random Number Generation
#'
#' The hbmPRNG package provides single function for use during stochastic
#' simulation to streamline salting + event hashing + reseeding the R random
#' number generator. The sole function in the package is `hash_seed`.
#'
#' @docType package
#' @name hbmPRNG
NULL
