
#' @title Set Hash Salt
#'
#' @description
#' This function sets a environment value to be used for hash-matched
#' pseudo-random number generation (PRNG). This means that `salt`
#' values are scoped.
#'
#' This is the conceptual equivalent of [set.seed()] with unmatched
#' PRNG.
#'
#' @param salt an object that will be used as the starter object for
#' all hash-matched. A plain integer is sufficient.
#'
#' @export
set.salt <- function(salt) {
  assign(".hash.salt", salt, envir = parent.frame())
}

#' @title Manage RNG state
#'
#' @importFrom digest digest
#' @export
set.hash <- function(hash, .hash.salt) {
  if (!missing(hash)) {
    obj <- list(.hash.salt, hash)
    set.seed(readBin(digest(
      obj, algo = "xxhash32", raw = TRUE
    ), "integer"))
  } else {
    warning("Using hash-prng methods without providing a hash.")
  }
}
