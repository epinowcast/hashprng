#' @title Add Hash-seeding to a Deviate Generators
#'
#' @description
#' This function provides a way extend the arguments of deviate
#' generating functions -- that is, the `runif`, `rbinom`, etc.
#'
#' Internally, `hashprng` applies this to commonly used `{stats}`
#' functions. When you attach `hashprng`, those are all masked.
#'
#' @param rFUN a function
#'
#' @return a new function, which should have all the same arguments
#' plus a new `hash` argument at the end.
#'
#' @export
rrejig <- function(rFUN) {
  .FUN <- rFUN # make a duplicate of FUN
  formals(.FUN) <- c(formals(rFUN), alist(hash = ))
  hashc <- str2lang("hashprng::set.hash(hash)")
  origc <- body(rFUN)
  body(.FUN) <- substitute({
    hashc
    origc
  })
  environment(.FUN) <- environment(rFUN)
  return(.FUN)
}
