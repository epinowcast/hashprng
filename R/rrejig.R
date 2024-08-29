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
  .FUN <- function() {}
  formals(.FUN) <- c(formals(rFUN), alist(hash = ))
  dispatch <- as.character(substitute(rFUN))
  hashc <- str2lang("set.hash(hash, get('.hash.salt', envir = parent.frame()))")
  origc <- str2lang(sprintf("%s%s%s(%s)",dispatch[2],dispatch[1],dispatch[3],toString(names(formals(rFUN)))))
  body(.FUN) <- substitute({
    hashc
    origc
  })
  environment(.FUN) <- parent.frame()
  return(.FUN)
}

#' @title Hash-Matched sample
#' @param hash an object identifying the event related to the draw
#' @inheritParams base::sample
#' @export
sample <- rrejig(base::sample)

#' @title Hash-Matched sample.int
#' @param hash an object identifying the event related to the draw
#' @inheritParams base::sample.int
#' @export
sample.int <- rrejig(base::sample.int)
