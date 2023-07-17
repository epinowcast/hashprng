
#' @title Hash-Based Matching Pseudo-Random Number Generation
#'
#' @details
#' This function provides a one-step invocation for hash-based matching
#' pseudo-random generation (HBM-PRNG).
#'
#' For matched stochastic simulation, we desire a few properties:
#'  - the _same_ random events are resolved _consistently_
#'  - possibility of different stochastic samples
#'  - reproducibility of pseudo-random simulations
#'
#' Traditional PRNG seeding provides the latter points. To the extent that the
#' PRNG is traversed the same way across simulations, events will also be
#' resolved consistently. However, once event resolution leads to diverging
#' outcomes (the whole point of doing otherwise-matched simulations with some
#' parameter varying), the overall trajectory of the simulation will be begin
#' to exercise the PRNG differently. When *different* events occur between the
#' samples, this does not matter - one random deviate is as good as another.
#' However, diverging trajectories can still share some of the same events.
#' These events should be resolved _consistently_: for example, if a
#' probabilistic threshold is increasing across scenarios, then a particular
#' event testing that should only change from pass to fail. In practice, this
#' means that same events need have the same PRNG draws, which is not possible
#' if the PRNG state has otherwise diverged due to other parts of the
#' simulation.
#'
#' The HBM PRNG approach encodes events such that when they are the same (as
#' defined by the simulation), they create identical hashes, which are then used
#' to set the PRNG state. This ensure the same subsequent draws for that event.
#'
#' @param salt the matching value for a particular collection of simulations
#'
#' @param ... distinguishing features to identify the event; see details.
#'
#' @export
#'
#' @examples
#'
#' salt <- 8675309
#' evt <- list(type = "infection", from = 1, to = 2, time = 3.1)
#' evt2 <- list(type = "recovery", from = 1, to = 2, time = 3.1)
#' salt |> hash_seed(evt$type, evt$from, evt$to, evt$time)
#' print(runif(10))
#' print(runif(10))
#' salt |> hash_seed(evt$type, evt$from, evt$to, evt$time)
#' print(runif(10))
#' salt <- 42
#' salt |> hash_seed(evt$type, evt$from, evt$to, evt$time)
#' print(runif(10))
hash_seed <- function(salt, ...) {
  # first, send all the `...` arguments to binary representation
  # HAEC SUNT DRACONES: leaves all error handling to `writeBin`
  binned <- Reduce(
    c,
    lapply(list(...), writeBin, con = raw()), init = writeBin(salt, raw())
  )
  res <- .Call('_hbmPRNG_digest', binned, PACKAGE = 'hbmPRNG')
  # todo: move set seed into hbmPRNG_digest?
  set.seed(res)
}
