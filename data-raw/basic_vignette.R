#' This code calculates & adds to inst the data associated with
#' the basic vignette.

library(hbmPRNG)

# parse the command args
.args <- if (interactive()) c(
  file.path("data-raw", "config.R"),
  file.path("inst", "basic_vignette.rda")
) else commandArgs(trailingOnly = TRUE)

# load shared definitions
source(.args[1])

#' @param t an integer scalar, the simulation time
#' @param y an integer vector, the current counts of individuals in S, I, and R
#' @param parameters a list, `transmission_p`, `recovery_p`, `vaccination_p`,
#'   and `vax_t`
#' @param salt optional; if non-null, using HBM
nonidentity_dStep <- function(t, y, parameters, salt = NULL) with(parameters, {

  # extract the S, I population counts
  num_infectious <- y[infectious]
  num_susceptible <- y[susceptible]

  # if we are matching events
  if (!is.null(salt)) {
    # draw the same quantiles for each when
    hash_seed(salt, t)
    evtqs <- runif(2)
    # convert these to the outcomes
    newinf <- qbinom(evtqs[1], num_susceptible, 1-(1-transmission_p)^num_infectious)
    newrem <- qbinom(evtqs[2], num_infectious, recovery_p)
  } else { # simply make the binomial draws
    newinf <- rbinom(1, num_susceptible, 1-(1-transmission_p)^num_infectious)
    newrem <- rbinom(1, num_infectious, recovery_p)
  }

  # move infectees S -> I, recoveries I -> R
  dY <- c(S = -newinf, I = newinf - newrem, R = newrem)

  if (t == vax_t) {
    if (!is.null(salt)) {
      # n.b. still on a consistent PRNG state for this time, so can just draw
      newvax <- qbinom(runif(1), num_susceptible - newinf, vaccination_p)
    } else {
      newvax <- rbinom(1, num_susceptible - newinf, vaccination_p)
    }
    # move vaccinees
    dY[susceptible] <- dY[susceptible] - newvax
    dY[removed] <- dY[removed] + newvax
  }

  # return the overall state change, as well as new incidence
  return(list(dY, newinf))
})

set.seed(8675309)
nonid_no_match <- system.time(
  ni_non_dt <- seq_len(samplen) |> parallel::mclapply(function(seed) data.table(
    type = "NON", model = "nonID",
    not_i = cumsum(stepper(
      y0 = yinit_nonid, dFUN = nonidentity_dStep, pars = nonps, HBM = FALSE
    )),
    withi = cumsum(stepper(
      y0 = yinit_nonid, dFUN = nonidentity_dStep, pars = intps, HBM = FALSE
    ))
  ), mc.cores = 5) |> rbindlist(idcol = "sample")
)

nonid_seed_match_only <- system.time(
  ni_smo_dt <- seq_len(samplen) |> parallel::mclapply(function(seed) data.table(
    type = "SMO", model = "nonID",
    not_i = cumsum(stepper(
      y0 = yinit_nonid, dFUN = nonidentity_dStep, pars = nonps, seed = seed, HBM = FALSE
    )),
    withi = cumsum(stepper(
      y0 = yinit_nonid, dFUN = nonidentity_dStep, pars = intps, seed = seed, HBM = FALSE
    ))
  ), mc.cores = 5) |> rbindlist(idcol = "sample")
)

nonid_hash_based_matching <- system.time(
  ni_hbm_dt <- seq_len(samplen) |> mclapply(function(seed) data.table(
    type = "HBM", model = "nonID",
    not_i = cumsum(stepper(
      y0 = yinit_nonid, dFUN = nonidentity_dStep, pars = nonps, seed = seed, HBM = TRUE
    )),
    withi = cumsum(stepper(
      y0 = yinit_nonid, dFUN = nonidentity_dStep, pars = intps, seed = seed, HBM = TRUE
    ))
  ), mc.cores = 5) |> rbindlist(idcol = "sample")
)

timings <- list(
  non = nonid_no_match, smo = nonid_seed_match_only, hbm = nonid_hash_based_matching
)

samples_dt <- rbind(ni_non_dt, ni_smo_dt, ni_hbm_dt)

save(timings, samples_dt, file = .args[2])

