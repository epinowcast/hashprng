#' This code calculates & adds to inst the data associated with
#' the intermediate vignette.

library(hbmPRNG)
library(data.table)
library(parallel)

# parse the command args
.args <- if (interactive()) c(
  file.path("data-raw", "config.R"),
  file.path("inst", "intermediate_vignette.rda")
) else commandArgs(trailingOnly = TRUE)

# load shared definitions
source(.args[1])

#' @param t an integer scalar, the simulation time
#' @param y an integer vector, the individual states: S, I, R
#' @param parameters a list, `transmission_p`, `recovery_p`, `vaccination_p`,
#'   and `vax_t`
#' @param salt an integer scalar, optional; if present, use hash-based matching
identity_dStep <- function(t, y, parameters, salt = NULL) with(parameters, {

  dY <- integer(N)   # the associated changes
  new_inf <- 0       # new incidence in this time step

  # consider whether each infectious individual exposes susceptible
  # individuals
  infectable <- which(y == susceptible)

  if (!is.null(salt)) {
    # update to the partial salt
    salt <- hash_salt(salt, "i", t)
  }

  for (infector in which(y == infectious)) if (length(infectable)) {

    if (!is.null(salt)) {
      # if HBM, need to reseed RNG for each pair => draw
      # but also possible to salt => draw all possible => slice relevant
      hash_seed(salt, infector)
      infectees <- infectable[
        (runif(tail(infectable, 1)) < transmission_p)[infectable]
      ]
    } else {
      # if un-matched, can simply draw how many infected => sample which
      infectees <- sample(
        infectable, rbinom(1, length(infectable), transmission_p)
      )
    }

    # infect the identified individuals, reduce the infectable
    dY[infectees] <- 1
    infectable <- setdiff(infectable, infectees)

    new_inf <- new_inf + length(infectees)

    # if this infector would recovery, indicate 2 => 3 transition
    # n.b. if HBM, still on the same set of draws
    if(runif(1) < recovery_p) dY[infector] <- 1
  }

  incI <- sum((dY == 1) & (y != infectious))

  # if its time for vaccination
  if (t == vax_t) {
    if (!is.null(salt)) {
      # if HBM, need to reseed for each individual
      # include the t here - what if the model changed vax_t?
      psalt <- hash_seed(salt, "vax", t)
      vaccinees <- infectable[
        (runif(tail(infectable, 1)) < vaccination_p)[infectable]
      ]
    } else {
      vaccinees <- sample(
        infectable, rbinom(1, length(infectable), vaccination_p)
      )
    }
    # for people still susceptible, indicate 1 => 3 transition
    dY[vaccinees] <- 2
  }

  return(list(dY, incI))
})

set.seed(8675309)
id_no_match <- system.time(
  wi_non_dt <- seq_len(samplen) |> parallel::mclapply(function(seed) data.table(
    type = "NON", model = "ID",
    not_i = cumsum(stepper(
      y0 = yinit, dFUN = identity_dStep, pars = nonps, HBM = FALSE
    )),
    withi = cumsum(stepper(
      y0 = yinit, dFUN = identity_dStep, pars = intps, HBM = FALSE
    ))
  ), mc.cores = 5) |> rbindlist(idcol = "sample")
)

id_seed_match_only <- system.time(
  wi_smo_dt <- seq_len(samplen) |> parallel::mclapply(function(seed) data.table(
    type = "SMO", model = "ID",
    not_i = cumsum(stepper(
      y0 = yinit, dFUN = identity_dStep, pars = nonps, seed = seed, HBM = FALSE
    )),
    withi = cumsum(stepper(
      y0 = yinit, dFUN = identity_dStep, pars = intps, seed = seed, HBM = FALSE
    ))
  ), mc.cores = 5) |> rbindlist(idcol = "sample")
)

id_hash_based_matching <- system.time(
  wi_hbm_dt <- seq_len(samplen) |> mclapply(function(seed) data.table(
    type = "HBM", model = "ID",
    not_i = cumsum(stepper(
      y0 = yinit, dFUN = identity_dStep, pars = nonps, seed = seed, HBM = TRUE
    )),
    withi = cumsum(stepper(
      y0 = yinit, dFUN = identity_dStep, pars = intps, seed = seed, HBM = TRUE
    ))
  ), mc.cores = 5) |> rbindlist(idcol = "sample")
)

timings <- list(
  non = id_no_match, smo = id_seed_match_only, hbm = id_hash_based_matching
)

samples_dt <- rbind(wi_non_dt, wi_smo_dt, wi_hbm_dt)

save(timings, samples_dt, file = .args[2])
