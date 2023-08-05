
susceptible <- 1L
infectious  <- 2L
removed     <- 3L

samplen <- 500L
pop <- 4000L
maxt <- 75L
initI <- 5L

ps <- list(
  N = pop,
  transmission_p = 1 - exp(-0.78/pop),
  # ~ 0.78 infections produced per day in fully susceptible pop
  recovery_p = 1 - exp(-0.44) # ~ 2.3 days infectious
  # implies R0 ~ 1.8
)

# non-intervention, set vax_t < 0
nonps <- c(ps, list(
  vax_t = -1, vaccination_p = NA
))

# intervention, vax_t == 10, 3.3% chance to happen
intps <- c(ps, list(
  vax_t = 10, vaccination_p = 0.033
))

# create populations
yinit <- rep.int(susceptible, pop)
yinit[1:initI] <- infectious
yinit_nonid <- c(pop - initI, initI, 0L)

ncores <- parallel::detectCores() - 1L

stepper <- function(
  maxtime = maxt, y0,
  dFUN, pars, seed, HBM
) {

  # if not HBM & a seed provided => SMO => set initial seeding
  if (!missing(seed) && !HBM) set.seed(seed)

  # reserve data structure
  res <- integer(maxtime + 1)
  res[1] <- 1
  y <- y0
  # run the simulation
  for (i in seq_len(maxtime)) {
    dY <- if (HBM) dFUN(i, y, pars, seed) else dFUN(i, y, pars)
    y <- y + dY[[1]]
    res[i + 1] <- dY[[2]]
  }

  return(res)
}
