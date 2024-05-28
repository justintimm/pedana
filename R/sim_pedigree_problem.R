#' Simulate a pedigree problem
#'
#' User friendly function that creates a full pedigree problem by simulating a
#' pedigree (via [sim_pedigree()]) and a genetic trait (using [sim_trait()],
#' [sim_trait_xd()], or [sim_trait_xr()]). Returns an object of class `ped`,
#' analysed with the [analyse_pedigree()] function. Sometimes the simulation is
#' not successful within iteration and time limits. Consider setting `force`
#' to `TRUE` to enforce a successful simulation.
#'
#' @param inheritance Inheritance of the trait to be simulated.
#' @param generations Number of `generations` to simulate. The minimum number of
#' `generations` is 2. There is no upper limit, but the objects can quickly
#' become very large for high numbers of `generations` and large total fertility
#' rates. See [sim_pedigree()] for details.
#' @param iter_limit Maximum number of iterations for attempting to simulate
#' a pedigree problem. Simulation may fail. Ignored if `force` is set to `TRUE`.
#' @param time_limit Time limit in seconds for attempting to simulate a
#' pedigree problem. Simulation may fail. Ignored if `force` is set to `TRUE`.
#' @param force Set to `TRUE` to use a more ruthless method to simulate an
#' unambiguous pedigree. The script runs until a valid pedigree is created.
#' @param seed Set seed to create reproducible results.
#' @param ... Other arguments passed to the pedigree simulation function
#' [sim_pedigree()].
#'
#' @return Returns an object of class `ped`.
#'
#' @examples
#' obj1 <- sim_pedigree_problem(inheritance = "AR", generations = 3, seed = 5)
#' plot(obj1)
#'
#' ## The following simulation is not successful
#' \dontrun{sim_pedigree_problem(inheritance = "XR", generations = 3, seed = 1)}
#'
#' ## Simulation is successful with force set to TRUE
#' obj2 <- sim_pedigree_problem(inheritance = "XR", generations = 3, seed = 1, force = TRUE)
#' plot(obj2)
#' @export
#' @md
sim_pedigree_problem <- function(inheritance = c("AD", "AR", "XD", "XR"),
                                 generations = 4,
                                 iter_limit = 20,
                                 time_limit = 5,
                                 force = FALSE,
                                 seed = NULL,
                                 ...) {

  start_time <- Sys.time()

  inheritance <- match.arg(inheritance)

  if (! is.numeric(generations) || generations < 3)
    stop("`generations` must be a positive numeric value greater than 2.")

  set.seed(seed)

  i <- comp_time <- 0L
  x <- NULL

  if (force)
    iter_limit <- time_limit <- 0

  while (is.null(x) && i < iter_limit && comp_time < time_limit) {
    x <- sim_pedigree(generations = generations, ...)
    x <- tryCatch(
      expr = {
        sim_trait(x, inheritance = inheritance)
      },
      error = function(e) { return(NULL) }
    )
    comp_time <- as.numeric(Sys.time() - start_time, units = "secs")
    i <- i + 1
  }

  start_time_force <- Sys.time()

  if (is.null(x) && force) {

    i <- comp_time <- 0L

    while (is.null(x)) {
      x <- sim_pedigree(generations = generations, ...)
      x <- tryCatch(
        expr = {
          switch(inheritance,
                 AD = sim_trait(x, inheritance = inheritance),
                 AR = sim_trait(x, inheritance = inheritance),
                 XD = sim_trait_xd(x),
                 XR = sim_trait_xr(x))
        },
        error = function(e) { return(NULL) }
      )
      comp_time <- as.numeric(Sys.time() - start_time_force, units = "secs")
      i <- i + 1
    }

  }

  if (is.null(x)) {
    stop("Not successful")
  } else {
    x <- analyse_pedigree(x)
  }

  x$time_prob_gen <- Sys.time() - start_time
  x$seed <- seed

  x
}
