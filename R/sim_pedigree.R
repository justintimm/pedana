sim_nuclear_family <- function(pedigree, parent_id, n_children) {

  parent <- pedigree[pedigree$id == parent_id, ]

  partner <- data.frame(id = max(pedigree$id) + 1,
                       father = 0,
                       mother = 0,
                       female = ! parent$female,
                       generation = parent$generation,
                       paternal.allele = NA,
                       maternal.allele = NA,
                       phaenotype = NA,
                       stringsAsFactors = FALSE)

  children <- data.frame(id = seq(from = max(pedigree$id) + 2,
                                  length.out = n_children),
                         father = ifelse(isFALSE(parent$female),
                                         parent$id,
                                         partner$id),
                         mother = ifelse(isTRUE(parent$female),
                                         parent$id,
                                         partner$id),
                         female = sample(c(TRUE, FALSE),
                                         size = n_children,
                                         replace = TRUE),
                         generation = parent$generation + 1,
                         paternal.allele = NA,
                         maternal.allele = NA,
                         phaenotype = NA,
                         stringsAsFactors = FALSE)

  pedigree <- rbind(pedigree, partner, children)

  pedigree

}


#' Simulate a pedigree
#'
#' [sim_pedigree()] simulates a pedigree or rather a family structure. A genetic
#' trait can be simulated afterwards, for example using [sim_trait()]. The size
#' of the pedigree depends largely on the arguments.
#'
#' @param generations Number of `generations` to simulate. The minimum number of
#' `generations` is 2. There is no upper limit, but the objects can quickly
#' become very large for high numbers of `generations` and large total fertility
#' rates (`tfr`).
#' @param tfr A positive numeric value. The total fertility rate describes how
#' many children women have on average. Therefore, the `tfr` influences the
#' size of the pedigree.
#'
#' @return Returns an object of class `ped`.
#'
#' @seealso [sim_trait()] and related functions [sim_trait_xd()] and
#' [sim_trait_xr()] to simulate the distribution of a trait for a simulated
#' pedigree.
#'
#' @examples
#' sim_pedigree(generations = 2)
#' set.seed(1)
#' plot(sim_pedigree(generations = 3, tfr = 3))
#'
#' @importFrom stats rpois
#' @importFrom utils packageDescription packageVersion
#'
#' @export
#' @md
sim_pedigree <- function(generations = 4,
                         tfr = 1.5) {

  start_time <- Sys.time()

  if (! is.numeric(generations) || generations < 2)
    stop("Number of `generations` must be at least 2.")

  if (! is.numeric(tfr) || tfr <= 0)
    stop("`tfr` must be a positive numeric value.")

  if (! length(tfr) == 1 && ! length(tfr) == (generations - 1))
    stop("`tfr` must have length 1 or `length(generations)` - 1.")

  if (length(tfr) == 1) tfr <- rep(tfr, generations - 1)


  # create initial person
  pedigree <- data.frame(id = 10,
                         father = 0,
                         mother = 0,
                         female = FALSE,
                         generation = 1,
                         paternal.allele = as.character(NA),
                         maternal.allele = as.character(NA),
                         phaenotype = as.character(NA),
                         stringsAsFactors = FALSE)

  while (max(pedigree$generation) < generations) {

    cases <- pedigree[pedigree$generation == max(pedigree$generation), "id"]
    lambda <- tfr[max(pedigree$generation)]
    gen <- max(pedigree$generation) + 1

    for (id in cases) {
      n_children <- stats::rpois(1, lambda = lambda)

      # prevent that there are no descendants at all, although the pedigree
      # should have at least one further generation
      if (n_children == 0 &&
          id == cases[length(cases)] &&
          gen > max(pedigree$generation)) n_children <- n_children + 1
      else if (n_children == 0) next

      pedigree <- sim_nuclear_family(pedigree, id, n_children)
    }
  }

  system <- list(R_version = getRversion(),
                 pkg = utils::packageDescription("pedana")$Package,
                 pkg_version = utils::packageVersion("pedana"))

  pedigree <- list(system = system,
                   time_ped_gen = Sys.time() - start_time,
                   family_members = NROW(pedigree),
                   generations = max(pedigree$generation),
                   data = pedigree)

  class(pedigree) <- c("ped")

  pedigree
}
