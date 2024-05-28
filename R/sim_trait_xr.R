#' Simulate an X-chromosomal recessive trait
#'
#' [sim_trait_xr()] simulates an X-chromosomal recessive trait. This function
#' is faster than [sim_trait()]. For this purpose, however, the simulated
#' pedigree is modified during the simulation. If this is not desired, the
#' function [sim_trait()] should be used instead. Often the simulation is not
#' successful. Use with caution.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree()] function.
#'
#' @return Returns an object of class `ped`.
#'
#' @seealso [sim_trait_xd()] for X-linked dominant traits and the more general
#' function [sim_trait()].
#'
#' @examples
#' set.seed(7)
#' obj <- sim_pedigree(generations = 4, tfr = 2)
#' obj <- sim_trait_xr(obj)
#' plot(obj)
#'
#' @importFrom kinship2 kinship
#'
#' @export
#' @md
sim_trait_xr <- function(x) {

  start_time <- Sys.time()

  if (! inherits(x, "ped"))
    stop("Unexpected input. Expects an object of class `ped`.")

  pedigree <- x$data
  x$complete <- FALSE
  x$stat_xr <- NULL

  pedigree[pedigree$generation == 1 & pedigree$female == TRUE, 6:7] <-
    data.frame("x", sample(c("X", "x"), size = 1, replace = F, prob = c(.7, .3)))

  pedigree[pedigree$generation == 1 & pedigree$female == FALSE, 6:7] <-
    c("Y", sample(c("X", "x"), size = 1, replace = F, prob = c(.9, .1)))

  pedigree[pedigree$generation > 1 & pedigree$mother == 0 &
             pedigree$father == 0 & pedigree$female == TRUE, 6:7] <-
    data.frame("X", sample(c("X", "x"), size = 1, replace = F, prob = c(.6, .4)))

  pedigree[pedigree$generation > 1 & pedigree$mother == 0 &
             pedigree$father == 0 & pedigree$female == FALSE,
           6:7] <- data.frame("Y", "X")

  i <- 0
  while (isFALSE(x$complete)) {
    i <- i + 1

    pedigree <- sim_transmission(pedigree, "XR")
    pedigree <- sim_phaenotype(pedigree, "XR")

    x$data <- pedigree
    analysis <- analyse_pedigree(x)$analysis
    x$complete <- analysis$unique_solution
    x$stat_xr <- c(x$stat_xr, analysis$stat_xr)

    if (i >= 5) {
      if (x$stat_xr[length(x$stat_xr)] == x$stat_xr[length(x$stat_xr) - 4])
        stop("No solution found.")
    }


    # Optimisation
    if (isFALSE(x$complete)) {

      # In which generation is the allele absent for the first time?
      res <- list()
      for (gen_i in max(pedigree$generation):1) {
        desc <- pedigree[pedigree$generation == gen_i &
                           pedigree$father > 0 &
                           pedigree$mother > 0, 6:7]

        res[[gen_i]] <- all(apply(desc, 1, function(x) "x" %in% x))
      }

      if (all(res == TRUE))
        stop("No solution found.")

      gen <- min(which(unlist(res) == FALSE))
      pedigree[pedigree$generation > gen &
                 pedigree$father > 0 &
                 pedigree$mother > 0, 6:7] <- NA

      desc <- pedigree[pedigree$generation == gen &
                         pedigree$father > 0 &
                         pedigree$mother > 0, ]

      # Inspect descendants
      x$data <- pedigree
      kinship <- kinship2::kinship(convert2kinship2(x), chrtype = "X")
      desc$ibd <- kinship[2, as.numeric(rownames(desc))]

      # Change the sex of a male who cannot possess the allele of interest
      if (any(desc$ibd == 0)) {
        male_rowid <- sample(which(desc$ibd == 0), 1)
        male <- desc[male_rowid, "id"]

        # If the male has children, change his position with the mother's
        # position. If not, change his sex.
        if (male %in% pedigree$father) {
          female <- unique(pedigree[pedigree$father == male, "mother"])
          pedigree[pedigree$id %in% c(male, female), 2:3] <-
            pedigree[pedigree$id %in% c(male, female), 2:3][2:1, ]
          pedigree[pedigree$id == female, 6:7] <- NA
        } else {
          pedigree[pedigree$id == male, "female"] <- TRUE
          pedigree[pedigree$id == male, 6:7] <- NA
        }
      }

      desc <- pedigree[pedigree$generation == gen &
                         pedigree$father > 0 &
                         pedigree$mother > 0, ]

      rel <- apply(desc[, 6:7], 1, function(x) ! "x" %in% x)
      pedigree[pedigree$id %in% desc[rel, 1], 6:7] <- NA

      pedigree$phaenotype <- NA

    }

  }

  x$data <- pedigree
  result <- analyse_pedigree(x)

  if (! result$analysis$unique_solution == TRUE ||
      ! result$analysis$inheritance == "XR")
    stop("No solution found.")

  x$inheritance <- "XR"
  x$allele_freq <- NA
  x$time_inh_sim <- Sys.time() - start_time

  x

}
