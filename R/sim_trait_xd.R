#' Simulate an X-chromosomal dominant trait
#'
#' [sim_trait_xd()] simulates an X-chromosomal dominant trait. This function is
#' faster than [sim_trait()]. For this purpose, however, the simulated pedigree
#' is modified during the simulation. If this is not desired, the function
#' [sim_trait()] should be used instead. Often the simulation is not successful.
#' Use with caution.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree()] function.
#'
#' @return Returns an object of class `ped`.
#'
#' @seealso [sim_trait_xr()] for X-linked recessive traits and the more general
#' function [sim_trait()].
#'
#' @examples
#' set.seed(4)
#' obj <- sim_pedigree(generations = 4, tfr = 3)
#' obj <- sim_trait_xd(obj)
#' plot(obj)
#'
#' @export
#' @md
sim_trait_xd <- function(x) {

  start_time <- Sys.time()

  if (! class(x) == "ped")
    stop("Unexpected input. Expects an object of class `ped`.")

  pedigree <- x$data
  x$complete <- FALSE
  x$stat_xd <- 1

  # distribute alleles among founders in generation 1
  alleles <- sample(c("X", "X", "x"), size = 2,
                    replace = F, prob = c(.25, .25, .5))

  pedigree[pedigree$generation == 1 & pedigree$female == TRUE, 6:7] <-
    c("x", alleles[1])
  pedigree[pedigree$generation == 1 & pedigree$female == FALSE, 6:7] <-
    c("Y", alleles[2])

  # founders in higher generations are not affected at first
  pedigree[pedigree$generation > 1 & pedigree$mother == 0 &
             pedigree$father == 0 & pedigree$female == TRUE,
           6:7] <- data.frame("x", "x")

  pedigree[pedigree$generation > 1 & pedigree$mother == 0 &
             pedigree$father == 0 & pedigree$female == FALSE,
           6:7] <- data.frame("Y", "x")

  # step 1: create a suitable pattern
  i <- 0
  while (x$stat_xd[length(x$stat_xd)] > .05) {

    i <- i + 1

    pedigree <- sim_transmission(pedigree, "XD")
    pedigree <- sim_phaenotype(pedigree, "XD")

    x$data <- pedigree
    analysis <- analyse_pedigree(x)$analysis
    x$complete <- analysis$unique_solution
    x$stat_xd <- c(x$stat_xd, analysis$stat_xd)

    if (i >= 5) {
      if (x$stat_xd[length(x$stat_xd)] == x$stat_xd[length(x$stat_xd) - 5])
        stop("No solution found.")
    }

    if (x$stat_xd[length(x$stat_xd)] > .05) {

      # In which generation is the allele absent for the first time?
      res <- list()
      for (gen_i in max(pedigree$generation):1) {
        desc <- pedigree[pedigree$generation == gen_i &
                           pedigree$father > 0 &
                           pedigree$mother > 0, 6:7]

        res[[gen_i]] <- all(apply(desc, 1, function(x) "X" %in% alleles))
      }

      if (all(res == TRUE)) {
        stop("No solution found.")
      }

      gen <- min(which(unlist(res) == FALSE))
      pedigree[pedigree$generation > gen &
                 pedigree$father > 0 &
                 pedigree$mother > 0, 6:7] <- NA

      desc <- pedigree[pedigree$generation == gen &
                         pedigree$father > 0 &
                         pedigree$mother > 0, ]

      # descendants not bearing the allele of interest
      desc_id <- desc[! apply(desc, 1, function(x) "X" %in% alleles), "id"]
      pedigree[pedigree$id %in% desc_id, 6:7] <- NA
      pedigree$phaenotype <- NA

    }
  }

  # step 2: create recognisable pattern
  conclusion <- which(analyse_pedigree(x)$analysis$conclusion == "neutral")
  if (length(conclusion) > 0) {
    if (any(c(2, 4) %in% conclusion)) {
      gen <- max(pedigree$generation) - 2
      pot_parents <- pedigree[pedigree$generation <= gen &
                              pedigree$female == TRUE &
                              pedigree$phaenotype == 1, "id"]

      # find affected in early generation to add the following situation
      parent <- sample(pot_parents, 1)

      # does the selected person already have children?
      if (any(apply(pedigree[, 2:3], 1, function(x) parent %in% x))) {
        new_parents <- unlist(pedigree[pedigree$father == parent |
                                         pedigree$mother == parent,
                                       c(2, 3, 5)][1, ])
      } else {
        parent <- pedigree[pedigree$id == parent, ]
        partner <- data.frame(id = max(pedigree$id) + 1,
                              father = 0,
                              mother = 0,
                              female = ! parent$female,
                              generation = parent$generation,
                              paternal.allele = NA,
                              maternal.allele = NA,
                              phaenotype = 0,
                              stringsAsFactors = FALSE)
        pedigree <- rbind(pedigree, partner)

        if (isTRUE(parent$female)) {
          new_parents <- c(partner$id, parent$id, parent$generation)
        } else {
          new_parents <- c(parent$id, partner$id, parent$generation)
        }
      }

      # add unaffected male with affected parents to exclude AR, XR
      new_fam <- data.frame(id = max(pedigree$id) + c(1:3),
                            father = c(new_parents[1],
                                       new_parents[1],
                                       max(pedigree$id) + 1),
                            mother = c(new_parents[2],
                                       new_parents[2],
                                       max(pedigree$id) + 2),
                            female = c(FALSE, TRUE, FALSE),
                            generation = c(new_parents[3] + 1,
                                           new_parents[3] + 1,
                                           new_parents[3] + 2),
                            paternal.allele = NA,
                            maternal.allele = NA,
                            phaenotype = c(1, 1, 0),
                            stringsAsFactors = FALSE)

      # only one parent should have ancestors in the family
      new_fam[sample(1:2, 1), 2:3] <- 0

      pedigree <- rbind(pedigree, new_fam)


    }
  }

  x$data <- pedigree
  result <- analyse_pedigree(x)

  if (! result$analysis$unique_solution == TRUE ||
      ! result$analysis$inheritance == "XD")
    stop("No solution found.")

  x$inheritance <- "XD"
  x$allele_freq <- NA
  x$time_inh_sim <- Sys.time() - start_time

  x
}
