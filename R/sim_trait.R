sim_transmission <- function(pedigree, inheritance) {
  while (anyNA(pedigree$paternal.allele) ||
         anyNA(pedigree$maternal.allele)) {
    for (i in which(is.na(pedigree$paternal.allele) |
                    is.na(pedigree$maternal.allele))) {
      mother <- pedigree[i, "mother"]
      father <- pedigree[i, "father"]
      cols <- c("paternal.allele", "maternal.allele")
      pedigree[i, "maternal.allele"] <-
        sample(pedigree[pedigree$id == mother, cols], 1)
      if (inheritance %in% c("AD", "AR")) {
        pedigree[i, "paternal.allele"] <-
          sample(pedigree[pedigree$id == father, cols], 1)
      } else {
        if (isFALSE(pedigree[i, "female"])) pedigree[i, cols[1]] <-
            pedigree[pedigree$id == father, cols[1]]
        if (isTRUE(pedigree[i, "female"])) pedigree[i, cols[1]] <-
            pedigree[pedigree$id == father, cols[2]]
      }
    }
  }
  pedigree
}

sim_genotype <- function(pedigree,
                         inheritance,
                         founder,
                         allele_freq) {

  a <- switch(inheritance,
              "AD" = c("a", "A"),
              "AR" = c("A", "a"),
              "XD" = c("x", "X"),
              "XR" = c("X", "x"))

  # Pedigrees are usually recorded only if there are relevant ancestors.
  # Therefore, allele carriers are statistically overrepresented in the
  # first generation. The factor indicates how much more likely the allele
  # is to occur in that generation.
  factor <- switch(inheritance,
                   "AD" = 10,
                   "AR" = 1,
                   "XD" = 2,
                   "XR" = 100)

  p1 <- c(1 - allele_freq, allele_freq)
  p2 <- c(1 - allele_freq / factor, allele_freq / factor)

  f1 <- founder[founder < 12]
  f2 <- founder[founder > 11]

  cols <- c("paternal.allele", "maternal.allele")
  if (length(f1) > 0) {
    g1 <- replicate(length(f1), sample(a, size = 2, replace = T, prob = p1))
    pedigree[pedigree$id %in% f1, cols] <- t(g1)
  }
  if (length(f2) > 0) {
    g2 <- replicate(length(f2), sample(a, size = 2, replace = T, prob = p2))
    pedigree[pedigree$id %in% f2, cols] <- t(g2)
  }
  if (inheritance %in% c("XD", "XR")) {
    id_m <- pedigree[pedigree$id %in% founder & pedigree$female == F, "id"]
    pedigree[pedigree$id %in% id_m, cols[1]] <- "Y"
  }

  pedigree

}

sim_phaenotype <- function(pedigree, inheritance) {
  cols <- c("paternal.allele", "maternal.allele")
  if (inheritance == "AD") pedigree$phaenotype <-
      apply(pedigree[, cols], 1, function(x) ifelse(any(x == "A"), 1, 0))
  if (inheritance == "AR") pedigree$phaenotype <-
      apply(pedigree[, cols], 1, function(x) ifelse(any(x == "A"), 0, 1))
  if (inheritance == "XD") pedigree$phaenotype <-
      apply(pedigree[, cols], 1, function(x) ifelse(any(x == "X"), 1, 0))
  if (inheritance == "XR") pedigree$phaenotype <-
      apply(pedigree[, cols], 1, function(x) ifelse(any(x == "X"), 0, 1))
  pedigree
}

#' Simulate an inherited trait
#'
#' [sim_trait()] simulates an inherited trait for one of the typical modes of
#' inheritance: autosomal dominant, autosomal recessive, X-linked dominant, or
#' X-linked recessive. Often the simulation is not successful, especially for
#' the X-linked modes of inheritance. Use with caution.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree()] function.
#' @param inheritance Inheritance of the trait to be simulated.
#' @param min_affected Specifies the minimum number of affected individuals to
#' be simulated.
#' @param allele_freq A positive numeric value. With a higher allele frequency,
#' the chance increases that several independent alleles will be introduced
#' into the pedigree.
#'
#' @return Returns an object of class `ped`.
#'
#' @seealso [sim_trait_xd()] for X-linked dominant traits and
#' [sim_trait_xr()] for X-linked recessive traits.
#'
#' @examples
#' set.seed(4)
#' obj <- sim_pedigree(generations = 3, tfr = 2)
#' obj <- sim_trait(obj)
#' plot(obj)
#'
#' @export
#' @md
sim_trait <- function(x,
                      inheritance = c("AD", "AR", "XD", "XR"),
                      min_affected = 3,
                      allele_freq = .01) {

  start_time <- Sys.time()

  inheritance <- match.arg(inheritance)

  if (! class(x) == "ped")
    stop("Unexpected input. Expects an object of class `ped`.")

  if (! is.numeric(allele_freq) || ! allele_freq < 1 || ! allele_freq > 0)
    stop("`allele_freq` must be a positive numeric between 0 and 1.")

  pedigree <- x$data

  # identify people for whom genotypes are simulated
  avail_founder <- pedigree[pedigree$father == 0 & pedigree$mother == 0, "id"]

  unique_solution <- FALSE
  x$stat_xd <- x$stat_xr <- NULL
  cols <- c("paternal.allele", "maternal.allele")

  mutant_allele <- switch(inheritance,
                          "AD" = "A",
                          "AR" = "a",
                          "XD" = "X",
                          "XR" = "x")

  while (isFALSE(unique_solution)) {

    carrier <- pedigree[pedigree$father == 0 & pedigree$mother == 0 &
                          (identical(pedigree$paternal.allele, mutant_allele) |
                           identical(pedigree$maternal.allele, mutant_allele)),
                        "id"]

    avail_founder <- setdiff(avail_founder, carrier)

    if (length(avail_founder) == 0 ||
        (inheritance == "AD" && allele_freq >= .3) ||
        (allele_freq >= .6)) {
      stop("No solution found.")
    }

    pedigree[pedigree$id %in% avail_founder |
               (pedigree$father > 0 & pedigree$mother > 0), cols] <- NA
    pedigree$phaenotype <- NA

    pedigree <- sim_genotype(pedigree, inheritance, avail_founder, allele_freq)
    pedigree <- sim_transmission(pedigree, inheritance)
    pedigree <- sim_phaenotype(pedigree, inheritance)
    x$data <- pedigree

    result <- analyse_pedigree(x)
    if (sum(x$data$phaenotype) >= min_affected &&
        result$analysis$unique_solution == TRUE &&
        result$analysis$inheritance == inheritance) {
      unique_solution <- TRUE
    } else {
      allele_freq <- allele_freq + .05
    }

    if (inheritance == "XD") x$stat_xd <- c(x$stat_xd, result$analysis$stat_xd)
    if (inheritance == "XR") x$stat_xr <- c(x$stat_xr, result$analysis$stat_xr)
  }

  x$data <- pedigree
  x$inheritance <- inheritance
  x$allele_freq <- allele_freq
  x$complete <- unique_solution
  x$time_inh_sim <- Sys.time() - start_time

  x
}
