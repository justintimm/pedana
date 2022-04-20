#' @importFrom stats pnorm
#' @importFrom utils combn
#' @importFrom kinship2 kinship
analyse_xr_likelihood <- function(x, cues) {
  if (any(cues$AD %in% c("excluded", "unlikely")) &&
      ! any(cues$AR %in% c("confirmed", "likely")) &&
      any(cues$XD %in% c("excluded", "unlikely")) &&
      ! any(cues$XR %in% c("excluded", "unlikely"))) {

    # AR -----------------------------------------------------------------------

    kinship_ar <- kinship2::kinship(convert2kinship2(x))[
      which(x$data$father > 0 & x$data$mother > 0 & x$data$phaenotype == 1),
      which(x$data$father == 0 & x$data$mother == 0 &
              (x$data$generation > 1 | x$data$female == TRUE)),
      drop = FALSE]

    # needs R 4.1
    potential_origins <-
      Map(as.numeric, apply(kinship_ar, 1,
                            function(i) names(which(i > 0)), simplify = FALSE))

    aff_individuals <-
      x$data[x$data$father > 0 & x$data$mother > 0 &
               x$data$phaenotype == 1, c("id")]

    origins <- NULL

    # affected females inherit two alleles
    if (length(aff_individuals) > 0) {
      parents_of_aff_individuals <-
        x$data[x$data$father > 0 & x$data$mother > 0 &
                 x$data$phaenotype == 1, c("father", "mother")]

      origins <-
        apply(parents_of_aff_individuals, 1, function(i)
          ifelse(sum(x$data[x$data$id == i[1], c("father", "mother")] == 0),
                 i[1], i[2]), simplify = FALSE)

      names(origins) <- aff_individuals
    }

    searching <- TRUE
    i <- length(potential_origins)
    while (searching) {
      comb <- t(combn(seq_len(length(potential_origins)), i))
      section <-
        apply(comb, 1, function(x) Reduce(intersect, potential_origins[x]), simplify = FALSE)
      sec_length <- lapply(section, length)
      origins <- append(origins, section[sec_length == 1])

      i <- i - 1
      if (all(sec_length >= 1)) {
        searching <- FALSE

      } else if (i == length(potential_origins) &&
                 length(section) == 1 &&
                 sec_length > 1) {
        origins <- append(origins, paste0(section[[1]], collapse = "|"))
        searching <- FALSE
      }
    }

    carrier_ar <- unique(unlist(origins))

    # eliminate founders_obscurity
    if (any(j <- grepl("\\|", carrier_ar))) {
      fo <- strsplit(carrier_ar[j], "\\|")
      elim <- lapply(fo, function(k)
        any(k %in% x$data[x$data$phaenotype == 1, "id"]) ||
          any(k %in% carrier_ar[! j]))
      if (any(unlist(elim))) {
        carrier_ar <- carrier_ar[- which(carrier_ar ==
                                           carrier_ar[j][unlist(elim)])]
      }
    }

    if (any(carrier_ar %in% x$data[x$data$phaenotype == 1, "id"])) {
      carrier_ar <- carrier_ar[- which(
        carrier_ar %in% x$data[x$data$phaenotype == 1, "id"])]
    }

    # XR -----------------------------------------------------------------------

    kinship_xr <- kinship2::kinship(convert2kinship2(x), chr = "X")[
      which(x$data$father > 0 & x$data$mother > 0 & x$data$phaenotype == 1),
      which(x$data$father == 0 & x$data$mother == 0 &
              (x$data$generation > 1 | x$data$female == TRUE)),
      drop = FALSE]

    kinship_xr <- kinship2::kinship(convert2kinship2(x), chr = "X")[
      which(x$data$father > 0 & x$data$mother > 0 & x$data$phaenotype == 1),
      which(x$data$father == 0 & x$data$mother == 0),
      drop = FALSE]

    # needs R 4.1
    potential_origins <-
      Map(as.numeric, apply(kinship_xr, 1,
                            function(i) names(which(i > 0)), simplify = FALSE))

    aff_females <-
      x$data[x$data$father > 0 & x$data$mother > 0 &
               x$data$phaenotype == 1 & x$data$female == TRUE, c("id")]

    origins <- founding_family <- NULL

    # affected females inherit two alleles
    if (length(aff_females) > 0) {
      parents_of_aff_females <-
        x$data[x$data$father > 0 & x$data$mother > 0 &
                 x$data$phaenotype == 1 & x$data$female == TRUE,
               c("father", "mother")]

      origins <-
        apply(parents_of_aff_females, 1, function(i)
          ifelse(sum(x$data[x$data$id == i[1], c("father", "mother")] == 0),
                 i[1], i[2]), simplify = FALSE)

      names(origins) <- aff_females
    }

    searching <- TRUE
    i <- 1
    while (searching) {
      comb <- t(combn(seq_len(length(potential_origins)), i))
      section <- apply(comb, 1, function(x)
        Reduce(intersect, potential_origins[x]), simplify = FALSE)
      founders_obscurity <- unlist(lapply(section,
                                          function(i) identical(i, c(10, 11))))
      section[founders_obscurity] <- "10|11"

      sec_length <- lapply(section, length)
      origins <- append(origins, section[sec_length == 1])

      if (all(sec_length <= 1)) {
        searching <- FALSE
      } else if (i == length(potential_origins) &&
                 length(section) == 1 &&
                 sec_length > 1) {
        origins <- append(origins, paste0(section[[1]], collapse = "|"))
        searching <- FALSE
      }
      i <- i + 1
    }

    carrier_xr <- unique(unlist(origins))

    # eliminate founders_obscurity
    if (any(j <- grepl("\\|", carrier_xr))) {
      fo <- strsplit(carrier_xr[j], "\\|")
      elim <- lapply(fo, function(k)
        any(k %in% x$data[x$data$phaenotype == 1, "id"]) ||
          any(k %in% carrier_xr[! j]))
      if (any(unlist(elim))) {
        carrier_xr <- carrier_xr[- which(carrier_xr ==
                                           carrier_xr[j][unlist(elim)])]
      }
    }

    # eliminate affected individuals from carier list
    if (any(carrier_xr %in% x$data[x$data$phaenotype == 1, "id"])) {
      carrier_xr <-
        carrier_xr[- which(
          carrier_xr %in% x$data[x$data$phaenotype == 1, "id"])]
    }

    # summary ------------------------------------------------------------------

    ar <- length(carrier_ar)
    xr <- length(carrier_xr)

    # calc z based on Pocock (2006)
    # https://doi.org/10.1136/bmj.332.7552.1256
    z <- abs((ar - xr) / sqrt(ar + xr))
    # calc p from z (one sided)
    p <- stats::pnorm(z, lower.tail = F)
    p

  } else {
    1
  }
}
