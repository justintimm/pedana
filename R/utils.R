#' @export
print.ped <- function(x, ...) {
  cat("Simulated Pedigree\n")
  cat("\nSettings:")
  cat("\nSimulated with       ", x$system$pkg, paste(x$system$pkg_version))
  cat("\nR version            ", paste(x$system$R_version))
  cat("\n\nStatistics:")
  cat("\nSeed                 ", ifelse(is.null(x$seed), "NULL", x$seed))
  cat("\nGenerations          ", x$gen)
  cat("\nFamily members       ", x$family_members)
  cat(" (")
  cat(sum(x$data$female == T), "females, ")
  cat(sum(x$data$female == F), "males)")

  if (! is.null(x$inheritance)) {
    cat("\nInheritance          ", x$inheritance)

    if (x$inheritance == "XD")
      cat("\nAD sig. less likely  ", "p =",
          round(x$stat_xd[length(x$stat_xd)], 3))

    if (x$inheritance == "XR")
      cat("\nAR sig. less likely  ", "p =",
          round(x$stat_xr[length(x$stat_xr)], 3))
  }

  cat("\n\nElapsed Time:")
  t1 <- round(as.numeric(x$time_ped_gen, units = "secs"), digits = 3)
  cat("\nPedigree generation  ", t1, "seconds")
  if (! is.null(x$time_prob_gen)) {
    t3 <- round(as.numeric(x$time_prob_gen, units = "secs"), digits = 3)
    cat("\nProblem sim. (total) ", t3, "seconds")
  } else  if (! is.null(x$inheritance)) {
    t2 <- round(as.numeric(x$time_inh_sim, units = "secs"), digits = 3)
    cat("\nInheritance sim.     ", t2, "seconds")
  }

  invisible(x)
}

#' @export
summary.ped <- function(object, ...) {
  plot(object)
  print(object)
}

convert2kinship2 <- function(x, ...) {
  affected <- ifelse(is.na(x$data$phaenotype), 0, x$data$phaenotype)
  object <- kinship2::pedigree(x$data$id,
                               x$data$father,
                               x$data$mother,
                               x$data$female,
                               affected = affected,
                               famid = rep(1, NROW(x$data)))[1]
  object
}

status <- function(x = NULL) {
  factor(x, levels = c("confirmed",
                       "likely",
                       "neutral",
                       "unlikely",
                       "excluded"), ordered = TRUE)
}

collapse_list <- function(x, l = "en") {
  if (l == "en") collapse <- c(" and ", ", and ")
  if (l == "de") collapse <- c(" und ", " und ")

  if (length(x) == 2) {
    x <- paste(x, collapse = collapse[1])
  } else  if (length(x) > 2) {
    x <- paste(c(paste(x[seq_len(length(x) - 1)], collapse = ", "),
                 x[length(x)]),
               collapse = collapse[2])
  }
  x
}

tr <- function(x, l = "en") {
  i <- sapply(x, function(j) which(txt == j))
  x <- txt[i, l]
  x
}
