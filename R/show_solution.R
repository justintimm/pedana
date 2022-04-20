#' Show a solution to an existing pedigree problem
#'
#' [show_solution()] aggregates the results of the analysis of a pedigree
#' problem obtained by [analyse_pedigree()] and can output a halfway or complete
#' solution to the given problem.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree_problem()].
#' @param complete If `TRUE`, show a complete solution.
#'
#' @return Returns a data frame showing a complete or incomplete solution for
#' an existing pedigree.
#'
#' @seealso [verbalise_solution()] to create a text briefly explain a solution
#' to the problem.
#'
#' @export
#' @md
show_solution <- function(x, complete = TRUE) {

  if (! class(x) == "ped")
    stop("Unexpected input. Expects an object of class `ped`.")

  # trim solution --------------------------------------------------------------

  x$analysis$cues$info <- 4 -
    apply(x$analysis$cues[, c("AD", "AR", "XD", "XR")] == "neutral", 1, sum)
  x$analysis$cues <- x$analysis$cues[order(x$analysis$cues$info,
                                           decreasing = T), ]

  if (! complete)
    neutralize <- c(x$inheritance, switch(x$inheritance,
                                          AD = sample(c("AR", "XD"), 1),
                                          AR = sample(c("AD", "XR"), 1),
                                          XD = "AD", XR = "AR"))

  if (max(x$analysis$cues$info) == 4) {
    solution <- x$analysis$cues[1, c(1:3, 8:12)]
    if (! complete) solution[, neutralize] <- "neutral"
  } else if (max(x$analysis$cues$info) == 2 && ! complete) {
    solution <- x$analysis$cues[1, c(1:3, 8:12)]
  } else {
    inheritances <- c("AD", "AR", "XD", "XR")
    if (! complete) inheritances <- setdiff(inheritances, neutralize)
    rel_cues <- sapply(inheritances, function(i)
      min(which(! x$analysis$cues[, i] == "neutral")))
    solution <- x$analysis$cues[unique(unlist(rel_cues)), c(1:3, 8:12)]
  }

  solution[order(solution$id), ]
}
