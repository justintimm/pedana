#' Show a solution to an existing pedigree problem
#'
#' [show_solution()] aggregates the results of the analysis of a pedigree
#' problem obtained by [analyse_pedigree()] and can output a halfway or complete
#' solution to the given problem.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree_problem()].
#' @param step If `NULL`, the function returns a complete solution.
#' When set to numeric values between 1 and 12, the function returns incomplete
#' solutions relevant for fading worked out solutions. For steps 1 to 6 the
#' function expects a pedigree representing an autosomal mode of inheritance.
#' Steps 7 and 8 only work for X-linked recessive pedigrees. Steps 9 and 10
#' refer to X-linked dominant pedigrees. Step 11 only works with recessive,
#' step 12 only with dominant modes of inheritance.
#'
#' @return Returns a data frame showing a complete or incomplete solution for
#' an existing pedigree.
#'
#' @seealso [verbalise_solution()] to create a text briefly explain a solution
#' to the problem.
#'
#' @importFrom stats reshape
#'
#' @export
#' @md
show_solution <- function(x, step = NULL) {

  if (! inherits(x, "ped"))
    stop("Unexpected input. Expects an object of class `ped`.")

  if (! is.null(step)) {
    if ((step %in% c(1:6) && ! x$analysis$inheritance %in% c("AD", "AR")) ||
        (step %in% c(7:8) && ! x$analysis$inheritance == "XR") ||
        (step %in% c(9:10) && ! x$analysis$inheritance == "XD") ||
        (step == 11 && ! x$analysis$inheritance %in% c("AR", "XR")) ||
        (step == 12 && ! x$analysis$inheritance %in% c("AD", "XD")))
      stop("Step not defined for this mode of inheritance")
  }

  # trim solution --------------------------------------------------------------

  x$analysis$cues$info <- 4 -
    apply(x$analysis$cues[, c("AD", "AR", "XD", "XR")] == "neutral", 1, sum)
  x$analysis$cues <- x$analysis$cues[order(x$analysis$cues$info,
                                           decreasing = T), ]

  if (max(x$analysis$cues$info) == 4) {
    solution <- x$analysis$cues[1, c(1:3, 8:12)]
  } else {
    inheritances <- c("AD", "AR", "XD", "XR")
    rel_cues <- sapply(inheritances, function(i)
      min(which(! x$analysis$cues[, i] == "neutral")))
    solution <- x$analysis$cues[unique(unlist(rel_cues)), c(1:3, 8:12)]
  }

  solution <- reshape(solution,
                      direction = "long",
                      varying = c("AD", "AR", "XD", "XR"),
                      v.names = "likelihood",
                      timevar = "moi",
                      times = c("AD", "AR", "XD", "XR"))

  solution <- solution[! solution$likelihood == "neutral", ]

  attributes(solution) <- list("row.names" = c("AD", "AR", "XD", "XR"),
                               "class" = "data.frame",
                               names = c("mother", "father", "child",
                                         "id", "moi", "likelihood"))

  if (is.null(step)) {
    return(solution)
  } else if (step == 1) {
    solution$likelihood[solution$likelihood == "confirmed"] <- NA
  } else if (step == 2) {
    solution[solution$likelihood == "confirmed",
             c("mother", "father", "child", "id", "likelihood")] <- NA
  } else if (step == 3) {
    solution[solution$likelihood == "confirmed",
             c("mother", "father", "child", "id", "likelihood")] <- NA
    solution$likelihood[1:2] <- NA
  } else if (step == 4) {
    solution[solution$moi %in% c("AD", "AR"),
             c("mother", "father", "child", "id", "likelihood")] <- NA
  } else if (step == 5) {
    two_excluded_moi <- sample(which(solution$likelihood == "excluded"), 2)
    solution$likelihood[two_excluded_moi] <- NA
  } else if (step == 6) {
    two_excluded_moi <- sample(which(solution$likelihood == "excluded"), 2)
    solution[two_excluded_moi,
             c("mother", "father", "child", "id", "likelihood")] <- NA
  } else if (step %in% c(7, 9)) {
    solution$likelihood[solution$likelihood %in% c("likely", "unlikely")] <- NA
  } else if (step %in% c(8, 10)) {
    solution[solution$likelihood %in% c("likely", "unlikely"),
             c("mother", "father", "child", "id", "likelihood")] <- NA
  } else if (step == 11) {
    solution[solution$moi %in% c("AR", "XR"),
             c("mother", "father", "child", "id", "likelihood")] <- NA
  } else if (step == 12) {
    solution[solution$moi %in% c("AD", "XD"),
             c("mother", "father", "child", "id", "likelihood")] <- NA
  }

  solution
}
