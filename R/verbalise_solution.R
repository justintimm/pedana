verbalise_proof <- function(x, l) {
  if (x["id"] %in% 101:108) {
    proof <- paste0(tr("proof_constellation", l), " (",
                    tr("father", l), ": ", x["father"], ", ",
                    tr("mother", l), ": ", x["mother"], ", ",
                    tr("child", l), ": ",  x["child"], ").")
  } else if (x["id"] == 111) {
    proof <- paste0(tr("proof_no_male_to_male", l), ".")
  } else if (x["id"] == 113) {
    proof <- paste0(tr("proof_carrier_frequency", l), ".")
  } else if (x["id"] == 115) {
    proof <- paste0(tr("proof_other_arguments", l), ".")
  }

  proof <- paste0(tr("proof", l), ": ", proof)

  proof
}

verbalise_claim <- function(x, l) {
  x <- x[which(names(x) %in% c("AD", "AR", "XD", "XR"))]
  x <- x[which(! x == "neutral")]

  claims_singular <- claims_plural <- NULL

  if (length(x[x == "excluded"]) > 1) {
    claim <- tr("solution_dialog_excluded_plural", l)
    inheritance <- tr(names(x[x == "excluded"]), l)

    if (l == "de")
      inheritance <- paste0(inheritance, "e")

    inheritance <- collapse_list(inheritance, l)
    claims_plural <- sprintf(claim, inheritance)

    x <- x[which(! x == "excluded")]
  }

  if (length(x) > 0) {
    claim <- tr(paste0("solution_dialog_", x), l)
    inheritance <- tr(names(x), l)
    if (l == "de")
      inheritance <- paste0(inheritance, "e")

    claims_singular <- sprintf(claim, inheritance)
  }

  claim <- c(claims_plural, claims_singular)

  claim <- paste(claim, collapse =  " ")

  claim
}

# collapse_list

#' Verbalise a solution to an existing pedigree problem
#'
#' [verbalise_solution()] creates a text briefly explaining a solution
#' to the problem in German or English. You can select whether or not to
#' output evidence to the stated solution.
#'
#' @param x Object of class `ped`, usually created using
#' [sim_pedigree_problem()].
#' @param details If `TRUE`, add evidence to the solution.
#' @param l String to specify the feedback's language.
#' Currently 'de' for German and 'en' for English feedback are supported.
#'
#' @return Returns a string explaining the correct result.
#'
#' @seealso [show_solution()] aggregates the results of the analysis of a
#' pedigree problem as data frame.
#'
#' @export
#' @md
verbalise_solution <- function(x,
                               details = FALSE,
                               l = c("en", "de")) {

  if (! class(x) == "ped")
    stop("Unexpected input. Expects an object of class `ped`.")

  l <- match.arg(l)

  solution <- show_solution(x)

  order <- apply(solution[, c("AD", "AR", "XD", "XR")], 2, function(i)
    if (any(i == "excluded")) {1}
    else if (any(i == "likely")) {2}
    else if (any(i == "unlikely")) {3}
    else {4} )

  solution <- solution[, c("father", "mother", "child", "id",
                           names(sort(order)))]

  solution$claim <- apply(solution, 1, function(i) verbalise_claim(i, l))

  if (details)
    solution$proof <- apply(solution, 1, function(i) verbalise_proof(i, l))

  solution$father <- solution$mother <- solution$child <- solution$id <- NULL
  solution$AD <- solution$AR <- solution$XD <- solution$XR <- NULL

  solution <- apply(solution, 1, function(i) paste(i, collapse = " "))
  solution <- paste(solution, collapse = " ")

  solution
}
