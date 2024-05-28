#' Analyse a pedigree problem
#'
#' [analyse_pedigree()] analyses a simulated pedigree problem to identify the
#' present mode of inheritance. Pedigrees created using [sim_pedigree_problem()]
#' are analysed during simulation. Therefore, calling [analyse_pedigree()] by
#' hand is usually not required.
#'
#' @param x Object of class `ped`, usually created using the individual
#' functions [sim_pedigree()] and [sim_trait()], [sim_trait_xd()], or
#' [sim_trait_xr()].
#'
#' @return Returns an object of class `ped`.
#'
#' The following list of constellations and characteristics is
#' monitored and represents the basic framework of the analysis.
#' * Affected female with unaffected parents: excludes AD, XD, XR
#' * Affected male with unaffected parents: excludes AD, XD
#' * Unaffected female with affected parents: excludes AR, XD, XR
#' * Unaffected male with affected parents: excludes AR, XR
#' * Affected male with unaffected mother: excludes XD
#' * Unaffected female with affected father: excludes XD
#' * Affected female with unaffected father: excludes XR
#' * Unaffected  male with affected mother: excludes XR
#' * All daughters, but no sons of affected males are affected as well:
#'    likely XD, unlikely AD
#' * The number of independent alleles required differs noticeably for
#'      the two recessive modes of inheritance: likely XR, unlikely AR
#'
#' The linkage of gender and affection status among children of affected
#' fathers is checked by Fisher's exact test. A significant relationship
#' indicates that an X-linked dominant inheritance is more likely than an
#' autosomal dominant inheritance.
#'
#' Whether the number of independent alleles required for the two recessive
#' modes of inheritance differs significantly enough in favor of
#' X-linked recessive inheritance is approximated using a simple
#' statistical test described by Pocock (2006).
#'
#' @references
#'
#' Pocock, S. J. (2006). The simplest statistical test: how to check for a
#' difference between treatments. \emph{BMJ, 332}, 1256-1258.
#' \url{https://doi.org/10.1136/bmj.332.7552.1256}
#'
#' @seealso [sim_trait()] and related functions [sim_trait_xd()] and
#' [sim_trait_xr()] to simulate the distribution of a trait for a pedigree
#' simulated using [sim_pedigree()].
#'
#' @examples
#' set.seed(7)
#' obj1 <- sim_pedigree()
#' obj1 <- sim_trait(obj1, inheritance = "AR")
#' obj1 <- analyse_pedigree(obj1)
#' obj1$analysis
#'
#' obj2 <- sim_pedigree_problem(seed = 13)
#' obj2 <- analyse_pedigree(obj2)
#' obj2$analysis
#'
#' @export
#' @md
analyse_pedigree <- function(x) {

  if (! inherits(x, "ped"))
    stop("Unexpected input. Expects an object of class `ped`.")

  unique_solution <- FALSE
  conclusion <- inheritance <- NA

  affected <- x$data$id
  children_of_affected <- x$data[x$data$father %in% affected |
                                        x$data$mother %in% affected, "id"]
  if (length(affected) == 0) {
    x[["analysis"]] <- list(unique_solution = unique_solution)
    return(x)
  }

  inf_const <- lapply(c(affected, children_of_affected), function(id)
    data.frame(
      father = x$data[x$data$id == id, "father"],
      mother = x$data[x$data$id == id, "mother"],
      child = id,
      female_child = x$data[x$data$id == id, "female"],
      phaenotype_child = x$data[x$data$id == id, "phaenotype"],
      stringsAsFactors = FALSE))
  inf_const <- do.call(rbind, inf_const)
  inf_const <- inf_const[! duplicated(inf_const) &
                           inf_const$father > 0 &
                           inf_const$mother > 0, ]
  inf_const <- merge(inf_const,
                     x$data[, c("id", "phaenotype")],
                     by.x = "father",
                     by.y = "id")
  inf_const <- merge(inf_const,
                     x$data[, c("id", "phaenotype")],
                     by.x = "mother",
                     by.y = "id",
                     suffixes = c("_father", "_mother"))

  cues <- cbind(inf_const,
                do.call(rbind, apply(inf_const, 1, analyse_fam_const)))

  # the value may still need to be adjusted
  stat_xd <- analyse_xd_likelihood(cues, inf_const)
  if (stat_xd < .05) {
    cues <- rbind(cues, data.frame(father = NA,
                                   mother = NA,
                                   child = NA,
                                   female_child = NA,
                                   phaenotype_child = NA,
                                   phaenotype_father  = NA,
                                   phaenotype_mother = NA,
                                   id = 111,
                                   AD = "unlikely",
                                   AR = "neutral",
                                   XD = "likely",
                                   XR = "neutral"))
  }

  # the value may still need to be adjusted
  stat_xr <- analyse_xr_likelihood(x, cues)
  if (stat_xr < .1) {
    cues <- rbind(cues, data.frame(father = NA,
                                   mother = NA,
                                   child = NA,
                                   female_child = NA,
                                   phaenotype_child = NA,
                                   phaenotype_father  = NA,
                                   phaenotype_mother = NA,
                                   id = 113,
                                   AD = "neutral",
                                   AR = "unlikely",
                                   XD = "neutral",
                                   XR = "likely"))
  }

  conclusion <- apply(cues[, c("AD", "AR", "XD", "XR")],
                      2, function(x) max(status(x)))
  if (table(conclusion)["excluded"] == 3) {
    conclusion[which(conclusion == "neutral")] <- qualifier <- "confirmed"

    unique_solution <- TRUE
  } else if (sum(table(conclusion)["excluded"],
                 table(conclusion)["unlikely"]) == 3) {
    conclusion[which(conclusion == "neutral")] <- qualifier <- "likely"
    unique_solution <- TRUE
  }

  all_neutral <- apply(cues[, c("AD", "AR", "XD", "XR")],
                       2, function(x) all(x == "neutral"))

  if (unique_solution && any(all_neutral)) {
    cues <- rbind(cues, data.frame(father = NA,
                                   mother = NA,
                                   child = NA,
                                   female_child = NA,
                                   phaenotype_child = NA,
                                   phaenotype_father  = NA,
                                   phaenotype_mother = NA,
                                   id = 115,
                                   AD = "neutral",
                                   AR = "neutral",
                                   XD = "neutral",
                                   XR = "neutral"))

    cues[NROW(cues), names(which(all_neutral))] <- qualifier
  }

  if (unique_solution) {
    inheritance <-
      names(conclusion)[which(conclusion %in% c("confirmed", "likely"))]
  }

  sex_ratio <- c(length(x$data[x$data$female == FALSE &
                                      x$data$phaenotype == 1, "id"]),
                 length(x$data[x$data$female == TRUE &
                                      x$data$phaenotype == 1, "id"]))
  sex_ratio <- c(sex_ratio, sex_ratio[1] / sex_ratio[2])
  names(sex_ratio) <- c("males", "females", "ratio")

  x[["analysis"]] <- list(unique_solution = unique_solution,
                          inheritance = inheritance,
                          conclusion = conclusion,
                          cues = cues,
                          sex_ratio = sex_ratio,
                          stat_xd = stat_xd,
                          stat_xr = stat_xr)

  x
}
