#' Check an argument regarding the mode of inheritance
#'
#' [check_argument()] is intended for internal use by the learning app and is
#' called via [check_argumentation()]. Nevertheless, it can also be used outside
#' of the app. Supply an argument to retrieve feedback and a scoring.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree_problem()] function.
#' @param argument List containing at least one claim plus proof. Each claim
#' needs to consist of one qualifier - confirmed, likely, neutral, unlikely,
#' excluded - plus inheritance in the two-letter code - AD, AR, XD, XR - i.e.
#' `excludedAR`. Possible options for proof are: `const`, `few_affected`,
#' `many_affected`, `m_and_f`, `mainly_m`, `mainly_f`, `every_gen`,
#' `not_every_gen`, `no_male_to_male`, `carrier_frequency`, `other_arguments`.
#' In addition, the list must also include variables father, mother, and child
#' for specifying a family constellation. If not needed, these
#' variables can be set to `NA`. For consistency reasons, each argument should
#' also be given an `id`, which is used in the app and necessary for
#' [check_argumentation()] to work.See the examples section to see a complete
#' argument.
#' @param elaborated_feedback If `FALSE`, a verbal feedback about the
#' correctness of the result is generated, but a detailed explanation is
#' omitted.
#' @param l String to specify the feedback's language.
#' Currently `de` for German and `en` for English feedback are supported.
#'
#' @return Returns the argument including feedback and a scoring as list.
#'
#' @seealso [check_argumentation()], which evaluates a full argumentation
#' consisting of any number of arguments.
#'
#' @examples
#' obj1 <- sim_pedigree_problem(inheritance = "AD", seed = 1)
#' argument <- list(id = 1,
#'                  claim = "excludedAR",
#'                  proof = "const",
#'                  father = 12,
#'                  mother = 13,
#'                  child = 14)
#'
#' check_argument(obj1, argument)
#'
#' @export
#' @md
check_argument <- function(x,
                           argument,
                           elaborated_feedback = TRUE,
                           l = c("en", "de")) {

  if (! inherits(x, "ped"))
    stop("Unexpected input. Expects an object of class `ped`.")

  l <- match.arg(l)

  # 1 rearrange data -----------------------------------------------------------
  exp <- data.frame(argument)
  exp$inheritance <-
    sub("(confirmed|likely|neutral|unlikely|excluded)", "", argument$claim)
  exp$likelihood <- sub("(AD|AR|XD|XR)", "", argument$claim)
  exp$id <- seq(NROW(exp))
  fb <- ""
  score <- rep(0, NROW(exp))

  # 2 check the argument to provide feedback -----------------------------------
  # 2.1 provide feedback for constellation based arguments ---------------------

  if (exp[1, "proof"] == "const") {

    fc <- x$analysis$cues[which(
      x$analysis$cues$father == exp[1, "father"] &
        x$analysis$cues$mother == exp[1, "mother"] &
        x$analysis$cues$child == exp[1, "child"]),
      c("id", "AD", "AR", "XD", "XR")]

    if (! NROW(fc) == 1) {
      fb <- tr("fb_const_unknown", l)
    } else if (fc$id == 0) {
      fb <- tr("fb_const_uninformative", l)
    } else {
      fb <- tr(paste0("fb_const_", fc$id), l)

      # compare input and solution
      fc <- fc[, c("AD", "AR", "XD", "XR")]
      res <- merge(exp[, c("id", "inheritance", "likelihood")],
                   data.frame("inheritance" = colnames(fc),
                              "likelihood" = unlist(fc)),
                   by = "inheritance")
      res$correct <- res[, "likelihood.x"] == res[, "likelihood.y"]
      res <- res[order(res$id), c("inheritance", "correct")]

      score <- ifelse(res$correct, 1, 0)

    }
  }

  # 2.2 provide feedback for superficial arguments -----------------------------
  if (exp[1, "proof"] %in% c("few_affected", "many_affected")) {
    fb <- tr("fb_prevalence", l)
  }

  if (exp[1, "proof"] %in% c("every_gen", "not_every_gen")) {
    fb <- tr("fb_distribution_pattern", l)
  }

  if (exp[1, "proof"] == "other_arguments") {
    return(append(argument, list(fb = NA, score = NA)))
  }

  # 2.3 provide feedback for arguments based on weak evidence ------------------
  if (exp[1, "proof"] %in% c("mainly_m", "mainly_f",
                             "m_and_f", "no_male_to_male",
                             "carrier_frequency")) {

    # 2.3.1 feedback a situation where weak proof results in a strong claim ----
    if (any(exp[, "likelihood"] %in% c("confirmed", "excluded"))) {
      fb <- tr("fb_strong_claim_weak_proof", l)
    } else {

      # 2.3.2 in depth-feedback: no_male_to_male -------------------------------
      if (exp[1, "proof"] == "no_male_to_male") {
        fb <- tr("fb_no_male_to_male", l)
        if (111 %in% x$analysis$cues$id) {
          score[exp$claim %in% c("likelyXD", "unlikelyAD")] <- 1
        }
      }

      # 2.3.3 in depth-feedback: carrier frequency -----------------------------
      else if (exp[1, "proof"] == "carrier_frequency") {
        fb <- tr("fb_carrier_frequency", l)
        if (all(x$analysis$conclusion[c("AR", "XR")] %in%
                c("confirmed", "excluded"))) {
          fb <- tr("fb_carrier_frequency_inapplicable", l)
        } else if (113 %in% x$analysis$cues$id) {
          score[exp$claim %in% c("unlikelyAR", "likelyXR")] <- 1
        }
      }

      # 2.3.4 in depth-feedback: mainly_f --------------------------------------
      else if (exp[1, "proof"] == "mainly_f") {
        fb <- tr("fb_mainly_f", l)
        if (111 %in% x$analysis$cues$id &&
            x$analysis$sex_ratio["females"] > 2 &&
            x$analysis$sex_ratio["ratio"] < .5) {
          score[exp$claim %in% c("likelyXD", "unlikelyAD")] <- 1
        }
      }

      # 2.3.5 in depth-feedback: mainly_m --------------------------------------
      else if (exp[1, "proof"] == "mainly_m") {
        fb <- tr("fb_mainly_m", l)
        if (113 %in% x$analysis$cues$id &&
            x$analysis$sex_ratio["males"] > 2 &&
            x$analysis$sex_ratio["ratio"] > 2) {
          score[exp$claim %in% c("likelyXR", "unlikelyAR")] <- 1
        }
      }

      # 2.3.5 in depth-feedback: m_and_f ---------------------------------------
      else if (exp[1, "proof"] %in% "m_and_f") {
        fb <- tr("fb_gender_ratio", l)
      }

    }
  }

  if (exp[1, "proof"] == "const" && all(score == 1)) {
    kr <- tr("fb_const_correct", l)
  } else if (all(score == 1)) {
    kr <- tr("fb_superficial_correct", l)
  } else if (any(score == 1)) {
    kr <- tr("fb_partially_correct", l)
  } else {
    kr <- tr("fb_incorrect", l)
  }

  fb <- ifelse(elaborated_feedback, paste(kr, fb), kr)

  append(argument, list(fb = fb, score = score))
}
