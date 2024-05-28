#' Check an argument regarding the mode of inheritance
#'
#' [check_argument()] is intended for internal use by the learning app and is
#' called via [check_argumentation()]. Nevertheless, it can also be used outside
#' of the app. Supply an argument to retrieve feedback and a scoring.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree_problem()] function.
#' @param argument List containing one claim plus proof. Each claim
#' needs to consist of one qualifier - confirmed, likely, unlikely,
#' excluded - plus inheritance in the two-letter code - AD, AR, XD, XR - i.e.
#' `excludedAR`. Possible options for proof are: `const`, `few_affected`,
#' `many_affected`, `m_and_f`, `mainly_m`, `mainly_f`, `every_gen`,
#' `not_every_gen`, `no_male_to_male`, `carrier_frequency_low`,
#' `carrier_frequency_high`, `other_arguments`.
#' In addition, the list must also include variables father, mother, and child
#' for specifying a family constellation. If not needed, these
#' variables can be set to `NA`. For consistency reasons, each argument should
#' also be given an `id`, which is used in the app and necessary for
#' [check_argumentation()] to work. See the examples section to see a complete
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
  inheritance <- sub("(confirmed|likely|unlikely|excluded)", "", argument$claim)
  likelihood <- sub("(AD|AR|XD|XR)", "", argument$claim)
  conclusion_score <- evidence_score <- 0
  fb_moi <- fb_sr <- fb_er <- ""

  # 2 check the conclusion to provide kr feedback on the conclusion part -------

  res <- as.character(x$analysis$conclusion[
    match(inheritance, names(x$analysis$conclusion))])

  conclusion_score <- as.numeric(res == likelihood)

  # 3 create solution related feedback -----------------------------------------

  if (conclusion_score == 0) {
    fb_moi <- tr(switch(res,
                       "confirmed" = "fb_moi_confirmed",
                       "unlikely" = "fb_moi_likely",
                       "likely" = "fb_moi_unlikely",
                       "excluded" = "fb_moi_excluded"), l)

    fb_moi <- sprintf(fb_moi, tr(inheritance, l))
  }

  # 4 check the argument to provide feedback -----------------------------------
  # 4.1 provide feedback for superficial arguments -----------------------------
  if (argument$proof %in% c("few_affected", "many_affected")) {
    fb_er <- tr("fb_prevalence", l)
  }

  else if (argument$proof %in% c("every_gen", "not_every_gen")) {
    fb_er <- tr("fb_distribution_pattern", l)
  }

  else if (argument$proof == "other_arguments" &&
      is.null(argument$evidence_score)) {
    return(append(argument, list(fb = NA,
                                 conclusion_score = conclusion_score,
                                 evidence_score = NA)))
  }

  # 4.2 provide feedback for arguments based on strong evidence ----------------
  else if (argument$proof %in% c("const", "other_arguments")) {

    # 4.2.1 provide feedback for constellation based arguments -----------------
    if (argument$proof == "const") {

      fc <- x$analysis$cues[which(
        x$analysis$cues$father == argument$father &
          x$analysis$cues$mother == argument$mother &
          x$analysis$cues$child == argument$child),
        c("id", "AD", "AR", "XD", "XR")]

      if (! NROW(fc) == 1) {
        fb_er <- tr("fb_const_unknown", l)
      } else if (fc$id == 0) {
        fb_er <- tr("fb_const_uninformative", l)
      } else {

        # compare input and solution
        if (fc[, inheritance] == likelihood) {
          evidence_score <- 1
        } else if (likelihood == "unlikely" &&
                   fc[, inheritance] == "excluded") {
          fb_er <- tr("fb_weak_claim_strong_proof_const", l)
          evidence_score <- .5
        } else {
          fb_er <- tr(paste0("fb_const_", fc$id), l)
          fb_er <- sprintf(fb_er, tr(paste0("claim_", res, inheritance), l))
        }
      }
    }

    # provide feedback for arguments based on the entire argumentation ---------
    else if (argument$proof == "other_arguments") {
      if ((likelihood == "unlikely" && res == "excluded") ||
          (likelihood == "likely" && res == "confirmed") ||
          (likelihood == "excluded" && res == "unlikely") ||
          (likelihood == "confirmed" && res == "likely")) {
        fb_er <- tr("fb_other_arguments_claim_proof_mismatch", l)
        evidence_score <- .5
      } else {
        fb_er <- tr("fb_other_arguments", l)
        evidence_score <- argument$evidence_score
      }
    }
  }

  # 4.3 provide feedback for arguments based on weak evidence ------------------
  else if (argument$proof %in% c("mainly_m", "mainly_f",
                             "m_and_f", "no_male_to_male",
                             "carrier_frequency_low",
                             "carrier_frequency_high")) {

    # 4.3.1 feedback a situation where weak proof results in a strong claim ----
    if (likelihood %in% c("confirmed", "excluded")) {
      fb_er <- tr("fb_strong_claim_weak_proof", l)
    }

    # 4.3.2 in depth-feedback: no_male_to_male ---------------------------------
    else if (argument$proof == "no_male_to_male") {
      fb_er <- tr("fb_no_male_to_male", l)
      if (111 %in% x$analysis$cues$id &&
          argument$claim %in% c("likelyXD", "unlikelyAD")) {
        evidence_score <- 1
      }
    }

    # 4.3.3 in depth-feedback: carrier frequency -------------------------------
    else if (argument$proof %in% c("carrier_frequency_low",
                                    "carrier_frequency_high")) {
      fb_er <- tr("fb_carrier_frequency", l)
      if (113 %in% x$analysis$cues$id &&
          ((argument$proof == "carrier_frequency_low" &&
            argument$claim == "likelyXR") ||
           (argument$proof == "carrier_frequency_high" &&
            argument$claim == "unlikelyAR")
          )) {
        evidence_score <- 1
      }
    }

    # 4.3.4 in depth-feedback: mainly_f ----------------------------------------
    else if (argument$proof == "mainly_f") {
      fb_er <- tr("fb_mainly_f", l)
      if (111 %in% x$analysis$cues$id &&
          x$analysis$sex_ratio["females"] > 2 &&
          x$analysis$sex_ratio["ratio"] < .5 &&
          argument$claim %in% c("likelyXD", "unlikelyAD")) {
        evidence_score <- 1
      }
    }

    # 4.3.5 in depth-feedback: mainly_m ----------------------------------------
    else if (argument$proof == "mainly_m") {
      fb_er <- tr("fb_mainly_m", l)
      if (113 %in% x$analysis$cues$id &&
          x$analysis$sex_ratio["males"] > 2 &&
          x$analysis$sex_ratio["ratio"] > 2 &&
          argument$claim %in% c("likelyXR", "unlikelyAR")) {
        evidence_score <- 1
      }
    }

    # 4.3.6 in depth-feedback: m_and_f -----------------------------------------
    else if (argument$proof == "m_and_f") {
      if (argument$claim %in% c("likelyAD", "likelyAR",
                                "unlikelyXD", "unlikelyXR")) {
        fb_er <- tr("fb_gender_ratio_likely_autosomal", l)
      } else if (argument$claim %in% c("unlikelyAD", "unlikelyAR",
                                       "likelyXD", "likelyXR")) {
        fb_er <- tr("fb_gender_ratio_likely_X-linked", l)
      }
    }
  }

  # 5 improve solution related feedback ----------------------------------------

  if (evidence_score == 0) {
    if (res %in% c("excluded", "confirmed")) {
      fb_sr <- tr(switch(res,
                         "excluded" = "fb_moi_excluded_hint",
                         "confirmed" = "fb_moi_confirmed_hint"), l)
      fb_sr <- sprintf(fb_sr, tr(inheritance, l))
    } else if (res %in% c("unlikely", "likely") &&
               inheritance %in% c("AD", "XD")) {
      fb_sr <- tr("fb_moi_likely_XD_hint", l)
    } else if (res %in% c("unlikely", "likely") &&
               inheritance %in% c("AR", "XR")) {
      fb_sr <- tr("fb_moi_likely_XR_hint", l)
    }
  }

  # 6 select kr feedback -------------------------------------------------------

  if (argument$proof %in% c("const", "other_arguments") &&
      conclusion_score == 1 &&
      evidence_score == 1) {
    kr <- tr("fb_conclusion_correct_evidence_correct_const", l)
    fb_er <- fb_moi <- fb_sr <- ""
  } else if (conclusion_score == 1 &&
             evidence_score == 1) {
    kr <- tr("fb_conclusion_correct_evidence_correct_superficial", l)
    fb_er <- fb_moi <- fb_sr <- ""
  } else  if (conclusion_score == 1) {
    kr <- tr("fb_conclusion_correct_evidence_incorrect", l)
  } else  if (evidence_score > 0) {
    kr <- tr("fb_conclusion_incorrect", l)
    fb_moi <- fb_sr <- ""
  } else {
    kr <- tr("fb_conclusion_incorrect", l)
  }

  evidence_score <- floor(evidence_score)

  if (elaborated_feedback) {
    fb <- kr
    if (! fb_er == "")
      fb <- paste(fb, fb_er, sep = "<br/><br/>")

    if (! fb_moi == "" && ! fb_sr == "") {
      fb <- paste0(fb, "<br/>", fb_moi, " ", fb_sr)
    } else if (! fb_moi == "") {
      fb <- paste0(fb, "<br/>", fb_moi)
    } else if (! fb_sr == "") {
      fb <- paste0(fb, "<br/>", fb_sr)
    }
  } else {
    fb <- kr
  }


  argument$fb <- fb
  argument$conclusion_score <- conclusion_score
  argument$evidence_score <- evidence_score

  argument
}
