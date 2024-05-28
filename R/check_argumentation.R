#' Check an entire argumentation regarding the mode of inheritance
#'
#' [check_argumentation()] is intended for internal use by the learning app.
#' Nevertheless, it can also be used outside of the app. Supply an argumentation
#' for evaluation. Provides feedback and a table with an overall summary of the
#' argumentation.
#'
#' @param x Object of class `ped`, usually created using the
#' [sim_pedigree_problem()] function.
#' @param argumentation List of individual arguments. See
#' [check_argument()] for details.
#' @param elaborated_feedback Set to `TRUE` to return comprehensive feedback on
#' the content.
#' @param l String to specify the feedback's language.
#' Currently 'de' for German and 'en' for English feedback are supported.
#'
#' @return Returns a list containing the argumentation including feedback and a
#' scoring per argument and a table which provides an overview of the
#' argumentation.
#'
#' @seealso [check_argument()], which evaluates an individual argument.
#'
#' @examples
#' obj1 <- sim_pedigree_problem(inheritance = "AD", seed = 1)
#' argument1 <- list(id = 1,
#'                   claim = "excludedAR",
#'                   proof = "const",
#'                   father = 12,
#'                   mother = 13,
#'                   child = 14)
#'
#' argument2 <- list(id = 2,
#'                   claim = "excludedXR",
#'                   proof = "const",
#'                   father = 12,
#'                   mother = 13,
#'                   child = 14)
#'
#' argumentation <- list('arg-1' = argument1,
#'                       'arg-2' = argument2)
#'
#' check_argumentation(obj1, argumentation)
#'
#' @export
#' @md
check_argumentation <- function(x,
                                argumentation,
                                elaborated_feedback = TRUE,
                                l = c("en", "de")) {

  if (! inherits(x, "ped"))
    stop("Unexpected input. Expects an object of class `ped`.")

  l <- match.arg(l)

  argumentation <- lapply(argumentation, function(i)
    check_argument(x, i[1:6], elaborated_feedback, l))

  df <- lapply(argumentation, function(x)
    data.frame(claim = x$claim,
               conclusion_score = x$conclusion_score,
               evidence_score = x$evidence_score))

  df <- do.call("rbind", df)
  df$inheritance <-
    sub("(confirmed|likely|neutral|unlikely|excluded)", "", df$claim)
  df$likelihood <- sub("(AD|AR|XD|XR)", "", df$claim)

  # additional modes of inheritance can be ruled out ---------------------------
  res <- merge(df[, c("inheritance", "likelihood",
                      "conclusion_score", "evidence_score")],
               data.frame("inheritance" = names(x$analysis$conclusion),
                          "likelihood" = as.character(x$analysis$conclusion)),
               by = "inheritance",
               all = TRUE,
               suffixes = c("_input", "_solution"))

  res <- res[! duplicated(res$inheritance), ]

  # set missing scores to zero -------------------------------------------------
  res[is.na(res$likelihood_input), c("conclusion_score", "evidence_score")] <- 0

  # check arguments based on the other arguments -------------------------------

  other_arg_as_evidence <-
    names(which(sapply(argumentation, function(i) NA %in% i$evidence_score)))

  if (length(other_arg_as_evidence) > 0) {

    arg_claim <- sapply(argumentation, function(x) x$claim)
    arg_claim <- arg_claim[names(arg_claim) %in% other_arg_as_evidence]

    arg_inheritance<- sub("(confirmed|likely|neutral|unlikely|excluded)", "",
                          arg_claim)
    arg_likelihood <- sub("(AD|AR|XD|XR)", "", arg_claim)

    if (length(other_arg_as_evidence) == 1 &&
        sum(res$evidence_score, na.rm = T) == 3 &&
        length(other_arg_as_evidence) == 1 &&
        length(arg_inheritance) == 1 &&
        length(arg_likelihood) == 1 &&
        argumentation[[other_arg_as_evidence]][["conclusion_score"]] == 1) {

      argumentation[[other_arg_as_evidence]][["evidence_score"]] <- 1
      res$evidence_score[is.na(res$evidence_score)] <- 1

      argumentation[[other_arg_as_evidence]] <-
        check_argument(x, argumentation[[other_arg_as_evidence]],
                       elaborated_feedback, l)

    } else {
      # multiple explanations could refer to 'other arguments'
      # create individual feedback for each
      for (i in other_arg_as_evidence) {

        argumentation[[i]][["evidence_score"]] <- 0
        res$evidence_score[is.na(res$evidence_score)] <- 0

        argumentation[[i]] <- check_argument(x, argumentation[[i]],
                                             elaborated_feedback, l)

      }
    }
  }

  list(argumentation = argumentation,
       table = res)
}
