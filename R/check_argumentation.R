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

  if (! class(x) == "ped")
    stop("Unexpected input. Expects an object of class `ped`.")

  l <- match.arg(l)

  argumentation <- lapply(argumentation, function(i)
    check_argument(x, i[1:6], elaborated_feedback, l))

  df <- lapply(argumentation, function(x) data.frame(claim = x$claim,
                                                     score = x$score))

  df <- do.call("rbind", df)
  df$inheritance <-
    sub("(confirmed|likely|neutral|unlikely|excluded)", "", df$claim)
  df$likelihood <- sub("(AD|AR|XD|XR)", "", df$claim)

  # two or more modes of inheritance were labeled as confirmed or likely -------
  tab <- table(df[df$likelihood %in% c("confirmed", "likely"), "inheritance"])
  confirmation_conflict <- data.frame("inheritance" = c("AD", "AR", "XD", "XR"),
                                      "confirmation_conflict" = FALSE)
  if (length(tab) > 1) {
    confirmation_conflict[confirmation_conflict$inheritance %in% names(tab),
                          "confirmation_conflict"] <- TRUE
  }

  # two statements regarding one mode of inheritance contradict each other -----
  unique_sm <- c(length(unique(df[df$inheritance == "AD", "likelihood"])),
                 length(unique(df[df$inheritance == "AR", "likelihood"])),
                 length(unique(df[df$inheritance == "XD", "likelihood"])),
                 length(unique(df[df$inheritance == "XR", "likelihood"])))

  # additional modes of inheritance can be ruled out ---------------------------
  res <- merge(df[, c("inheritance", "likelihood", "score")],
               data.frame("inheritance" = names(x$analysis$conclusion),
                          "likelihood" = as.character(x$analysis$conclusion),
                          "unique_statements" = unique_sm),
               by = "inheritance",
               all = TRUE,
               suffixes = c("_input", "_solution"))
  res <- merge(res,
               confirmation_conflict,
               by = "inheritance",
               all = TRUE)

  res <- res[! duplicated(res$inheritance), ]

  # set missing scores to zero -------------------------------------------------
  res[res$confirmation_conflict, "score"] <- 0
  res[res$unique_statements > 1, "score"] <- 0
  res[is.na(res$likelihood_input), "score"] <- 0

  # check arguments based on the other arguments -------------------------------

  other_arg_as_evidence <-
    names(which(sapply(argumentation, function(i) NA %in% i$score)))

  if (length(other_arg_as_evidence) > 0) {
    arg_inheritance <- sub("(confirmed|likely|neutral|unlikely|excluded)", "",
                           argumentation[[other_arg_as_evidence]][["claim"]])
    arg_likelihood <- sub("(AD|AR|XD|XR)", "",
                          argumentation[[other_arg_as_evidence]][["claim"]])

    if (sum(res$score, na.rm = T) == 3 &&
        length(other_arg_as_evidence) == 1 &&
        length(arg_inheritance) == 1 &&
        length(arg_likelihood) == 1 &&
        res[res$inheritance == arg_inheritance,
            "likelihood_solution"] == arg_likelihood) {
      argumentation[[other_arg_as_evidence]][["fb"]] <-
        tr("fb_const_correct", l)
      argumentation[[other_arg_as_evidence]][["score"]] <- 1
      res$score[is.na(res$score)] <- 1
    } else {
      argumentation[[other_arg_as_evidence]][["fb"]] <-
        tr("fb_incorrect", l)
      argumentation[[other_arg_as_evidence]][["score"]] <- 0
      res$score[is.na(res$score)] <- 0
    }
  }

  list(argumentation = argumentation,
       table = res)
}
