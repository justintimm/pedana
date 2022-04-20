module_argumentation_evaluation <- function(id, pedigree, argumentation,
                                            elaborated_feedback, l = "de") {
  moduleServer(
    id,
    function(input, output, session) {

      notes <- result <- NULL

      argumentation <- argumentation[[length(argumentation)]]

      # check if there have ever been arguments, or if any are still active
      if (length(argumentation) == 0 || is.null(unlist(argumentation))) {

        # define error message
        notes <- ("no_argument")

      } else {

        # check for incomplete arguments
        notes <- unique(unlist(lapply(argumentation, function(x) x$error)))

      }

      if (is.null(notes)) {

        # skip empty arguments
        argumentation[sapply(argumentation, is.null)] <- NULL
        result <- check_argumentation(pedigree, argumentation,
                                      elaborated_feedback, l)

        result$table$result <- ""
        result$table$result <-
          ifelse(result$table$confirmation_conflict == TRUE,
                 paste0(result$table$result,
                        pedana:::tr("fb_table_conflict", l)),
                 result$table$result)
        result$table$result <-
          ifelse(result$table$unique_statements > 1,
                 paste0(result$table$result,
                        pedana:::tr("fb_table_ambiguous", l)),
                 result$table$result)
        result$table$result <-
          ifelse(is.na(result$table$likelihood_input),
                 paste0(result$table$result,
                        pedana:::tr("fb_table_question", l)),
                 result$table$result)
        result$table$result <-
          ifelse(result$table$result == "",
                 ifelse(result$table$score == 1,
                        pedana:::tr("fb_table_correct", l),
                        pedana:::tr("fb_table_incorrect", l)),
                 result$table$result)

        score <- sum(result$table$score)

        result$table <-
          result$table[, c("inheritance", "likelihood_input", "result")]

        result$table$likelihood_input[is.na(result$table$likelihood_input)] <-
          "blank"

        result$table$inheritance <-
          pedana:::tr(result$table$inheritance, l)
        result$table$likelihood_input <-
          pedana:::tr(paste0("claim_", result$table$likelihood_input), l)

        colnames(result$table) <-
          pedana:::tr(c("fb_table_colnames_mode_of_inheritance",
                        "fb_table_colnames_input",
                        "fb_table_colnames_feedback"), l)

      }

      list(notes = reactive(notes),
           argumentation = reactive(result$argumentation),
           table = reactive(result$table),
           score = reactive(score))

    }
  )
}
