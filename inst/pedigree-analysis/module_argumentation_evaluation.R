module_argumentation_evaluation <- function(id, pedigree, argumentation,
                                            elaborated_feedback, l = "de") {
  moduleServer(
    id,
    function(input, output, session) {

      notes <- result <- NULL

      argumentation <- argumentation[[id]]

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

        score <- sum(result$table$evidence_score)

      }

      list(notes = reactive(notes),
           argumentation = reactive(result$argumentation),
           score = reactive(score))

    }
  )
}
