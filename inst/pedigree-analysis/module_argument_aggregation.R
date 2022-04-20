module_argument_aggregation <- function(id, input) {
  moduleServer(
    id,
    function(input, output, session) {

      arg <- list(id = id,
                  claim = input$claim,
                  proof = input$proof,
                  father = input$father,
                  mother = input$mother,
                  child = input$child,
                  error = NULL)

      if (is.null(input$claim)) arg$error <- "no_claim"
      if (input$proof == "") arg$error <- c(arg$error, "no_proof")

      arg

    }
  )
}
