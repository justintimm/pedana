module_argument_modification <- function(id, arg,
                                         action = c("color", "uncolor")) {
  moduleServer(
    id,
    function(input, output, session) {

      if (is.null(arg)) return(NULL)

      # toggle button states ---------------------------------------------------
      toggle <- c("claim", "proof")

      if (input$proof == "const")
        toggle <- c(toggle, "father", "mother", "child")

      div_classes <- c("argument_incorrect",
                       "argument_partially_correct",
                       "argument_correct",
                       "argument_unscored")

      if (action == "color") {

        sapply(toggle, function(x) shinyjs::disable(x))

        class <- switch(as.character(mean(c(arg$conclusion_score,
                                            arg$evidence_score))),
                        "0" = div_classes[1],
                        "1" = div_classes[3],
                        div_classes[2])

        shinyjs::removeCssClass(session$ns("argument-div"),
                                class = div_classes[4], asis = TRUE)
        shinyjs::addCssClass(session$ns("argument-div"),
                             class = class, asis = TRUE)

        output[["argument-feedback"]] <- renderText({ arg$fb })
        output[["icon"]] <- renderUI({
          switch(as.character(mean(c(arg$conclusion_score,
                                     arg$evidence_score))),
                 "1" = HTML("<p><i class=\"fas fa-circle-check icon_scoring\"></i></p>"),
                 "0" = HTML("<p><i class=\"fas fa-circle-xmark icon_scoring\"></i></p>"),
                 HTML("<p><i class=\"fas fa-circle-exclamation icon_scoring\"></i></p>"))

        })

      } else if (action == "uncolor") {

        if (! arg$evidence_score  == 1) {

          if (arg$conclusion_score  == 1) {
            toggle <- toggle[! toggle == "claim"]
          }

          sapply(toggle, function(x) shinyjs::enable(x))

          sapply(div_classes, function(x) shinyjs::removeCssClass(
            session$ns("argument-div"), class = div_classes[1:3], asis = TRUE))
          shinyjs::addCssClass(session$ns("argument-div"),
                               class = div_classes[4], asis = TRUE)
          output[["argument-feedback"]] <- renderText({ NULL })
          output[["icon"]] <- renderUI({ NULL })
        }

      } else if (action == "erase") {

        sapply(div_classes, function(x) shinyjs::removeCssClass(
          session$ns("argument-div"), class = div_classes[1:3], asis = TRUE))
        shinyjs::addCssClass(session$ns("argument-div"),
                             class = div_classes[4], asis = TRUE)
        output[["argument-feedback"]] <- renderText({ NULL })
        output[["icon"]] <- renderUI({ NULL })

      }

    }
  )
}
