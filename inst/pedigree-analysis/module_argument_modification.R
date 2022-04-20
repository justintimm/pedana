module_argument_modification <- function(id, arg,
                                         action = c("color", "uncolor")) {
  moduleServer(
    id,
    function(input, output, session) {

      if (is.null(arg)) return(NULL)

      # toggle button states ---------------------------------------------------
      toggle <- c("remove", "claim", "proof")

      if (input$proof == "const")
        toggle <- c(toggle, "father", "mother", "child")

      sapply(toggle, function(x) shinyjs::toggleState(x))

      div_classes <- c("argument_incorrect",
                       "argument_partially_correct",
                       "argument_correct",
                       "argument_unscored")

      if (action == "color") {

        class <- switch(as.character(mean(arg$score)),
                        "0" = div_classes[1],
                        "1" = div_classes[3],
                        div_classes[2])

        shinyjs::removeCssClass(session$ns("argument-div"),
                                class = div_classes[4], asis = TRUE)
        shinyjs::addCssClass(session$ns("argument-div"),
                             class = class, asis = TRUE)

        output[["argument-feedback"]] <- renderText({ arg$fb })
        output[["icon"]] <- renderUI({
          switch(as.character(mean(arg$score)),
                 "1" = HTML("<i class=\"fas fa-star icon_scoring\"></i>"),
                 "0" = HTML("<i class=\"far fa-star icon_scoring\"></i>"),
                 HTML("<i class=\"fas fa-star-half-alt icon_scoring\"></i>"))
        })

      } else if (action == "uncolor") {

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
