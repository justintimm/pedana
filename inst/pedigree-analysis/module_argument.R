module_argument_ui <- function(id, moi = NULL, value = NULL, language) {
  ns <- NS(id)

  # set initial values for all inputs
  initial_value <- list(claim = NULL,
                        proof =  NULL,
                        father = 0,
                        mother = 0,
                        child = 0)

  # create options for the claim input
  claims <- c("",
              paste0(c("confirmed",
                     "likely",
                     "unlikely",
                     "excluded"), moi))

  names(claims) <- pedana:::tr(c("claim_message",
                                 "claim_confirmed",
                                 "claim_likely",
                                 "claim_unlikely",
                                 "claim_excluded"), language())

  # create options for the evidence input
  proof <- c("",
             "const",
             "few_affected",
             "many_affected",
             "m_and_f",
             "mainly_m",
             "mainly_f",
             "every_gen",
             "not_every_gen",
             "no_male_to_male",
             "carrier_frequency_low",
             "carrier_frequency_high",
             "other_arguments")

  names(proof) <- pedana:::tr(c("proof_message",
                                "proof_constellation",
                                "proof_few_affected",
                                "proof_many_affected",
                                "proof_males_and_females",
                                "proof_mainly_males",
                                "proof_mainly_females",
                                "proof_every_generation",
                                "proof_not_every_generation",
                                "proof_no_male_to_male",
                                "proof_carrier_frequency_low",
                                "proof_carrier_frequency_high",
                                "proof_other_arguments"), language())

  # create arg-box
  div(id = ns("argument-container"),
      column(
        3,
        div(
          id = ns("argument-div"),
          class = "argument argument_unscored",
          conditionalPanel(condition = "output.argumentation-show_scoring",
            fluidRow(column(12, div(class = "center", uiOutput(outputId = ns("icon")))))),
          fluidRow(column(
            12,
            p(htmlOutput(ns("claim_label")), br(), textOutput(ns("claim_text"))),
          )),
          fluidRow(column(
            12,
            selectizeInput(
              inputId = ns("claim"),
              label = NULL,
              choices = claims,
              selected = initial_value$claim,
              multiple = FALSE,
              width = "100%"
            )
          )),
          fluidRow(column(
            12,
            selectizeInput(
              inputId = ns("proof"),
              label = textOutput(ns("proof_label")),
              choices = proof,
              selected = initial_value$proof,
              width = "100%"
            )
          )),
          fluidRow(
            column(4, numericInput(
              inputId = ns("father"),
              label = textOutput(ns("father_label")),
              value = initial_value$father,
              width = "100%"),
              style = "padding-right: 10px;"),
            column(4, numericInput(
              inputId = ns("mother"),
              label = textOutput(ns("mother_label")),
              value = initial_value$mother,
              width = "100%"),
              style = "padding-left: 12.5px; padding-right: 12.5px;"),
            column(4, numericInput(
              inputId = ns("child"),
              label = textOutput(ns("child_label")),
              value = initial_value$child,
              width = "100%"),
              style = "padding-left: 10px;")
          ),
          conditionalPanel(condition = "output.argumentation-show_scoring",
                           fluidRow(column(12, div(class = "feedback",
                    htmlOutput(outputId = ns("argument-feedback")))
          )))
        )
      ))
}

module_argument_server <- function(id, action, data, language) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      moi <- switch(as.character(id),
                    `1` = "AD",
                    `2` = "AR",
                    `3` = "XD",
                    `4` = "XR")

      # toggle state of numeric inputs (active for family constellations only)
      observeEvent(input$proof, {
        sapply(c("father", "mother", "child"),
               function(x)
                 shinyjs::toggleElement(x, condition = input$proof == "const"))
      })

      # reset all inputs when new task is created
      observeEvent(action(), {

        claim <- proof <- character(0L)
        father <- mother <- child <- 0

        if (! is.null(data())) {

          df <- data()
          df <- df[df$moi == moi, ]

          if (! is.na(df$likelihood)) {
            claim <- paste0(df$likelihood, moi)
          }
          if (! is.na(df$id)) {
            proof <- switch(as.character(df$id),
                            `101` = "const",
                            `102` = "const",
                            `103` = "const",
                            `104` = "const",
                            `105` = "const",
                            `106` = "const",
                            `107` = "const",
                            `108` = "const",
                            `111` = "no_male_to_male",
                            `113` = "carrier_frequency",
                            `115` = "other_arguments")

            if (proof == "carrier_frequency" && moi == "XR")
              proof <- "carrier_frequency_low"

            if (proof == "carrier_frequency" && moi == "AR")
              proof <- "carrier_frequency_high"
          }

          if (! is.na(df$father))
            father <- df$father
          if (! is.na(df$mother))
            mother <- df$mother
          if (! is.na(df$child))
            child <- df$child

        }

        shinyjs::enable("claim")
        updateSelectizeInput(session, "claim", selected = claim)
        if (length(claim) > 0) shinyjs::disable("claim")

        shinyjs::enable("proof")
        updateSelectizeInput(session, "proof", selected = proof)
        if (length(proof) > 0) shinyjs::disable("proof")

        shinyjs::enable("father")
        updateNumericInput(session, "father", value = father)
        if (! father == 0) shinyjs::disable("father")

        shinyjs::enable("mother")
        updateNumericInput(session, "mother", value = mother)
        if (! mother == 0) shinyjs::disable("mother")

        shinyjs::enable("child")
        updateNumericInput(session, "child", value = child)
        if (! child == 0) shinyjs::disable("child")

      }, ignoreInit = TRUE)

      claim_label <- reactiveVal("")
      claim_text <- reactiveVal("")
      proof_label <- reactiveVal("")
      father_label <- reactiveVal("")
      mother_label <- reactiveVal("")
      child_label <- reactiveVal("")

      observeEvent(language(), {
        # create options for the claim input
        claims <- c("",
                    paste0(c("confirmed",
                             "likely",
                             "unlikely",
                             "excluded"), moi))

        names(claims) <- pedana:::tr(c("claim_message",
                                       "claim_confirmed",
                                       "claim_likely",
                                       "claim_unlikely",
                                       "claim_excluded"), language())

        # create options for the evidence input
        proof <- c("",
                   "const",
                   "few_affected",
                   "many_affected",
                   "m_and_f",
                   "mainly_m",
                   "mainly_f",
                   "every_gen",
                   "not_every_gen",
                   "no_male_to_male",
                   "carrier_frequency_low",
                   "carrier_frequency_high",
                   "other_arguments")

        names(proof) <- pedana:::tr(c("proof_message",
                                      "proof_constellation",
                                      "proof_few_affected",
                                      "proof_many_affected",
                                      "proof_males_and_females",
                                      "proof_mainly_males",
                                      "proof_mainly_females",
                                      "proof_every_generation",
                                      "proof_not_every_generation",
                                      "proof_no_male_to_male",
                                      "proof_carrier_frequency_low",
                                      "proof_carrier_frequency_high",
                                      "proof_other_arguments"), language())

        # update labels and keep inputs selected during language change
        updateSelectizeInput(session, "claim", choices = claims, selected = input$claim)
        updateSelectizeInput(session, "proof", choices = proof, selected = input$proof)
      })

      observe({
        claim_label(pedana:::tr("claim", language()))
        claim_text(pedana:::tr(paste0("claim_text_", moi), language()))
        proof_label(pedana:::tr("proof", language()))
        father_label(pedana:::tr("father", language()))
        mother_label(pedana:::tr("mother", language()))
        child_label(pedana:::tr("child", language()))
      })

      output$claim_label <- renderText({ claim_label() })
      output$claim_text <- renderText({ claim_text() })
      output$proof_label <- renderText({ proof_label() })
      output$father_label <- renderText({ father_label() })
      output$mother_label <- renderText({ mother_label() })
      output$child_label <- renderText({ child_label() })

    }
  )
}

