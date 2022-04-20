module_argument_ui <- function(id, value = NULL, l = "de") {
  ns <- NS(id)

  # create options for the claims input
  claims <- list()
  moi <- c("AD", "AR", "XD", "XR")

  init <- list(proof =  NULL,
               const = NULL,
               father = 0,
               mother = 0,
               child = 0)

  # extract information to include full argument -------------------------------

  if (! is.null(value)) {

    init$claim <- paste0(value[, c("AD", "AR", "XD", "XR")],
                         names(value[, c("AD", "AR", "XD", "XR")]))

    init$claim <- init$claim[! grepl("neutral", init$claim)]

    if (value$id %in% 101:108) {
      init$proof <- "const"
      init$father <- value$father
      init$mother <- value$mother
      init$child <- value$child
    } else if (value$id == 111) {
      init$proof <- "no_male_to_male"
    } else if (value$id == 113) {
      init$proof <- "carrier_frequency"
    } else if (value$id == 115) {
      init$proof <- "other_arguments"
    } else {
      stop("Critical Error")
    }

  }

  # build a named vector and a named list for selectize inputs -----------------

  claims[[pedana:::tr("claim_confirmed", l)]] <-
    paste0("confirmed", moi)
  names(claims[[pedana:::tr("claim_confirmed", l)]]) <-
    pedana:::tr(paste0("claim_confirmed", moi), l)
  claims[[pedana:::tr("claim_likely", l)]] <-
    paste0("likely", moi)
  names(claims[[pedana:::tr("claim_likely", l)]]) <-
    pedana:::tr(paste0("claim_likely", moi), l)
  claims[[pedana:::tr("claim_unlikely", l)]] <-
    paste0("unlikely", moi)
  names(claims[[pedana:::tr("claim_unlikely", l)]]) <-
    pedana:::tr(paste0("claim_unlikely", moi), l)
  claims[[pedana:::tr("claim_excluded", l)]] <-
    paste0("excluded", moi)
  names(claims[[pedana:::tr("claim_excluded", l)]]) <-
    pedana:::tr(paste0("claim_excluded", moi), l)

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
             "carrier_frequency",
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
                                "proof_carrier_frequency",
                                "proof_other_arguments"), l)

  div(id = ns("argument-container"),
      column(
        4,
        div(
          id = ns("argument-div"),
          class = "argument argument_unscored",
          conditionalPanel(condition = "output.argumentation-show_scoring",
            fluidRow(column(12, div(class = "center", uiOutput(ns("icon")))))),
          fluidRow(column(
            12,
            selectizeInput(
              inputId = ns("claim"),
              pedana:::tr("claim", l),
              claims,
              selected = init$claim,
              multiple = TRUE,
              width = "100%"
            )
          )),
          fluidRow(column(
            12,
            selectizeInput(
              inputId = ns("proof"),
              pedana:::tr("proof", l),
              proof,
              selected = init$proof,
              width = "100%"
            )
          )),
          fluidRow(
            column(4, numericInput(
              inputId = ns("father"),
              pedana:::tr("father", l),
              init$father)),
            column(4, numericInput(
              inputId = ns("mother"),
              pedana:::tr("mother", l),
              init$mother)),
            column(4, numericInput(
              inputId = ns("child"),
              pedana:::tr("child", l),
              init$child))
          ),
          conditionalPanel(condition = "output.argumentation-show_scoring",
                           fluidRow(column(12, div(class = "feedback",
                    htmlOutput(outputId = ns("argument-feedback")))
          ))),
          fluidRow(column(
            12,
            actionButton(
              inputId = ns("remove"),
              label = "",
              icon = icon("times"),
              class = "rm_button",
              width = "100%"
            )
          ))
        )
      ))
}

module_argument_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      status <- reactiveVal(TRUE)

      # toggle state of numeric inputs (active for family constellations only)
      observeEvent(input$proof, {
        sapply(c("father", "mother", "child"),
               function(x) shinyjs::toggleState(x, input$proof == "const"))
      })

      # remove argument
      # open question: which div element to remove? div or container
      observeEvent(input$remove, {
        removeUI(selector = paste0("#argumentation-", id,
                                   "-argument-container"), multiple = TRUE)
        status(FALSE)
      })

      status



    }
  )
}
