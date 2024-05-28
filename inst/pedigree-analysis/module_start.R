module_start_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinyjs::hidden(
    div(
      id = ns("start"),
      class = "div-start",
      tagList(
        fluidRow(column(width = 8, offset = 2, htmlOutput(ns("global_description")))),
        fluidRow(column(width = 8, offset = 2, div(radioButtons(ns("lvl"), label = NULL, inline = TRUE, choices = c("Level 1" = "lvl1", "Level 2" = "lvl2", "Level 3" = "lvl3")), class = "lvl-radiobuttons"))),
        fluidRow(column(width = 8, offset = 2, htmlOutput(ns("lvl_description")))),
        fluidRow(column(width = 4, offset = 2, selectInput(ns("step"), label = textOutput(ns("step_label")), choices = NULL, width = "100%")),
                 column(width = 4, numericInput(ns("seed"), label = textOutput(ns("seed_label")), value = sample(10000:99999, 1), width = "100%"))),
        fluidRow(column(width = 8, offset = 2, align = "right", actionButton(ns("start"), label = textOutput(ns("start_label")))))
      )
    )
  )
}

module_start_server <- function(id, language) {
  moduleServer(
    id,
    function(input, output, session) {

      request <- reactiveValues()
      global_description <- reactiveVal("")
      lvl_description <- reactiveVal("")
      step_label <- reactiveVal("")
      seed_label <- reactiveVal("")
      start_label <- reactiveVal("")

      observeEvent(language(), {
        if (is.null(request$lvl)) {
          shinyjs::show(id = "start")
          # create new seed
          updateNumericInput(session, "seed", value = sample(10000:99999, 1))
        }

        steps <- 1:12
        names(steps) <- paste(pedana:::tr("step_name", language()), 1:12)
        updateSelectInput(session, "step", choices = steps)

        choices <- c("lvl1", "lvl2", "lvl3")
        names(choices) <- c(pedana:::tr("lvl1_label", language()),
                            pedana:::tr("lvl2_label", language()),
                            pedana:::tr("lvl3_label", language()))

        updateRadioButtons(session, "lvl", inline = TRUE, choices = choices, selected = input$lvl)
      })

      observe({
        global_description(pedana:::tr("description_global", language()))
        lvl_description(pedana:::tr(paste0("description_", input$lvl), language()))
        step_label(pedana:::tr("step_label", language()))
        seed_label(pedana:::tr("seed_label", language()))
        start_label(pedana:::tr("start_label", language()))


        # keep step selected during language change
        updateSelectInput(session, "step", selected = input$step)

        # keep input step disabled for levels where it is not applicable
        shinyjs::toggleState("step", condition = input$lvl == "lvl2")

      })

      output$global_description <- renderText({ global_description() })
      output$lvl_description <- renderText({ lvl_description() })
      output$step_label <- renderText({ step_label() })
      output$seed_label <- renderText({ seed_label() })
      output$start_label <- renderText({ start_label() })

      observeEvent(input$start, {
        shinyjs::hide(id = "start")

        num_lvl <- switch(input$lvl,
                          `lvl1` = 1,
                          `lvl2` = 2,
                          `lvl3` = 3)

        request$lvl <- num_lvl
        request$step <- input$step
        request$seed <- input$seed
      })

      request
    }
  )
}
