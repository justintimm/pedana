module_start_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinyjs::hidden(
    div(
      id = ns("start"),
      class = "div-start",
      tagList(
        fluidRow(
          column(width = 3, offset = 3, actionButton(ns("lvl1"), label = div(icon("star"), br(), "Level 1"), width = "100%", class = "lvl-button")),
          column(width = 3, actionButton(ns("lvl2"), label = div(icon("star"), icon("star"), br(), "Level 2"), width = "100%", class = "lvl-button"))),
        fluidRow(
          column(width = 3, offset = 3, actionButton(ns("lvl3"), label = div(icon("star"), icon("star"), icon("star"), br(), "Level 3"), width = "100%", class = "lvl-button")),
          column(width = 3, actionButton(ns("lvl4"), label = div(icon("star"), icon("star"), icon("star"), icon("star"), br(), "Level 4"), width = "100%", class = "lvl-button"))),
        fluidRow(
          column(width = 8, offset = 2, htmlOutput(ns("lvl_description")))))
    )
  )
}

module_start_server <- function(id, language) {
  moduleServer(
    id,
    function(input, output, session) {

      lvl <- reactiveVal(NULL)
      lvl_description <- reactiveVal("")

      observeEvent(language(), {
        shinyjs::show(id = "start")
      })

      shinyjs::onevent("hover", "lvl1", lvl_description(
        pedana:::tr("description_lvl1", language())))
      shinyjs::onevent("hover", "lvl2", lvl_description(
        pedana:::tr("description_lvl2", language())))
      shinyjs::onevent("hover", "lvl3", lvl_description(
        pedana:::tr("description_lvl3", language())))
      shinyjs::onevent("hover", "lvl4", lvl_description(
        pedana:::tr("description_lvl4", language())))


      output$lvl_description <- renderText({ lvl_description() })

      observeEvent(input$lvl1, {
        # shinyjs::hide(id = "start")
        # lvl(1)
      })
      observeEvent(input$lvl2, {
        # shinyjs::hide(id = "start")
        # lvl(2)
      })
      observeEvent(input$lvl3, {
        shinyjs::hide(id = "start")
        lvl(3)
      })
      observeEvent(input$lvl4, {
        # shinyjs::hide(id = "start")
        # lvl(4)
      })

      lvl
    }
  )
}
