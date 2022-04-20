module_language_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("language"),
    class = "div-language",
    tagList(
      h4("Wähle deine Sprache."),
      h4("Choose your language.", style = "font-style: italic;"),
      actionButton(ns("de"), label = "Deutsch", class = "language-button"),
      actionButton(ns("en"), label = "English", class = "language-button")
    )
  )
}

module_language_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      language <- reactiveVal(NULL)
      observeEvent(input$de, {
        shinyjs::hide(id = "language")
        language("de")
      })
      observeEvent(input$en, {
        shinyjs::hide(id = "language")
        language("en")
      })

      language
    }
  )
}
