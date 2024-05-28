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

      observeEvent(input$browserLanguage, {
        shinyjs::hide(id = id)
        l <- substr(input$browserLanguage, 1, 2)
        if (l %in% c("de", "en")) {
          shinyjs::hide(id = id)
          language(l)
        }
      })

      observeEvent(input$de, {
        shinyjs::hide(id = id)
        language("de")
      })
      observeEvent(input$en, {
        shinyjs::hide(id = id)
        language("en")
      })

      language
    }
  )
}
