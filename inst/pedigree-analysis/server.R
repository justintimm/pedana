source("utils.R")
source("module_language.R")
source("module_start.R")
source("module_task.R")
source("module_argument.R")
source("module_argument_aggregation.R")
source("module_argument_modification.R")
source("module_argumentation.R")
source("module_argumentation_evaluation.R")
source("module_modal_message.R")
source("module_tab_extensions.R")

server <- function(input, output, session) {

  language <- module_language_server("language")

  # documentation of javascript function Shiny.setInputValue
  # https://shiny.posit.co/r/articles/build/communicating-with-js/
  # I don't know why, but the function does not work properly within the language module
  shinyjs::runjs("Shiny.setInputValue('language-browserLanguage', navigator.language);")

  # update language
  # necessary, as the module does not recognise the global inputs
  observeEvent(input$switch_language_de, {
    language("de")
  })

  # update language
  # necessary, as the module does not recognise the global inputs
  observeEvent(input$switch_language_en, {
    language("en")
  })

  request <- module_start_server("start", language)
  task <- module_task_server("task", request, language)
  argumentation <- module_argumentation_server("argumentation", task,
                                               request, language)

  module_tab_extensions("global", language)

  # load js extensions
  output$js_header_extensions <- renderUI({
    if (require("pedanaTracking"))
      pedanaTracking::add_tracking()
  })

  output$js_body_extensions <- renderUI({
    if (require("pedanaTracking"))
      pedanaTracking::add_event_tracking()
  })

  # load tracking extensions
  if (require("pedanaTracking"))
    pedanaTracking::add_tracking_modules("global", task)

  # load ui extensions
  if (require("pedanaUDE"))
    pedanaUDE::add_ude_modules("global", language)

  output$logo <- renderImage({
    list(src = "www/logo.png",
         width = 70,
         height = 81,
         alt = "logo of the R package pedana")
  }, deleteFile = FALSE)

}
