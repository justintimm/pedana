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

if (file.exists("ude")) source("ude/module_ude.R")

server <- function(input, output, session) {

  language <- module_language_server("language")
  lvl <- module_start_server("start", language)
  task <- module_task_server("task", lvl, language)
  argumentation <- module_argumentation_server("argumentation", task,
                                               lvl, language)

  if (file.exists("ude")) module_ude("global", language)

  module_tab_extensions("global", language)

  output$logo <- renderImage({
    list(src = "www/logo.png",
         width = 70,
         height = 81,
         alt = "logo of the R package pedana")
  }, deleteFile = FALSE)

}
