source("module_language.R")
source("module_start.R")
source("module_task.R")
source("module_argument.R")
source("module_argumentation.R")

ui <- tagList(
  shinyjs::useShinyjs(),
  includeCSS("www/style.css"),
  uiOutput("js_header_extensions"),
  navbarPage(
    title = "pedana",
    id = "global-pedana",
    header = NULL,
    footer =
      div(class = "footer_wrapper", div(class = "footer", fluidRow(
        column(4, paste0("pedana ",
                         packageVersion("pedana"),
                         ", developed by Justin Timm")),
        column(4, div(imageOutput("logo", width = 70, height = 81), align = "center")),
        column(4, a("pedana Sourcecode on GitHub",
                    href = "https://github.com/justintimm/pedana",
                    target = "_blank"), style = "text-align: right;")))),
    collapsible = TRUE,
    tabPanel(
      title = "Start",
      value = "start",
      icon = icon("sitemap"),
      tagList(
        div(class = "center", module_language_ui("language")),
        uiOutput("js_body_extensions"),
        module_start_ui("start"),
        module_task_ui("task"),
        module_argumentation_ui("argumentation")
      )
    )
  )
)
