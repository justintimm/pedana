module_task_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinyjs::hidden(
    div(
      id = ns("task"),
      class = "div_task",
      tagList(
        htmlOutput(ns("task_title")),
        htmlOutput(ns("task_text")),
        plotOutput(ns("pedigree"), height = "500px")
      )
    )
  )
}

module_task_server <- function(id, lvl, language) {
  moduleServer(
    id,
    function(input, output, session) {

      task <- reactiveValues()

      observeEvent(lvl(), {

        showModal(modalDialog(
          HTML(pedana:::tr("dialog_loading_pedigree_message", language())),
          title = pedana:::tr("dialog_loading_pedigree_title", language()),
          size = "s", footer = NULL))

        # inheritance <- sample(c("AD", "AR", "XD", "XR"),
        #                       size = 1,
        #                       prob = c(1/4, 1/4, 1/4, 1/4))
        #
        # pedigree <- sim_pedigree_problem(inheritance = inheritance,
        #                                  generations = 4,
        #                                  force = TRUE,
        #                                  seed = sample(100:299, 1))

        if(static_task_id == 1) {
          pedigree <- sim_pedigree_problem(inheritance = "AD",
                                           generations = 4,
                                           seed = 1)
        } else {
          pedigree <- sim_pedigree_problem(inheritance = "AR",
                                           generations = 4,
                                           seed = 1)
        }

        task$pedigree <- analyse_pedigree(pedigree)

        output$task_title <- renderText({
          ns <- session$ns
          pedana:::tr("task_title", language())
        })

        output$task_text <- renderText({
          ns <- session$ns
          pedana:::tr("task_text", language())
        })

        output$pedigree <- renderPlot({
          ns <- session$ns
          plot(task$pedigree, cex = .8)
        },
        res = 120,
        execOnResize = TRUE
        )

        removeModal()

        shinyjs::show(id = "task")

      }, ignoreInit = TRUE)

      task
    }
  )
}
