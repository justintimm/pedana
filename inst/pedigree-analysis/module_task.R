module_task_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinyjs::hidden(
    div(
      id = ns("task"),
      class = "div_task",
      tagList(
        fluidRow(column(width = 12, htmlOutput(ns("task_title")))),
        fluidRow(column(width = 12, htmlOutput(ns("task_text")))),
        fluidRow(column(width = 12, plotOutput(ns("pedigree"))))
      )
    )
  )
}

module_task_server <- function(id, request, language) {
  moduleServer(
    id,
    function(input, output, session) {

      task <- reactiveValues()

      observeEvent(request$lvl, {

        showModal(modalDialog(
          HTML(pedana:::tr("dialog_loading_pedigree_message", language())),
          title = pedana:::tr("dialog_loading_pedigree_title", language()),
          size = "s", footer = NULL))

        if (request$lvl == 2) {
          inheritance <- switch(request$step,
                                `1` = c("AD", "AR"),
                                `2` = c("AD", "AR"),
                                `3` = c("AD", "AR"),
                                `4` = c("AD", "AR"),
                                `5` = c("AD", "AR"),
                                `6` = c("AD", "AR"),
                                `7` = "XR",
                                `8` = "XR",
                                `9` = "XD",
                                `10` = "XD",
                                `11` = c("AR", "XR"),
                                `12` = c("AD", "XD"))
        } else {
          inheritance <- c("AD", "AR", "XD", "XR")
        }

        set.seed(request$seed)
        inheritance <- sample(inheritance, 1)

        pedigree <- sim_pedigree_problem(inheritance = inheritance,
                                         generations = 4,
                                         force = TRUE,
                                         seed = request$seed)
        task$pedigree <- analyse_pedigree(pedigree)

        output$task_title <- renderText({
          ns <- session$ns

          title <- paste("<h4>",
                         pedana:::tr("task_title", language()),
                         "•",
                         pedana:::tr(paste0("lvl", request$lvl, "_label"),
                                     language()))

          if (request$lvl == 2)
            title <- paste(title,
                           "•",
                           pedana:::tr("step_name", language()),
                           request$step)

          paste(title, "</h4>")

        })

        output$task_text <- renderText({
          ns <- session$ns
          pedana:::tr("task_text", language())
        })

        output$pedigree <- renderPlot({
          old_par <- par(mar = c(2, 1, 1, 1),
                         oma = c(2, 0, 0, 0),
                         no.readonly = T)
          ns <- session$ns
          plot(task$pedigree, cex = .8, align = c(4, 2))
          mtext(paste("pedana Pedigree:", request$seed),
                side = 1, adj = 1, padj = 0, outer = TRUE, cex = .5)
          par(old_par)
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
