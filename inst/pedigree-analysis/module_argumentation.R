module_argumentation_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinyjs::hidden(
    div(
      id = ns("argumentation"),
      class = "div-argumentation",
      tagList(
        fluidRow(column(12, align = "right",
                        actionButton(ns("stop"), label = "", icon = icon("home")),
                        shinyjs::disabled(actionButton(ns("edit"), label = "", icon = icon("edit"))),
                        actionButton(ns("add_arg"), label = "", icon = icon("plus")),
                        actionButton(ns("check_arg"), label = "", icon = icon("check")))),
        fluidRow(column(12, align = "center",
                        div(class = "custom_table", tableOutput(ns("feedback"))))),
        div(id = ns("argbox"), style = "float: middle")
      )
    )
  )
}

module_argumentation_server <- function(id, task, lvl, language) {
  moduleServer(
    id,
    function(input, output, session) {

      argumentID <- reactiveVal(0L)
      feedbackID <- reactiveVal(0L)
      analysisID <- reactiveVal(0L)
      arguments <- reactiveValues()
      argumentation <- reactiveValues()
      feedback <- reactiveValues()
      show_feedback <- reactiveVal(NULL)
      lvl_dep_navi <- reactiveVal(NULL)

      observeEvent(task$pedigree, {
        shinyjs::show(id = "argumentation")
      })

      observeEvent(lvl(), {
        ns <- session$ns

        if (lvl() == 1) {
          solution <- show_solution(reactiveValuesToList(task)$pedigree,
                                    complete = TRUE)
          for (i in seq_len(NROW(solution))) {
            argumentID(argumentID() + 1)

            insertUI(selector = "#argumentation-argbox", where = "beforeEnd",
                     ui = module_argument_ui(ns(argumentID()),
                                             value = solution[i, ],
                                             l = language()))

            arguments[[paste0("id:", isolate(argumentID()))]] <-
              list(id = isolate(argumentID()),
                   eval = module_argument_server(argumentID()),
                   fb = NULL)
          }

          shinyjs::delay(500, shinyjs::click("check_arg"))
          shinyjs::delay(550, sapply(c("add_arg", "check_arg"),
                                     function(x) shinyjs::disable(x)))
        } else if (lvl() == 2) {
          solution <- show_solution(reactiveValuesToList(task)$pedigree,
                                    complete = FALSE)
          for (i in seq_len(NROW(solution))) {
            argumentID(argumentID() + 1)

            insertUI(selector = "#argumentation-argbox", where = "beforeEnd",
                     ui = module_argument_ui(ns(argumentID()),
                                             value = solution[i, ],
                                             l = language()))

            arguments[[paste0("id:", isolate(argumentID()))]] <-
              list(id = isolate(argumentID()),
                   eval = module_argument_server(argumentID()),
                   fb = NULL)
          }
          shinyjs::click("add_arg")
          lvl_dep_navi(c("edit", "add_arg", "check_arg"))
        } else if (lvl() == 3) {
          shinyjs::click("add_arg")
          lvl_dep_navi(c("edit", "add_arg", "check_arg"))
        } else if (lvl() == 4) {
          shinyjs::click("add_arg")
          lvl_dep_navi(c("add_arg", "check_arg"))
        }

      })

      observeEvent(input$add_arg, {
        ns <- session$ns
        argumentID(argumentID() + 1)

        insertUI(selector = "#argumentation-argbox", where = "beforeEnd",
                 ui = module_argument_ui(ns(argumentID()), l = language()))

        arguments[[paste0("id:", isolate(argumentID()))]] <-
          list(id = isolate(argumentID()),
               eval = module_argument_server(argumentID()),
               fb = NULL)
      })

      observeEvent(input$check_arg, {
        ns <- session$ns
        feedbackID(feedbackID() + 1)

        elaborated_feedback <- ifelse(lvl() > 3, FALSE, TRUE)

        argumentation[[paste0("id:", isolate(feedbackID()))]] <-
          sapply(rv_to_ordered_list(arguments),
                 function(x) if (x$eval())
                   module_argument_aggregation(x$id, input),
                 simplify = FALSE)

        feedback[["result"]] <-
          module_argumentation_evaluation(feedbackID(),
                                          reactiveValuesToList(task)$pedigree,
                                          rv_to_ordered_list(argumentation),
                                          elaborated_feedback,
                                          language())

        if (! is.null(feedback$result$notes())) {
          module_modal_message(feedbackID(),
                               feedback$result$notes(),
                               language())
        } else {
          analysisID(analysisID() + 1)
          show_feedback(TRUE)
        }

        observeEvent(show_feedback(), {

          show_feedback(NULL)

          # after one revision or full score the edit mode is always blocked
          if (analysisID() > 1 || feedback$result$score() == 4)
            lvl_dep_navi(c("add_arg", "check_arg"))

          sapply(lvl_dep_navi(), function(x) shinyjs::toggleState(x))
          sapply(feedback$result$argumentation(), function(x)
            module_argument_modification(x[["id"]], x, "color"))

          if (lvl() > 1) {
            if (feedback$result$score() == 4) {
              # final feedback, full score
              module_modal_message(feedbackID(), "solution_complete", language())
            } else if (lvl() == 4 || analysisID() > 1) {
              # final feedback, incomplete score, explain correct solution
              module_modal_message(
                feedbackID(), "solution_incomplete_kcr", language(),
                verbalise_solution(reactiveValuesToList(task)$pedigree,
                                   details = ifelse(lvl() == 4, FALSE, TRUE),
                                   l = language()))
            } else {
              # incomplete score, revision possible
              module_modal_message(feedbackID(), "solution_incomplete",
                                   language())
            }
          }

          output$feedback <-
            renderTable({
              ns <- session$ns
              feedback$result$table()
            },
            spacing = "m",
            width = "80%",
            align = "lrc",
            na = "",
            caption = pedana:::tr("fb_table_footer", language()),
            sanitize.text.function = function(x) x)
        })

        # show conditional panel scoring based on reactiveVal
        output$show_scoring <- reactive({
          show_feedback()
        })

      })

      observeEvent(input$edit, {
        output$feedback <- NULL
        sapply(lvl_dep_navi(), function(x) shinyjs::toggleState(x))
        sapply(feedback$result$argumentation(), function(x)
          module_argument_modification(x[["id"]], x, "uncolor"))
      })

      observeEvent(input$stop, {
        refresh <- module_modal_message("refresh", "back_to_start", language())
        observeEvent(refresh(), { session$reload() })
      })

    }
  )
}
