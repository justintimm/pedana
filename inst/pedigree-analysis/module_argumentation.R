module_argumentation_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinyjs::hidden(
    div(
      id = ns("argumentation"),
      class = "div-argumentation",
      tagList(
        fluidRow(column(8, align = "left",
                        shinyjs::disabled(actionButton(ns("edit"), label = textOutput(ns("edit_label")), icon = icon("edit"))),
                        actionButton(ns("check_arg"), label = textOutput(ns("check_arg_label")), icon = icon("check")),
                        actionButton(ns("next_task"), label = textOutput(ns("next_task_label")), icon = icon("forward-fast"))),
                 column(4, align = "right",
                        actionButton(ns("stop"), label = textOutput(ns("stop_label")), icon = icon("right-from-bracket")))),
        div(id = ns("argbox"), style = "float: middle")
      )
    )
  )
}

module_argumentation_server <- function(id, task, request, language) {
  moduleServer(
    id,
    function(input, output, session) {

      argumentID <- reactiveVal(0L)
      feedbackID <- reactiveVal(0L)
      analysisID <- reactiveVal(0L)
      taskID <- reactiveVal(0L)

      argument_list <- reactiveValues()
      argumentation <- reactiveValues()
      feedback <- reactiveValues()

      update_argumentation <- reactiveValues(data = NULL,
                                             action = 0L)

      show_feedback <- reactiveVal(NULL)
      next_task <- reactiveVal(NULL)

      observeEvent(task$pedigree, {
        shinyjs::show(id = "argumentation")
      })

      observeEvent(request$lvl, {

        ns <- session$ns

        if (taskID() == 0) {
          # create an argbox for each mode of inheritance
          for (i in c("AD", "AR", "XD", "XR")) {
            argumentID(argumentID() + 1)

            insertUI(selector = "#argumentation-argbox", where = "beforeEnd",
                     ui = module_argument_ui(ns(argumentID()),
                                             moi = i,
                                             value = NULL,
                                             language = reactive(language())))

            module_argument_server(argumentID(),
                                   action = reactive(update_argumentation$action),
                                   data = reactive(update_argumentation$data),
                                   language = reactive(language()))

            argument_list[[paste0("id:", isolate(argumentID()))]] <-
              list(id = isolate(argumentID()),
                   fb = NULL)
          }
        }

        # update argboxes based on the selected level and task
        if (request$lvl == 1) {

          # find solution
          solution <- show_solution(reactiveValuesToList(task)$pedigree)

          # update arguments
          shinyjs::delay(10, update_argumentation$data <- solution)
          shinyjs::delay(20, update_argumentation$action <-
                           update_argumentation$action + 1)

          # switch to feedback mode
          shinyjs::delay(30, shinyjs::click("check_arg"))
          shinyjs::delay(40, shinyjs::disable("check_arg"))

          # show prompt to foster self-explanation
          module_modal_message("prompt", "self_explanation_prompt",
                               language(), taskID = isolate(taskID()))

        } else if (request$lvl == 2) {

          # find solution
          solution <- show_solution(reactiveValuesToList(task)$pedigree,
                                    step = request$step)

          # update arguments
          shinyjs::delay(10, update_argumentation$data <- solution)
          shinyjs::delay(20, update_argumentation$action <-
                           update_argumentation$action + 1)

          # show prompt to foster self-explanation
          module_modal_message("prompt", "task_completion_prompt", language())

        }

      })

      observeEvent(input$check_arg, {
        ns <- session$ns
        feedbackID(feedbackID() + 1)

        elaborated_feedback <- ifelse(analysisID() > 0, TRUE, FALSE)

        argumentation[[paste0("id:", isolate(feedbackID()))]] <-
          sapply(rv_to_ordered_list(argument_list),
                 function(x) module_argument_aggregation(x$id, input),
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

          shinyjs::disable("check_arg")

          sapply(feedback$result$argumentation(), function(x)
            module_argument_modification(x[["id"]], x, "color"))

          if (request$lvl > 1) {
            if (feedback$result$score() == 4) {

              # final feedback, full score
              module_modal_message(feedbackID(), "solution_complete", language())

            } else if (analysisID() == 1) {

              # incomplete score, revision possible
              module_modal_message(feedbackID(), "try_again_feedback",
                                   language())

              # enable edit button
              shinyjs::enable("edit")

            } else if (analysisID() == 2) {

              # incomplete score, revision possible
              module_modal_message(feedbackID(), "elaborated_feedback",
                                   language())

              # enable edit button
              shinyjs::enable("edit")

            } else if (analysisID() == 3) {
              # final feedback, incomplete score, explain correct solution
              module_modal_message(
                feedbackID(), "solution_incomplete_kcr", language(),
                verbalise_solution(reactiveValuesToList(task)$pedigree,
                                   details = ifelse(request$lvl == 4, FALSE, TRUE),
                                   l = language()))
            }
          }

        })

        # show conditional panel scoring based on reactiveVal
        # output$show_scoring <- reactive({
        #   show_feedback()
        # })

      })

      observeEvent(input$edit, {

        show_feedback(NULL)

        # reset checkbutton
        shinyjs::disable("edit")
        shinyjs::enable("check_arg")

        # uncolor arg-boxes
        sapply(feedback$result$argumentation(), function(x)
          module_argument_modification(x[["id"]], x, "uncolor"))

      })

      observeEvent(input$stop, {
        refresh <- module_modal_message("refresh", "back_to_start", language())
        observeEvent(refresh(), {
          session$reload()
        })
      })

      observeEvent(input$next_task, {

        if (is.null(feedback$result)) {
          next_task <- module_modal_message("next", "next_task", language())
        } else if (! is.null(feedback$result$notes())) {
          next_task <- module_modal_message("next", "next_task", language())
        } else if (feedback$result$score() == 4) {
          next_task(TRUE)
        } else {
          next_task <- module_modal_message("next", "next_task", language())
        }

        observeEvent(next_task(), {

          taskID(taskID() + 1)

          # reset trial counter
          analysisID(0L)
          feedbackID(0L)

          new_level <- isolate(request$lvl)
          request$lvl <- 0L

          # check whether to reset from input or feedback mode
          if (! is.null(show_feedback())) {

            # reset checkbutton
            shinyjs::enable("check_arg")

            # uncolor arg-boxes
            sapply(feedback$result$argumentation(), function(x)
              module_argument_modification(x[["id"]], x, "erase"))

            show_feedback(NULL)

            if (feedback$result$score() == 4)
              request$step <- as.numeric(request$step) + 1

            if (new_level == 2 && as.numeric(request$step) > 12)
              new_level <- 3

          }

          # triggers reset of arg-boxes
          update_argumentation$data <- NULL
          update_argumentation$action <- update_argumentation$action + 1L

          feedback$result <- NULL
          argumentation[["id:1"]] <- argumentation[["id:2"]] <- argumentation[["id:3"]] <- NULL

          request$seed <- sample(10000:99999, 1)
          request$lvl <- new_level
        })
      })

      # language adaptivity ----------------------------------------------------

      edit_label <- reactiveVal("")
      check_arg_label <- reactiveVal("")
      next_task_label <- reactiveVal("")
      stop_label <- reactiveVal("")

      observe({
        edit_label(pedana:::tr("edit_label", language()))
        check_arg_label(pedana:::tr("check_arg_label", language()))
        next_task_label(pedana:::tr("next_task_label", language()))
        stop_label(pedana:::tr("stop_label", language()))
      })

      output$edit_label <- renderText({ edit_label() })
      output$check_arg_label <- renderText({ check_arg_label() })
      output$next_task_label <- renderText({ next_task_label() })
      output$stop_label <- renderText({ stop_label() })

    }
  )
}
