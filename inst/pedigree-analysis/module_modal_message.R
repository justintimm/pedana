module_modal_message <- function(id, notes, l = "de", solution = NULL, taskID = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      continue <- reactiveVal(NULL)

      note <- notes[1]

      size <- switch(note,
                     "solution_incomplete_kcr" = "m",
                     "self_explanation_prompt" = "m",
                     "s")

      type <- switch(note,
                     "no_claim" = "error",
                     "no_proof" = "error",
                     "self_explanation_prompt" = "message",
                     "task_completion_prompt" = "message",
                     "try_again_feedback" = "message",
                     "elaborated_feedback" = "message",
                     "solution_incomplete_kcr" = "message",
                     "soluion_complete" = "message",
                     "next_task" = "choice",
                     "back_to_start" = "choice",
                     "error")

      note <- switch(note,
                     "no_claim" = "dialog_argument_incomplete",
                     "no_proof" = "dialog_argument_incomplete",
                     "self_explanation_prompt" = "dialog_self_explanation_prompt",
                     "task_completion_prompt" = "dialog_task_completion_prompt",
                     "try_again_feedback" = "dialog_solution_incomplete_try_again",
                     "elaborated_feedback" = "dialog_solution_incomplete_elaborated_feedback",
                     "solution_incomplete_kcr" = "dialog_solution_incomplete_kcr",
                     "solution_complete" = "dialog_solution_complete",
                     "back_to_start" = "dialog_app_refresh",
                     "next_task" = "dialog_next_task",
                     "unknown_error")

      if (note == "dialog_self_explanation_prompt") {
        message <- pedana:::tr(paste0("dialog_self_explanation_prompt_message_", (taskID %% 3) + 1), l)
      } else {
        message <- pedana:::tr(paste0(note, "_message"), l)
      }

      title <- pedana:::tr(paste0(note, "_title"), l)

      if (note == "dialog_solution_incomplete_kcr")
        message <- sprintf(message, solution)

      if (type == "choice") {

        showModal(
          modalDialog(HTML(message),
                      title = title,
                      size = size,
                      footer = tagList(
                        modalButton(pedana:::tr("dialog_cancel", l)),
                        actionButton(ns("dialog_ok"),
                                     pedana:::tr("dialog_ok", l)))))

      } else if (type %in% c("error", "message")) {

        showModal(
          modalDialog(HTML(message),
                      title = title,
                      size = size,
                      footer = modalButton(pedana:::tr("dialog_ok", l)),
                      easyClose = TRUE))
      }

      observeEvent(input$dialog_ok, {
        removeModal()
        continue(TRUE)
      }, ignoreInit = T)

      continue

    }
  )
}
