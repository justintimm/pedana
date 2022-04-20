module_modal_message <- function(id, notes, l = "de", solution = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      continue <- reactiveVal(NULL)

      note <- notes[1]

      size <- switch(note,
                     "solution_incomplete_kcr" = "m",
                     "s")

      type <- switch(note,
                     "no_claim" = "error",
                     "no_proof" = "error",
                     "no_argument" = "error",
                     "solution_incomplete" = "message",
                     "solution_incomplete_kcr" = "message",
                     "soluion_complete" = "message",
                     "back_to_start" = "choice",
                     "error")

      message <- switch(note,
                        "no_claim" = "dialog_argument_incomplete_message",
                        "no_proof" = "dialog_argument_incomplete_message",
                        "no_argument" = "dialog_solution_empty_message",
                        "solution_incomplete" = "dialog_solution_incomplete_message",
                        "solution_incomplete_kcr" = "dialog_solution_incomplete_kcr_message",
                        "solution_complete" = "dialog_solution_complete_message",
                        "back_to_start" = "dialog_app_refresh_message",
                        "unknown_error")

      title <- switch(note,
                      "no_claim" = "dialog_argument_incomplete_title",
                      "no_proof" = "dialog_argument_incomplete_title",
                      "no_argument" = "dialog_solution_empty_title",
                      "solution_incomplete" = "dialog_solution_incomplete_title",
                      "solution_incomplete_kcr" = "dialog_solution_incomplete_kcr_title",
                      "solution_complete" = "dialog_solution_complete_title",
                      "back_to_start" = "dialog_app_refresh_title",
                      "unknown_error")

      message <- pedana:::tr(message, l)
      title <- pedana:::tr(title, l)

      if (note == "solution_incomplete_kcr")
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
