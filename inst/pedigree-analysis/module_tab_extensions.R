module_tab_extensions <- function(id, language) {
  moduleServer(
    id,
    function(input, output, session) {

      insert_my_tab <- function(title, url, value, icon) {
        insertTab("pedana",
                  tabPanel(title = title,
                           includeMarkdown(url),
                           value = value,
                           icon = icon(icon)),
                  target = "start",
                  position = "after",
                  select = FALSE)
      }

      observeEvent(language(), {
        if (is.null(language())) {
          insert_my_tab(title = "Anleitung",
                        url = "www/anleitung.Rmd",
                        value = "anleitung",
                        icon = "chalkboard-teacher")
          insert_my_tab(title = "Manual",
                        url = "www/manual.Rmd",
                        value = "manual",
                        icon = "chalkboard-teacher")

          hideTab(inputId = "pedana", target = "manual")
        } else if (language() == "de") {
          hideTab(inputId = "pedana", target = "manual")
          showTab(inputId = "pedana", target = "anleitung")
        } else if (language() == "en") {
          hideTab(inputId = "pedana", target = "anleitung")
          showTab(inputId = "pedana", target = "manual")
        }
      }, ignoreNULL = FALSE)


    }
  )
}
