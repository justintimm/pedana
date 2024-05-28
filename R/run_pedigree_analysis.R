#' Run Shiny application pedana for practicing pedigree analysis
#'
#' [run_pedigree_analysis()] starts the Shiny application `pedana` for
#' practicing pedigree analysis.
#'
#' @param ... Arguments passed on to [shiny::runApp()].
#'
#' @examples
#' \dontrun{run_pedigree_analysis()}
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs toggleState addCssClass removeCssClass
#'   show click delay disable disabled hide hidden onevent
#'
#' @export
#' @md
run_pedigree_analysis <- function(...) {
  app_location <- system.file("pedigree-analysis",
                              package = "pedana")

  shiny::runApp(app_location, ...)
}
