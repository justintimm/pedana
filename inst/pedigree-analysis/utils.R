#' Convert an rv object to an ordered list
#'
#' [rv_to_ordered_list()] is a wrapper for [shiny::reactiveValuesToList()].
#'
#' @param x The `reactiveValues` object to be ordered. Expects a named
#' [shiny::reactiveValues()] object. Each element needs to be named in the
#' form 'id:value' (e.g. `id:1`, `id:2`, ...) to be sorted by the values.
#' @param isolate If `TRUE`, the reactiveValues object is isolated to avoid
#' dependencies. See [shiny::reactiveValuesToList()] for details.
#'
#' @return Returns an ordered list.
#'
#' [shiny::reactiveValues()] are list-like objects, but sometimes behave
#' differently from conventional lists. For example, object elements do not
#' have a natural order
#' (See \url{https://github.com/rstudio/shiny/issues/2629} for details).
#'
#' @md
rv_to_ordered_list <- function(x, isolate = TRUE) {
  if (isolate) {
    x <- shiny::isolate(shiny::reactiveValuesToList(x))
  } else {
    x <- shiny::reactiveValuesToList(x)
  }

  if (! all(grepl("^id:[[:digit:]]+$", names(x))))
    stop("Unexpected input. Unable to sort the object based on element names. ",
         "Expects a named reactiveValues object. Each element needs to be ",
         "named in the form 'id:value' (e.g. id:1, id:2, ...) to be sorted by ",
         "these values.")

  id <- sub("id:", "", names(x))
  x[order(as.numeric(id))]
}
