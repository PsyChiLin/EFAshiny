#' EFAshiny
#'
#' EFAshiny is an user-friendly shiny application for exploratory factor analysis
#' 
#' @import shiny
#' @import devtools
#' @export
#' @seealso \url{https://github.com/PsyChiLin/EFAshiny}
#' @examples
#' if (interactive()) {
#'   EFAshiny()
#' }
EFAshiny <- function() {
  shiny::runApp(system.file("bn",package = 'EFAshiny'))
}
