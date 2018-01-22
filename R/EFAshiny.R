#' An user-friendly shiny application for exploratory factor analysis
#'
#' The EFAshiny graphical user interface is designed to free users from scripting in R by wrapping together various packages for data management, factor analysis, and graphics.
#' @import shiny
#' @import devtools
#' @export 
#' @note EFAshiny can directly be used on-line. Check See Also section.
#' @seealso Online version: \url{https://psychilin.shinyapps.io/EFAshiny/} 
#' @seealso Tutorial : \url{https://github.com/PsyChiLin/EFAshiny} 
#' @author Chi-Lin Yu <psychilinyu@gmail.com>
#' @author Ching-Fan Sheu <csheu@mail.ncku.edu.tw>
#' @examples
#' if (interactive()) {
#'   EFAshiny()
#' }
EFAshiny <- function() {
  shiny::runApp(system.file("bn",package = 'EFAshiny'))
}
