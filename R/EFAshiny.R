#' An user-friendly shiny application for exploratory factor analysis.
#'
#' The EFAshiny graphical user interface in shiny (Chang, Cheng, Allaire, Xie, & McPherson, 2017) is designed to free users from scripting in R by wrapping together various packages, such as ggplot2 (Wickham, 2016) and psych (Revelle, 2017) R packages, for data management, factor analysis, and graphics. Easy-to-follow analysis flow and reasonable default settings avoiding common errors (Henson & Roberts, 2006) are provided. Results of analysis in tables and graphs are presented on-line and can be exported.
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
