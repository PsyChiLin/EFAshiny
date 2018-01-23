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
        if(!require(ggplot2)) {require(ggplot2)}
        if(!require(psych)) {require(psych)}
        if(!require(EFAutilities)) {require(EFAutilities)}
        if(!require(corrplot)) {require(corrplot)}
        if(!require(reshape2)) {require(reshape2)}
        if(!require(moments)) {require(moments)}
        if(!require(gridExtra)) {require(gridExtra)}
        if(!require(qgraph)) {require(qgraph)}
        if(!require(bootnet)) {require(bootnet)}
        if(!require(igraph)) {require(igraph)}
        shiny::runApp(system.file("efas",package = 'EFAshiny'))
}
