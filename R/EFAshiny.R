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
        if(!require(devtools)) {
                message("installing the 'devtools' package")
                install.packages("devtools")
        }
        if(!require(shiny)) {
                message("installing the 'shiny' package")
                install.packages("shiny")
        }
        if(!require(corrplot)) {
                message("installing the 'corrplot' package")
                install.packages("corrplot")
        }
        if(!require(psych)) {
                message("installing the 'psych' package")
                install.packages("psych")
        }
        if(!require(reshape2)) {
                message("installing the 'reshape2' package")
                install.packages("reshape2")
        }
        if(!require(ggplot2)) {
                message("installing the 'ggplot2' package")
                install.packages("ggplot2")
        }
        if(!require(moments)) {
                message("installing the 'moments' package")
                install.packages("moments")
        }
        if(!require(grid)) {
                message("installing the 'grid' package")
                install.packages("grid")
        }
        if(!require(gridExtra)) {
                message("installing the 'gridExtra' package")
                install.packages("gridExtra")
        }
        if(!require(EFAutilities)) {
                message("installing the 'EFAutilities' package")
                install.packages("EFAutilities")
        }
        if(!require(shinythemes)) {
                message("installing the 'shinythemes' package")
                install.packages("shinythemes")
        }
  shiny::runApp(system.file("efas",package = 'EFAshiny'))
}
