#' An user-friendly shiny application for exploratory factor analysis
#'
#' The EFAshiny graphical user interface is designed to free users from scripting in R by wrapping together various packages for data management, factor analysis, and graphics.
#' @import shiny
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
EFAshiny <- function(run = T, report_Apppath = F) {
        app <- system.file("efas",
                           package = 'EFAshiny')
        if (run == T) {shiny::runApp(app)}
        if (run == F) {
                if (report_Apppath == T){
                        return(app)
                }
                if (report_Apppath == F){
                        onlineapp <- "Have fun with EFAshiny : https://psychilin.shinyapps.io/EFAshiny/"
                        tutorial <- "See tutorial : https://github.com/PsyChiLin/EFAshiny"
                        return(onlineapp)
                }
        }
}
