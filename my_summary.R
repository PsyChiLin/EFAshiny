my_summary <- function(x) {
        funs <- c(mean, sd, skewness, kurtosis)
        sapply(funs, function(f)f(x, na.rm = TRUE))
}