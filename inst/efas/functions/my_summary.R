my_summary <- function(x) {
        funs <- c(mean, sd, skewness, kurtosis, median, mad)
        sapply(funs, function(f)f(x, na.rm = TRUE))
}