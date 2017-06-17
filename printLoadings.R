printLoadings <- function (x, digits = 2, cutoff = 0, sort = T, ...) {
        Lambda <- unclass(x)
        p <- nrow(Lambda)
        factors <- ncol(Lambda)
        if (sort) {
                mx <- max.col(abs(Lambda))
                ind <- cbind(1L:p, mx)
                mx[abs(Lambda[ind]) < 0.5] <- factors + 1
                Lambda <- Lambda[order(mx, 1L:p), ]
        }
        cat("\nLoadings:\n")
        fx <- format(round(Lambda, digits))
        names(fx) <- NULL
        nc <- nchar(fx[1L], type = "c")
        fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
        newx <- print(fx, quote = FALSE, ...) # I assigned this to a variable
        vx <- colSums(x^2)
        varex <- rbind(`SS loadings` = vx)
        if (is.null(attr(x, "covariance"))) {
                varex <- rbind(varex, `Proportion Var` = vx/p)
                if (factors > 1) 
                        varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
        }
        cat("\n")
        print(round(varex, digits))
        invisible(newx) #previously returned x
}