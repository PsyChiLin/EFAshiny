fa.diagram.ext <- function (fa.results,col = c("black", "red"), Phi = NULL, fe.results = NULL, sort = TRUE, 
          labels = NULL, cut = 0.3, simple = TRUE, errors = FALSE, 
          g = FALSE, digits = 1, e.size = 0.05, rsize = 0.15, side = 2, 
          main, cex = NULL, marg = c(0.5, 0.5, 1, 0.5), adj = 1, ...) 
{
        old.par <- par(mar = marg)
        on.exit(par(old.par))
        #col <- c("black", "red")
        if (missing(main)) 
                if (is.null(fe.results)) {
                        main <- "Factor Analysis"
                }
        else {
                main <- "Factor analysis and extension"
        }
        if (!is.matrix(fa.results) && !is.null(fa.results$fa) && 
            is.list(fa.results$fa)) 
                fa.results <- fa.results$fa
        if (is.null(cex)) 
                cex <- 1
        if (sort) {
                fa.results <- fa.sort(fa.results)
                if (!is.null(fe.results)) {
                        fe.results <- fa.sort(fe.results)
                }
        }
        if ((!is.matrix(fa.results)) && (!is.data.frame(fa.results))) {
                factors <- as.matrix(fa.results$loadings)
                if (!is.null(fa.results$Phi)) {
                        Phi <- fa.results$Phi
                }
                else {
                        if (!is.null(fa.results$cor)) {
                                Phi <- fa.results$cor
                        }
                }
        }
        else {
                factors <- fa.results
        }
        nvar <- dim(factors)[1]
        if (is.null(nvar)) {
                nvar <- length(factors)
                num.factors <- 1
        }
        else {
                num.factors <- dim(factors)[2]
        }
        nvar <- dim(factors)[1]
        e.size = e.size * 16 * cex/nvar
        if (is.null(nvar)) {
                nvar <- length(factors)
                num.factors <- 1
        }
        else {
                num.factors <- dim(factors)[2]
        }
        if (is.null(rownames(factors))) {
                rownames(factors) <- paste("V", 1:nvar, sep = "")
        }
        if (is.null(colnames(factors))) {
                colnames(factors) <- paste("F", 1:num.factors, sep = "")
        }
        var.rect <- list()
        fact.rect <- list()
        max.len <- max(nchar(rownames(factors))) * rsize
        x.max <- max((nvar + 1), 6)
        limx = c(-max.len/2, x.max)
        n.evar <- 0
        if (!is.null(fe.results)) {
                n.evar <- dim(fe.results$loadings)[1]
                limy <- c(0, max(nvar + 1, n.evar + 1))
        }
        else {
                limy = c(0, nvar + 1)
        }
        top <- max(nvar, n.evar) + 1
        plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
             axes = FALSE, ylab = "", xlab = "", main = main, ...)
        max.len <- max(strwidth(rownames(factors)), strwidth("abc"))/1.8
        limx = c(-max.len/2, x.max)
        cex <- min(cex, 20/x.max)
        if (g) {
                left <- 0.3 * x.max
                middle <- 0.6 * x.max
                gf <- 2
        }
        else {
                left <- 0
                middle <- 0.5 * x.max
                gf <- 1
        }
        for (v in 1:nvar) {
                var.rect[[v]] <- dia.rect(left, top - v - max(0, n.evar - 
                                                                      nvar)/2, rownames(factors)[v], xlim = limx, ylim = limy, 
                                          cex = cex, ...)
        }
        f.scale <- (top)/(num.factors + 1)
        f.shift <- max(nvar, n.evar)/num.factors
        if (g) {
                fact.rect[[1]] <- dia.ellipse(-max.len/2, top/2, colnames(factors)[1], 
                                              xlim = limx, ylim = limy, e.size = e.size, cex = cex, 
                                              ...)
                for (v in 1:nvar) {
                        if (simple && (abs(factors[v, 1]) == max(abs(factors[v, 
                                                                             ]))) && (abs(factors[v, 1]) > cut) | (!simple && 
                                                                                                                   (abs(factors[v, 1]) > cut))) {
                                dia.arrow(from = fact.rect[[1]], to = var.rect[[v]]$left, 
                                          labels = round(factors[v, 1], digits), col = ((sign(factors[v, 
                                                                                                      1]) < 0) + 1), lty = ((sign(factors[v, 1]) < 
                                                                                                                                     0) + 1))
                        }
                }
        }
        for (f in gf:num.factors) {
                fact.rect[[f]] <- dia.ellipse(left + middle, (num.factors + 
                                                                      gf - f) * f.scale, colnames(factors)[f], xlim = limx, 
                                              ylim = limy, e.size = e.size, cex = cex, ...)
                for (v in 1:nvar) {
                        if (simple && (abs(factors[v, f]) == max(abs(factors[v, 
                                                                             ]))) && (abs(factors[v, f]) > cut) | (!simple && 
                                                                                                                   (abs(factors[v, f]) > cut))) {
                                dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                                          labels = round(factors[v, f], digits), col = ((sign(factors[v, 
                                                                                                      f]) < 0) + 1), lty = ((sign(factors[v, f]) < 
                                                                                                                                     0) + 1), adj = f%%adj + 1, cex = cex)
                        }
                }
        }
        if (!is.null(Phi) && (ncol(Phi) > 1)) {
                for (i in 2:num.factors) {
                        for (j in 1:(i - 1)) {
                                if (abs(Phi[i, j]) > cut) {
                                        dia.curve(from = fact.rect[[j]]$right, to = fact.rect[[i]]$right, 
                                                  labels = round(Phi[i, j], digits), scale = (i - 
                                                                                                      j), cex = cex, ...)
                                }
                        }
                }
        }
        if (errors) {
                for (v in 1:nvar) {
                        dia.self(location = var.rect[[v]], scale = 0.5, side = side)
                }
        }
        if (!is.null(fe.results)) {
                e.loadings <- fe.results$loadings
                for (v in 1:n.evar) {
                        var.rect[[v]] <- dia.rect(x.max, top - v - max(0, 
                                                                       nvar - n.evar)/2, rownames(e.loadings)[v], xlim = limx, 
                                                  ylim = limy, cex = cex, ...)
                        for (f in 1:num.factors) {
                                if (simple && (abs(e.loadings[v, f]) == max(abs(e.loadings[v, 
                                                                                           ]))) && (abs(e.loadings[v, f]) > cut) | (!simple && 
                                                                                                                                    (abs(e.loadings[v, f]) > cut))) {
                                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$left, 
                                                  labels = round(e.loadings[v, f], digits), 
                                                  col = ((sign(e.loadings[v, f]) < 0) + 1), 
                                                  lty = ((sign(e.loadings[v, f]) < 0) + 1), 
                                                  adj = f%%adj + 1)
                                }
                        }
                }
        }
}