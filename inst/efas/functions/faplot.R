faplot <- function(data,n.obs = NULL,quant = 0.95,fm = "promax",n.iter = 200){
        set.seed(10)
        rst_list <- list()
        library(reshape2)
        library(ggplot2)
        library(psych)
        #Sys.sleep(2)
        fa.parallel <- function (x, n.obs = NULL, fm = "minres", fa = "both", main = "Parallel Analysis Scree Plots", 
                                 n.iter = 20, error.bars = FALSE, se.bars = FALSE, SMC = FALSE, 
                                 ylabel = NULL, show.legend = TRUE, sim = TRUE, quant = 0.95, 
                                 cor = "cor", use = "pairwise", plot = TRUE, correct = 0.5) {
                cl <- match.call()
                ci <- 1.96
                arrow.len <- 0.05
                nsub <- dim(x)[1]
                nvariables <- dim(x)[2]
                resample <- TRUE
                if ((isCorrelation(x)) && !sim) {
                        warning("You specified a correlation matrix, but asked to just resample (sim was set to FALSE).  This is impossible, so sim is set to TRUE")
                        sim <- TRUE
                        resample <- FALSE
                }
                if (!is.null(n.obs)) {
                        nsub <- n.obs
                        rx <- x
                        resample <- FALSE
                        if (dim(x)[1] != dim(x)[2]) {
                                warning("You specified the number of subjects, implying a correlation matrix, but do not have a correlation matrix, correlations found ")
                                switch(cor, cor = {
                                        rx <- cor(x, use = use)
                                }, cov = {
                                        rx <- cov(x, use = use)
                                        covar <- TRUE
                                }, tet = {
                                        rx <- tetrachoric(x, correct = correct)$rho
                                }, poly = {
                                        rx <- polychoric(x, correct = correct)$rho
                                }, mixed = {
                                        rx <- mixedCor(x, use = use, correct = correct)$rho
                                }, Yuleb = {
                                        rx <- YuleCor(x, , bonett = TRUE)$rho
                                }, YuleQ = {
                                        rx <- YuleCor(x, 1)$rho
                                }, YuleY = {
                                        rx <- YuleCor(x, 0.5)$rho
                                })
                                if (!sim) {
                                        warning("You specified a correlation matrix, but asked to just resample (sim was set to FALSE).  This is impossible, so sim is set to TRUE")
                                        sim <- TRUE
                                        resample <- FALSE
                                }
                        }
                }
                else {
                        if (isCorrelation(x)) {
                                warning("It seems as if you are using a correlation matrix, but have not specified the number of cases. The number of subjects is arbitrarily set to be 100  ")
                                rx <- x
                                nsub = 100
                                n.obs = 100
                                resample <- FALSE
                        }
                        else {
                                switch(cor, cor = {
                                        rx <- cor(x, use = use)
                                }, cov = {
                                        rx <- cov(x, use = use)
                                        covar <- TRUE
                                }, tet = {
                                        rx <- tetrachoric(x, correct = correct)$rho
                                }, poly = {
                                        rx <- polychoric(x, correct = correct)$rho
                                }, mixed = {
                                        rx <- mixedCor(x, use = use, correct = correct)$rho
                                }, Yuleb = {
                                        rx <- YuleCor(x, , bonett = TRUE)$rho
                                }, YuleQ = {
                                        rx <- YuleCor(x, 1)$rho
                                }, YuleY = {
                                        rx <- YuleCor(x, 0.5)$rho
                                })
                        }
                }
                valuesx <- eigen(rx)$values
                if (SMC) {
                        diag(rx) <- smc(rx)
                        fa.valuesx <- eigen(rx)$values
                }
                else {
                        fa.valuesx <- fa(rx, fm = fm, warnings = FALSE)$values
                }
                temp <- list(samp = vector("list", n.iter), samp.fa = vector("list", 
                                                                             n.iter), sim = vector("list", n.iter), sim.fa = vector("list", 
                                                                                                                                    n.iter))
                templist <- mclapply(1:n.iter, function(XX) {
                        if (is.null(n.obs)) {
                                bad <- TRUE
                                while (bad) {
                                        sampledata <- matrix(apply(x, 2, function(y) sample(y, 
                                                                                            nsub, replace = TRUE)), ncol = nvariables)
                                        colnames(sampledata) <- colnames(x)
                                        switch(cor, cor = {
                                                C <- cor(sampledata, use = use)
                                        }, cov = {
                                                C <- cov(sampledata, use = use)
                                                covar <- TRUE
                                        }, tet = {
                                                C <- tetrachoric(sampledata, correct = correct)$rho
                                        }, poly = {
                                                C <- polychoric(sampledata, correct = correct)$rho
                                        }, mixed = {
                                                C <- mixedCor(sampledata, use = use, correct = correct)$rho
                                        }, Yuleb = {
                                                C <- YuleCor(sampledata, , bonett = TRUE)$rho
                                        }, YuleQ = {
                                                C <- YuleCor(sampledata, 1)$rho
                                        }, YuleY = {
                                                C <- YuleCor(sampledata, 0.5)$rho
                                        })
                                        bad <- any(is.na(C))
                                }
                                values.samp <- eigen(C)$values
                                temp[["samp"]] <- values.samp
                                if (fa != "pc") {
                                        if (SMC) {
                                                sampler <- C
                                                diag(sampler) <- smc(sampler)
                                                temp[["samp.fa"]] <- eigen(sampler)$values
                                        }
                                        else {
                                                temp[["samp.fa"]] <- fa(C, fm = fm, SMC = FALSE, 
                                                                        warnings = FALSE)$values
                                        }
                                }
                        }
                        if (sim) {
                                simdata = matrix(rnorm(nsub * nvariables), nrow = nsub, 
                                                 ncol = nvariables)
                                sim.cor <- cor(simdata)
                                temp[["sim"]] <- eigen(sim.cor)$values
                                if (fa != "pc") {
                                        if (SMC) {
                                                diag(sim.cor) <- smc(sim.cor)
                                                temp[["sim.fa"]] <- eigen(sim.cor)$values
                                        }
                                        else {
                                                fa.values.sim <- fa(sim.cor, fm = fm, SMC = FALSE, 
                                                                    warnings = FALSE)$values
                                                temp[["sim.fa"]] <- fa.values.sim
                                        }
                                }
                        }
                        replicates <- list(samp = temp[["samp"]], samp.fa = temp[["samp.fa"]], 
                                           sim = temp[["sim"]], sim.fa = temp[["sim.fa"]])
                })
                if (is.null(ylabel)) {
                        ylabel <- switch(fa, pc = "eigen values of principal components", 
                                         fa = "eigen values of principal factors", both = "eigenvalues of principal components and factor analysis")
                }
                values <- t(matrix(unlist(templist), ncol = n.iter))
                values.sim.mean = colMeans(values, na.rm = TRUE)
                if (!missing(quant)) {
                        values.ci = apply(values, 2, function(x) quantile(x, 
                                                                          quant))
                }
                else {
                        values.ci <- values.sim.mean
                }
                if (se.bars) {
                        values.sim.se <- apply(values, 2, sd, na.rm = TRUE)/sqrt(n.iter)
                }
                else {
                        values.sim.se <- apply(values, 2, sd, na.rm = TRUE)
                }
                ymin <- min(valuesx, values.sim.mean)
                ymax <- max(valuesx, values.sim.mean)
                sim.pcr <- sim.far <- NA
                switch(fa, pc = {
                        if (plot) {
                                plot(valuesx, type = "b", main = main, ylab = ylabel, 
                                     ylim = c(ymin, ymax), xlab = "Component Number", 
                                     pch = 4, col = "blue")
                        }
                        if (resample) {
                                sim.pcr <- values.sim.mean[1:nvariables]
                                sim.pcr.ci <- values.ci[1:nvariables]
                                sim.se.pcr <- values.sim.se[1:nvariables]
                                if (plot) {
                                        points(sim.pcr, type = "l", lty = "dashed", pch = 4, 
                                               col = "red")
                                }
                        } else {
                                sim.pcr <- NA
                                sim.se.pc <- NA
                        }
                        if (sim) {
                                if (resample) {
                                        sim.pc <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                            nvariables)]
                                        sim.pc.ci <- values.ci[(nvariables + 1):(2 * 
                                                                                         nvariables)]
                                        sim.se.pc <- values.sim.se[(nvariables + 1):(2 * 
                                                                                             nvariables)]
                                } else {
                                        sim.pc <- values.sim.mean[1:nvariables]
                                        sim.pc.ci <- values.ci[1:nvariables]
                                        sim.se.pc <- values.sim.se[1:nvariables]
                                }
                                if (plot) {
                                        points(sim.pc, type = "l", lty = "dotted", pch = 4, 
                                               col = "red")
                                }
                                pc.test <- which(!(valuesx > sim.pc.ci))[1] - 1
                        } else {
                                sim.pc <- NA
                                sim.pc.ci <- NA
                                sim.se.pc <- NA
                                pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 1
                        }
                        fa.test <- NA
                        sim.far <- NA
                        sim.fa <- NA
                }, fa = {
                        if (plot) {
                                plot(fa.valuesx, type = "b", main = main, ylab = ylabel, 
                                     ylim = c(ymin, ymax), xlab = "Factor Number", 
                                     pch = 2, col = "blue")
                        }
                        sim.se.pc <- NA
                        if (resample) {
                                sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                     nvariables)]
                                sim.far.ci <- values.ci[(nvariables + 1):(2 * nvariables)]
                                sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                                                                                      nvariables)]
                                if (plot) {
                                        points(sim.far, type = "l", lty = "dashed", pch = 2, 
                                               col = "red")
                                }
                        }
                        if (sim) {
                                if (resample) {
                                        sim.fa <- values.sim.mean[(3 * nvariables + 1):(4 * 
                                                                                                nvariables)]
                                        sim.fa.ci <- values.ci[(3 * nvariables + 1):(4 * 
                                                                                             nvariables)]
                                        sim.se.fa <- values.sim.se[(3 * nvariables + 
                                                                            1):(4 * nvariables)]
                                } else {
                                        sim.fa <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                            nvariables)]
                                        sim.fa.ci <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                               nvariables)]
                                        sim.se.fa <- values.sim.se[(nvariables + 1):(2 * 
                                                                                             nvariables)]
                                        sim.far <- NA
                                        sim.far.ci <- NA
                                        sim.se.far <- NA
                                }
                                if (plot) {
                                        points(sim.fa, type = "l", lty = "dotted", pch = 2, 
                                               col = "red")
                                }
                                fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
                                        1
                        } else {
                                sim.fa <- NA
                                fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
                                        1
                        }
                        sim.pc <- NA
                        sim.pcr <- NA
                        sim.se.pc <- NA
                        pc.test <- NA
                }, both = {
                        if (plot) {
                                plot(valuesx, type = "b", main = main, ylab = ylabel, 
                                     ylim = c(ymin, ymax), xlab = "Factor/Component Number", 
                                     pch = 4, col = "blue")
                                points(fa.valuesx, type = "b", pch = 2, col = "blue")
                        }
                        if (sim) {
                                if (resample) {
                                        sim.pcr <- values.sim.mean[1:nvariables]
                                        sim.pcr.ci <- values.ci[1:nvariables]
                                        sim.se.pcr <- values.sim.se[1:nvariables]
                                        sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                             nvariables)]
                                        sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                                                                                              nvariables)]
                                        sim.far.ci <- values.ci[(nvariables + 1):(2 * 
                                                                                          nvariables)]
                                        sim.pc <- values.sim.mean[(2 * nvariables + 1):(3 * 
                                                                                                nvariables)]
                                        sim.pc.ci <- values.ci[(2 * nvariables + 1):(3 * 
                                                                                             nvariables)]
                                        sim.se.pc <- values.sim.se[(2 * nvariables + 
                                                                            1):(3 * nvariables)]
                                        sim.fa <- values.sim.mean[(3 * nvariables + 1):(4 * 
                                                                                                nvariables)]
                                        sim.fa.ci <- values.ci[(3 * nvariables + 1):(4 * 
                                                                                             nvariables)]
                                        sim.se.fa <- values.sim.se[(3 * nvariables + 
                                                                            1):(4 * nvariables)]
                                        pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 
                                                1
                                        fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
                                                1
                                } else {
                                        sim.pc <- values.sim.mean[1:nvariables]
                                        sim.pc.ci <- values.ci[1:nvariables]
                                        sim.se.pc <- values.sim.se[1:nvariables]
                                        sim.fa <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                            nvariables)]
                                        sim.fa.ci <- values.ci[(nvariables + 1):(2 * 
                                                                                         nvariables)]
                                        sim.se.fa <- values.sim.se[(nvariables + 1):(2 * 
                                                                                             nvariables)]
                                        pc.test <- which(!(valuesx > sim.pc.ci))[1] - 
                                                1
                                        fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
                                                1
                                }
                                if (plot) {
                                        points(sim.pc, type = "l", lty = "dotted", pch = 4, 
                                               col = "red")
                                        points(sim.fa, type = "l", lty = "dotted", pch = 4, 
                                               col = "red")
                                        points(sim.pcr, type = "l", lty = "dashed", pch = 2, 
                                               col = "red")
                                        points(sim.far, type = "l", lty = "dashed", pch = 2, 
                                               col = "red")
                                }
                                pc.test <- which(!(valuesx > sim.pc.ci))[1] - 1
                                fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
                                        1
                        } else {
                                sim.pcr <- values.sim.mean[1:nvariables]
                                sim.pcr.ci <- values.ci[1:nvariables]
                                sim.se.pcr <- values.sim.se[1:nvariables]
                                sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                                                                                     nvariables)]
                                sim.far.ci <- values.ci[(nvariables + 1):(2 * nvariables)]
                                sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                                                                                      nvariables)]
                                sim.fa <- NA
                                sim.pc <- NA
                                sim.se.fa <- NA
                                sim.se.pc <- NA
                                pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 1
                                fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
                                        1
                        }
                        if (resample) {
                                if (plot) {
                                        points(sim.pcr, type = "l", lty = "dashed", pch = 4, 
                                               col = "red")
                                        points(sim.far, type = "l", lty = "dashed", pch = 4, 
                                               col = "red")
                                }
                        }
                })
                if (error.bars) {
                        if (!any(is.na(sim.pc))) {
                                for (i in 1:length(sim.pc)) {
                                        ycen <- sim.pc[i]
                                        yse <- sim.se.pc[i]
                                        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                                               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                                               lty = NULL, lwd = par("lwd"), xpd = NULL)
                                }
                        }
                        if (!any(is.na(sim.pcr))) {
                                for (i in 1:length(sim.pcr)) {
                                        ycen <- sim.pcr[i]
                                        yse <- sim.se.pcr[i]
                                        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                                               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                                               lty = NULL, lwd = par("lwd"), xpd = NULL)
                                }
                        }
                        if (!any(is.na(sim.fa))) {
                                for (i in 1:length(sim.fa)) {
                                        ycen <- sim.fa[i]
                                        yse <- sim.se.fa[i]
                                        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                                               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                                               lty = NULL, lwd = par("lwd"), xpd = NULL)
                                }
                        }
                        if (!any(is.na(sim.far))) {
                                for (i in 1:length(sim.far)) {
                                        ycen <- sim.far[i]
                                        yse <- sim.se.far[i]
                                        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                                               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                                               lty = NULL, lwd = par("lwd"), xpd = NULL)
                                }
                        }
                }
                if (show.legend && plot) {
                        if (is.null(n.obs)) {
                                switch(fa, both = {
                                        if (sim) {
                                                legend("topright", c("  PC  Actual Data", "  PC  Simulated Data", 
                                                                     " PC  Resampled Data", "  FA  Actual Data", 
                                                                     "  FA  Simulated Data", " FA  Resampled Data"), 
                                                       col = c("blue", "red", "red", "blue", "red", 
                                                               "red"), pch = c(4, NA, NA, 2, NA, NA), 
                                                       text.col = "green4", lty = c("solid", "dotted", 
                                                                                    "dashed", "solid", "dotted", "dashed"), 
                                                       merge = TRUE, bg = "gray90")
                                        } else {
                                                legend("topright", c("  PC  Actual Data", " PC  Resampled Data", 
                                                                     "  FA  Actual Data", " FA  Resampled Data"), 
                                                       col = c("blue", "red", "blue", "red"), pch = c(4, 
                                                                                                      NA, 2, NA, NA), text.col = "green4", lty = c("solid", 
                                                                                                                                                   "dashed", "solid", "dashed"), merge = TRUE, 
                                                       bg = "gray90")
                                        }
                                }, pc = {
                                        if (sim) {
                                                legend("topright", c("  PC  Actual Data", "  PC  Simulated Data", 
                                                                     " PC  Resampled Data"), col = c("blue", "red", 
                                                                                                     "red", "blue", "red", "red"), pch = c(4, 
                                                                                                                                           NA, NA, 2, NA, NA), text.col = "green4", 
                                                       lty = c("solid", "dotted", "dashed", "solid", 
                                                               "dotted", "dashed"), merge = TRUE, bg = "gray90")
                                        } else {
                                                legend("topright", c("  PC  Actual Data", " PC  Resampled Data"), 
                                                       col = c("blue", "red", "red", "blue", "red", 
                                                               "red"), pch = c(4, NA, NA, 2, NA, NA), 
                                                       text.col = "green4", lty = c("solid", "dashed", 
                                                                                    "solid", "dotted", "dashed"), merge = TRUE, 
                                                       bg = "gray90")
                                        }
                                }, fa = {
                                        if (sim) {
                                                legend("topright", c("  FA  Actual Data", "  FA  Simulated Data", 
                                                                     " FA  Resampled Data"), col = c("blue", "red", 
                                                                                                     "red", "blue", "red", "red"), pch = c(4, 
                                                                                                                                           NA, NA, 2, NA, NA), text.col = "green4", 
                                                       lty = c("solid", "dotted", "dashed", "solid", 
                                                               "dotted", "dashed"), merge = TRUE, bg = "gray90")
                                        } else {
                                                legend("topright", c("  FA  Actual Data", " FA  Resampled Data"), 
                                                       col = c("blue", "red", "red", "blue", "red", 
                                                               "red"), pch = c(4, NA, NA, 2, NA, NA), 
                                                       text.col = "green4", lty = c("solid", "dashed", 
                                                                                    "solid", "dotted", "dashed"), merge = TRUE, 
                                                       bg = "gray90")
                                        }
                                })
                        }
                        else {
                                switch(fa, both = {
                                        legend("topright", c("PC  Actual Data", " PC  Simulated Data", 
                                                             "FA  Actual Data", " FA  Simulated Data"), 
                                               col = c("blue", "red", "blue", "red"), pch = c(4, 
                                                                                              NA, 2, NA), text.col = "green4", lty = c("solid", 
                                                                                                                                       "dotted", "solid", "dotted"), merge = TRUE, 
                                               bg = "gray90")
                                }, pc = {
                                        legend("topright", c("PC  Actual Data", " PC  Simulated Data"), 
                                               col = c("blue", "red", "blue", "red"), pch = c(4, 
                                                                                              NA, 2, NA), text.col = "green4", lty = c("solid", 
                                                                                                                                       "dotted", "solid", "dotted"), merge = TRUE, 
                                               bg = "gray90")
                                }, fa = {
                                        legend("topright", c("FA  Actual Data", " FA  Simulated Data"), 
                                               col = c("blue", "red", "blue", "red"), pch = c(4, 
                                                                                              NA, 2, NA), text.col = "green4", lty = c("solid", 
                                                                                                                                       "dotted", "solid", "dotted"), merge = TRUE, 
                                               bg = "gray90")
                                })
                        }
                }
                colnames(values) <- paste0("Sim", 1:ncol(values))
                if (fa != "pc" && plot) 
                        abline(h = 1)
                results <- list(fa.values = fa.valuesx, pc.values = valuesx, 
                                pc.sim = sim.pc, pc.simr = sim.pcr, fa.sim = sim.fa, 
                                fa.simr = sim.far, nfact = fa.test, ncomp = pc.test, 
                                Call = cl)
                if (fa == "pc") {
                        colnames(values)[1:nvariables] <- paste0("C", 1:nvariables)
                }
                else {
                        colnames(values)[1:(2 * nvariables)] <- c(paste0("C", 
                                                                         1:nvariables), paste0("F", 1:nvariables))
                        if (sim) {
                                if (resample) 
                                        colnames(values)[(2 * nvariables + 1):ncol(values)] <- c(paste0("CSim", 
                                                                                                        1:nvariables), paste0("Fsim", 1:nvariables))
                        }
                        results$nfact <- fa.test
                }
                results$ncomp <- pc.test
                results$values <- values
                #cat("Parallel analysis suggests that ")
                #cat("the number of factors = ", fa.test, " and the number of components = ", 
                #    pc.test, "\n")
                class(results) <- c("psych", "parallel")
                return(invisible(results))
        }
        PArst <-  suppressMessages(fa.parallel(x =  data, n.obs = n.obs,fa = "pc", 
                                               quant = quant,fm = fm, plot = FALSE,n.iter = n.iter))
        #PArst$nfact
        plotpa <- suppressMessages(melt(data.frame(Actual = PArst$pc.values ,sim = PArst$pc.sim)))
        plotpa$index <- as.factor(rep(seq(1,length(PArst$pc.values)),2))
        Fig <- ggplot(plotpa, aes(x = index,y = value, group = variable,
                                  col = variable, linetype = variable))+
                geom_point(aes(size = variable))+
                geom_line()+
                theme_bw()+
                scale_x_discrete(labels = c(seq(1,ncol(data))))+
                scale_linetype_manual(values=c(1,2,2),name = "",
                                      labels = c("Actual Data",paste("Parallel Analysis :",quant)))+
                scale_size_manual(values=c(3,1,1),name = "",
                                  labels = c("Actual Data",paste("Parallel Analysis :",quant)))+
                scale_color_manual(values = c("firebrick","chartreuse4","chartreuse2"),name = "",
                                   labels = c("Actual Data",paste("Parallel Analysis :",quant)))+
                labs(list(x = "Number", y = "Eigen Values"))+
                ggtitle("Scree Plots : Parallel Analysis")+
                theme(legend.position = c(0.7,0.77),
                      plot.title = element_text(hjust = 0.5))
        rst_list[[1]] <- Fig
        rst_list[[2]] <- PArst$ncomp
        return(rst_list)
        print(Fig)
} 



