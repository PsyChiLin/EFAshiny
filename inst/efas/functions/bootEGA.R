bootEGA <- function(data, n, medianStructure = TRUE, plot.MedianStructure = TRUE, ncores = 4, layout = "spring") {
        set.seed(0)
          if(!require(qgraph)) {
                    message("installing the 'qgraph' package")
                    install.packages("qgraph")
                  }
        
                  if(!require(bootnet)) {
                            message("installing the 'bootnet' package")
                            install.packages("bootnet")
                          }
        
                  if(!require(igraph)) {
                            message("installing the 'igraph' package")
                            install.packages("igraph")
                          }
        
                  boot.ega <- bootnet(data, nBoot = n, default = "EBICglasso",
                                                             computeCentrality = FALSE, type = "parametric", nCores = ncores)
                  bootGraphs <- vector("list", n)
                  for (i in 1:n) {
                            bootGraphs[[i]] <- boot.ega$boots[[i]]$graph
                            colnames(bootGraphs[[i]]) <- colnames(data)
                            rownames(bootGraphs[[i]]) <- colnames(data)
                          }
                  boot.igraph <- vector("list", n)
                  for (l in 1:n) {
                            boot.igraph[[l]] <- as.igraph(qgraph(abs(bootGraphs[[l]]), DoNotPlot = TRUE))
                          }
                  boot.wc <- vector("list", n)
                  for (m in 1:n) {
                            boot.wc[[m]] <- walktrap.community(boot.igraph[[m]])
                          }
                  boot.ndim <- matrix(NA, nrow = n, ncol = 2)
                  for (m in 1:n) {
                            boot.ndim[m, 2] <- max(boot.wc[[m]]$membership)
                          }
                  colnames(boot.ndim) <- c("Boot.Number", "N.Dim")
                  boot.ndim[, 1] <- seq_len(n)
                  if (medianStructure == TRUE) {
                            median.Structure <- apply(simplify2array(bootGraphs),
                                                                                     1:2, median)
                            median.igraph <- as.igraph(qgraph(abs(median.Structure), DoNotPlot = TRUE))
                            median.wc <- walktrap.community(median.igraph)
                            median.ndim <- max(median.wc$membership)
                            dim.variables <- data.frame(items = colnames(data), dimension = median.wc$membership)
                          }
                  if (plot.MedianStructure == TRUE) {
                            plot.median.ega <- qgraph(median.Structure, layout =  layout ,
                                                      vsize = 6, groups = as.factor(median.wc$membership))
                          }
                  Median <- median(boot.ndim[, 2])
                  sd.boot <- sd(boot.ndim[, 2])
                  se.boot <- (1.253 * sd.boot)/sqrt(nrow(boot.ndim))
                  ciMult <- qt(0.95/2+0.5, nrow(boot.ndim) - 1)
                  ci <- se.boot * ciMult
                  summary.table <- data.frame(n.Boots = n, median.dim = Median,
                    SD.dim = sd.boot, SE.dim = se.boot, CI.dim = ci, Lower = Median -ci, Upper = Median + ci)
                  result <- list()
                  result$n <- n
                  result$boot.ndim <- boot.ndim
                  result$bootGraphs <- bootGraphs
                  result$summary.table <- summary.table
                  medianGraph <- list()
                  medianGraph$graph <- median.Structure
                  medianGraph$median.dim.variables <- dim.variables[order(dim.variables[,2]), ]
                  result$medianGraph <- medianGraph
                  result$plot <- plot.median.ega 
                  class(result) <- "bootEGA"
                  return(result)
                }