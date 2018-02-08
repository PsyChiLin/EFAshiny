faplot <- function(data,n.obs = NULL,quant = 0.95,fm = "promax",n.iter = 200){
        set.seed(10)
        rst_list <- list()
        library(reshape2)
        library(ggplot2)
        library(psych)
        #Sys.sleep(2)
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