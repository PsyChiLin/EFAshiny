SEplot <- function(q,v,data){
        theme_default <- function(base_size = 10, base_family = ""){
                theme_bw(base_size = base_size, base_family = base_family) %+replace%
                        theme( strip.background = element_blank()
                        )
        }
        qm <- melt(q$rotatedse)
        qm$Method <- "CF-quartimax"
        qm$Var1 <- colnames(data)
        vm <- melt(v$rotatedse)
        vm$Method <- "CF-varimax"
        vm$Var1 <- colnames(data)
        qv <- rbind(qm,vm)
        colnames(qv)[1:3] <-c("Item","Factor","SE") 
        qv$Item <- factor(qv$Item, levels = colnames(data))
        Fig <- ggplot(qv, aes(x = Item, y = SE, col = Method))+
                geom_point()+
                facet_grid(Factor~.)+
                scale_color_manual(values = c("chartreuse4","firebrick"))+
                theme_default()
        return(Fig)
}
