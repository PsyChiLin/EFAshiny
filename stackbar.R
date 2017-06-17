stackbar <- function(CorMat,PatMat,order){
        corrs.m <- melt(CorMat,value.name="Correlation")
        corrs.m$Var1 <- factor(corrs.m$Var1, levels =c(order))
        corrs.m$Var2 <- factor(corrs.m$Var2, levels =c(order))
        p1 <- ggplot(corrs.m, aes(Var2, Var1, fill=abs(Correlation))) + 
                geom_tile() + 
                geom_text(aes(label = round(Correlation, 2)), size=2.5) + 
                theme_default()+
                theme(axis.text.x = element_text(angle = 90), 
                      axis.title.x=element_blank(), 
                      axis.title.y=element_blank()) +
                #set correlation fill gradient
                scale_fill_gradient(low="white", high="red") + 
                guides(fill=F) #omit unnecessary gradient legend
        loadings.m <-melt(unclass(PatMat$cis$means),value.name="Loading")
        colnames(loadings.m)[1:2] <- c("Item","Factor")
        loadings.m$Item <- factor(loadings.m$Item, levels =c(order))
         p2 <- ggplot(loadings.m, aes(Item, abs(Loading), fill=Factor)) + 
                geom_bar(stat="identity") + 
                 coord_flip() + 
                ylab("Loading Strength") + 
                 theme_default(base_size=10) + 
                #remove labels and tweak margins for combining with the correlation matrix plot
                theme(axis.text.y = element_blank(), 
                      axis.title.y = element_blank())
         grid.arrange(p1, p2, ncol=2, widths=c(2, 1))
}