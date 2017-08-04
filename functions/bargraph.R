bargraph <- function(data,order=NULL,nf,highcol = "blue", lowcol = "red",ci){
        theme_default <- function(base_size = 10, base_family = ""){
                theme_bw(base_size = base_size, base_family = base_family) %+replace%
                        theme( strip.background = element_blank()
                        )
        }
        loadings.m <-melt(unclass(data$cis$means),value.name="Loading")
        colnames(loadings.m)[1:2] <- c("Item","Factor")
        if(nf != 1){
                low <- melt(data$cis$ci[,1:nf],value.name = "Lower")
                up <- melt(data$cis$ci[,I(nf+1):I(nf+nf)],value.name = "Upper")
        }
        if(nf == 1){
                low <- melt(data$cis$ci[,1],value.name = "Lower")
                up <- melt(data$cis$ci[,2],value.name = "Upper")
        }
        loadings.m$Lower <- low$Lower
        loadings.m$Upper <- up$Upper
        loadings.m$Item <- factor(loadings.m$Item, levels =c(order))
        if (ci == T){
                Fig <- ggplot(loadings.m, aes(Item, Loading, fill=Loading)) + 
                        facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
                        geom_bar(stat="identity") + #make the bars
                        coord_flip() + #flip the axes so the test names can be horizontal  
                        #define the fill color gradient: blue=positive, red=negative
                        scale_fill_gradient2(name = "Loading", 
                                             high = highcol, mid = "white", low = lowcol, 
                                             midpoint=0, guide=F)+
                        geom_errorbar(aes(ymin=Lower, ymax=Upper),
                                      #position=position_dodge(.3),
                                      width=.1                    # Width of the error bars
                        )+
                        ylab("Loading Strength") + #improve y-axis label
                        theme_default()
                #use a black-and0white theme with set font size
        }
        if (ci == F){
                Fig <- ggplot(loadings.m, aes(Item, Loading, fill=Loading)) + 
                        facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
                        geom_bar(stat="identity") + #make the bars
                        coord_flip() + #flip the axes so the test names can be horizontal  
                        #define the fill color gradient: blue=positive, red=negative
                        scale_fill_gradient2(name = "Loading", 
                                             high = highcol, mid = "white", low = lowcol, 
                                             midpoint=0, guide=F)+
                        #geom_errorbar(aes(ymin=Lower, ymax=Upper),
                        #              #position=position_dodge(.3),
                        #              width=.1                    # Width of the error bars
                        #)+
                        ylab("Loading Strength") + #improve y-axis label
                        theme_default()
                #use a black-and0white theme with set font size
        }
        return(Fig)
        print(Fig)
        
}