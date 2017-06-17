bargraph <- function(data,order,highcol = "blue", lowcol = "red"){
        theme_default <- function(base_size = 10, base_family = ""){
                theme_bw(base_size = base_size, base_family = base_family) %+replace%
                        theme( strip.background = element_blank()
                        )
        }
        loadings.m <-melt(unclass(data$cis$means),value.name="Loading")
        colnames(loadings.m)[1:2] <- c("Item","Factor")
        loadings.m$Item <- factor(loadings.m$Item, levels =c(order))
        Fig <- ggplot(loadings.m, aes(Item, abs(Loading), fill=Loading)) + 
                facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
                geom_bar(stat="identity") + #make the bars
                coord_flip() + #flip the axes so the test names can be horizontal  
                #define the fill color gradient: blue=positive, red=negative
                scale_fill_gradient2(name = "Loading", 
                                     high = highcol, mid = "white", low = lowcol, 
                                     midpoint=0, guide=F) +
                ylab("Loading Strength") + #improve y-axis label
                theme_default()
                #use a black-and0white theme with set font size
        return(Fig)
        print(Fig)
        
}