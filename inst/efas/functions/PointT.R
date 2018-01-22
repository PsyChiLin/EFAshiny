PointT <- function(q,v,data){
        qdf <- as.data.frame(q$rotated)
        vdf <- as.data.frame(v$rotated)
        qdf$Method <- "CF-quartimax"
        vdf$Method <- "CF-varimax"
        point <- cbind(qdf,vdf)
        row.names(point) <- colnames(data)
        return(point)
}