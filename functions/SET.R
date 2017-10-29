SET <- function(q,v,data){
        qdf <- as.data.frame(q$rotatedse)
        vdf <- as.data.frame(v$rotatedse)
        qdf$Method <- "CF-quartimax"
        vdf$Method <- "CF-varimax"
        Stable <- cbind(qdf,vdf)
        row.names(Stable) <- colnames(data)
        return(Stable)
}