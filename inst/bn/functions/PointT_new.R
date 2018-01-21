PointT_new <- function(q,v,data, nbf){
        theme_default <- function(base_size = 10, base_family = ""){
                theme_bw(base_size = base_size, base_family = base_family) %+replace%
                        theme( strip.background = element_blank()
                        )
        }
        if (nbf == 1){
                stop("Errrrrr")   
        } else {
                # Q
                qdf <- as.data.frame(q$rotated)
                qdf$Method <- "CF-quartimax"
                qdf_abs <- abs(qdf[,1:nbf])
                colnames(qdf_abs) <- paste0(colnames(qdf_abs),"_CF-quartimax")
                # V
                vdf <- as.data.frame(v$rotated)
                vdf$Method <- "CF-varimax"
                vdf_abs <- abs(vdf[,1:nbf])
                colnames(vdf_abs) <- paste0(colnames(vdf_abs),"_CF-varimax")
                # Dist
                d <- as.matrix(dist(t(cbind(qdf_abs,vdf_abs))))
                d <- as.data.frame(d)[-c(1:nbf),-c((nbf+1):(nbf+nbf))]
                collist <- list()
                for (i in 1:nbf){
                        collist[[i]] <- which.min(d[,i])
                }
                
                vdf_new <- vdf[,as.numeric(unlist(collist))]
                colnames(vdf_new) <- paste0("F",c(1:nbf),"_CF-varimax")
                qdf_new <- qdf
                colnames(qdf_new) <- paste0("F",c(1:nbf),"_CF-quartimax")
                Pointtable <- cbind(qdf_new[,1:nbf],vdf_new)
                #colnames(Pointtable) <- 
                row.names(Pointtable ) <- colnames(data)
#                 qm <- melt(q$rotatedse)
#                 qm$Method <- "CF-quartimax"
#                 qm$Var1 <- colnames(data)
#                 v$rotatedse <- v$rotatedse[,as.numeric(unlist(collist))]
#                 colnames(v$rotatedse) <- paste0("F",c(1:nbf))
#                 vm <- melt(v$rotatedse)
#                 vm$Method <- "CF-varimax"
#                 vm$Var1 <- colnames(data)
#                qv <- rbind(qm,vm)
#                 colnames(qv)[1:3] <-c("Item","Factor","SE") 
#                 qv$Item <- factor(qv$Item, levels = colnames(D))
#                 Fig <- ggplot(qv, aes(x = Item, y = SE, col = Method))+
#                         geom_point()+
#                         facet_grid(Factor~.)+
#                         scale_color_manual(values = c("chartreuse4","firebrick"))+
#                         theme_default()
        }
        #rst <- list()
        #rst[[1]] <- Pointtable
        #rst[[2]] <- Fig
        return(Pointtable)
}
