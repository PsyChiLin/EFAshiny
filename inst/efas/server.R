if(!require(shiny)) {require(shiny)}
if(!require(shinythemes)) {require(shinythemes)}
if(!require(ggplot2)) {require(ggplot2)}
if(!require(psych)) {require(psych)}
if(!require(EFAutilities)) {require(EFAutilities)}
if(!require(corrplot)) {require(corrplot)}
if(!require(reshape2)) {require(reshape2)}
if(!require(moments)) {require(moments)}
if(!require(gridExtra)) {require(gridExtra)}
if(!require(qgraph)) {require(qgraph)}
if(!require(bootnet)) {require(bootnet)}
if(!require(igraph)) {require(igraph)}

options(shiny.sanitize.errors = FALSE)
file.sources <- list.files(path = "functions/",pattern="*.R")
RSE <- read.csv("data/RSE_naomit.csv")
sapply(paste0("functions/",file.sources),source)

shinyServer(function(input, output) {
        Dataset <- reactive({
                if (is.null(input$file)) {
                        set.seed(100)
                        dst <- RSE[sort(sample(1: 46546, 256)),1:10]
                        row.names(dst) <- 1:256
                        return(dst)
                }
                if (input$dataformat == "txt"){
                        if (input$hdr == "TRUE"){ Dataset <- read.table(input$file$datapath, header = T)}
                        if (input$hdr == "FALSE"){ Dataset <- read.table(input$file$datapath, header = F)}
                }
                if (input$dataformat == "csv"){
                        if (input$hdr == "TRUE"){ Dataset <- read.csv(input$file$datapath, header = T)}
                        if (input$hdr == "FALSE"){ Dataset <- read.csv(input$file$datapath, header = F)}
                }
                if (input$datatype == "Correlation Matrix"){
                        row.names(Dataset) <- Dataset[,1]
                        Dataset <- Dataset[,-1]
                }
                Dataset <- na.omit(Dataset)
                
                return(Dataset)
        })
        output$varselect <- renderUI({
                if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
                selectInput("vars", "Variables to include:",
                            names(Dataset()), names(Dataset()), multiple =TRUE)
                       
        })
        output$Nselect <- renderUI({
                if (input$datatype == "Correlation Matrix"){nobss <- input$nobs}
                if (input$datatype == "Raw Data"){nobss <- dim(Dataset())[1] }
                sliderInput("Nselect", "Sample Size", 1, nobss , nobss,
                            step = 1, round = FALSE,
                            format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                            width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                            timezone = NULL, dragRange = TRUE)
        })
        D <- reactive({
                if (is.null(input$vars) || length(input$vars)==0){
                        D <- NULL}
                else{if(input$datatype == "Correlation Matrix"){D <- Dataset()[input$vars,input$vars,drop=FALSE]}
                        else{D <- Dataset()[,input$vars,drop=FALSE]}}
                return(as.data.frame(D))
        })
        # Show table
        output$table <- renderTable({return(D())},rownames = T)
        output$downloadSave_SelectData <- downloadHandler(filename = "SelectedData.csv",content = function(file) {write.csv(D()[,input$vars,drop=FALSE], file,row.names=FALSE)})
        # Reactive M
        M <- reactive({
                if(input$datatype == "Correlation Matrix"){M <- as.matrix(D())}
                if(input$datatype == "Raw Data"){
                        if(input$cortype == "Pearson") {M <- cor(as.matrix(D()))} 
                        if(input$cortype == "tetrachoric"){M <- tetrachoric(D())$rho}
                        if(input$cortype == "polychoric"){M <- polychoric(D())$rho}
                }
                return(M)
        })
        # Reactive Summary For Download
        SumTable <- reactive({
                if (input$datatype == "Correlation Matrix") {stop("The numeric summary is not applicable for a corrleation matrix input.")}
                if (!all(sapply(D(),class) %in% c("numeric","integer"))) { stop("All input variables should be numeric or integer")}
                if (input$datatype == "Raw Data"){
                dta_desc <- apply(D(),2,my_summary)
                row.names(dta_desc) <- c("Mean","SD","Skewness","Kurtosis","Median","MAD")
                rst <- as.data.frame(t(dta_desc))
                rst <- round(rst,3)
                return(rst)
                }
        })
        output$sum_table <- renderTable({print(SumTable())}, rownames = T)
        output$downloadSave_summary <- downloadHandler(filename = "Summary.csv",content = function(file) {
                        write.csv(SumTable(),file,row.names = T)
                })
        # distribution of itmes
        observe(output$itemdist <- renderPlot({
                if (input$datatype == "Correlation Matrix") {stop("The distribution plot is not applicable for a corrleation matrix input.")}
                if (!all(sapply(D(),class) %in% c("numeric","integer"))) { stop("All input variables should be numeric or integer")}
                        dtalong <- melt(D())
                        colnames(dtalong) <- c("Item", "Response")
                        ggplot(dtalong, aes(x = Response, fill = Item))+
                                geom_histogram(bins = 10)+
                                facet_wrap(~Item)+
                                theme_default()+
                                labs(list(y = "Count"))
        },height = input$ploth1,width = input$plotw1))
        # Correlation Matrix
        observe(output$distPlot <- renderPlot({
                corrplot(M(),order=input$rodermethod, method="ellipse",type="upper",tl.pos = "lt")
                corrplot(M(),add=TRUE, type="lower",
                         method="number",order="hclust",
                         diag=FALSE,
                         tl.pos="n", cl.pos="n")
        },height = input$ploth1,width = input$plotw1)) 
        
        ### Factor Retention
        # Select variables:
        output$Nselect <- renderUI({
                sliderInput("Nselect", "Sample Size", 1, dim(D())[1] ,dim(D())[1],
                            step = 1, round = FALSE,
                            format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                            width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                            timezone = NULL, dragRange = TRUE)
        })
        # Reactive D
        D2 <- reactive({
                if (is.null(input$vars) || length(input$vars)==0){
                        D2 <- NULL}
                else{if(input$datatype == "Correlation Matrix"){D2 <- Dataset()[input$vars,input$vars,drop=FALSE]}
                        else{D2 <- Dataset()[sort(sample(1:dim(Dataset())[1],input$Nselect)),input$vars,drop=FALSE]}}
                return(as.data.frame(D2))
        })
        # Reactive M
        M2 <- reactive({
                if(input$datatype == "Correlation Matrix"){M2 <- M()}
                if(input$datatype == "Raw Data"){
                        if(input$cortype == "Pearson") {M2 <- cor(as.matrix(D2()))} 
                        if(input$cortype == "tetrachoric"){M2 <- tetrachoric(D2())$rho}
                        if(input$cortype == "polychoric"){M2 <- polychoric(D2())$rho}
                }
                return(M2)
        })
        observe(output$nfPlot <- renderPlot({
                try(
                faplot(M2(),n.obs = ifelse(input$datatype == "Correlation Matrix",input$nobs,input$Nselect),
                       quant = as.numeric(input$qpa), fm = input$fm,
                       n.iter = input$npasim))
                },height = input$ploth2,width = input$plotw2))
        VssTable <- reactive({
                Vs<- VSS(M2(),n = input$maxn,
                         plot = F, n.obs = ifelse(input$datatype == "Correlation Matrix",input$nobs,input$Nselect))
                mapvss <- data.frame(nFactor = row.names(Vs$vss.stats),
                                     MAP = Vs$map)
                otherindex <- Vs$vss.stats[,c(6:8,11)]
                VssTable <- cbind(mapvss,otherindex)
                return(VssTable)
        })
        observe(output$EGAplot <- renderPlot({
                if(input$datatype == "Correlation Matrix"){stop("The EGA is not applicable for a corrleation matrix input.")}
                bootEGA(data = D2(), n = input$npasim, 
                                                      medianStructure = TRUE, 
                                                      plot.MedianStructure = TRUE, 
                                                      ncores = 4)},
                                             height = input$ploth2,width = input$plotw2))
        output$nfTable <- renderTable({print(VssTable())},rownames = F)
        output$downloadSave_nfTable <- downloadHandler(filename = "Vss_Table.csv",content = function(file) {
                write.csv(VssTable(),file,row.names = F)
        })
        ### Factor Extraction and Rotation
        # reactive factor analysis Results
        farst <- reactive({
                farst <- fa(M2(),input$nfactors,n.obs =  ifelse(input$datatype == "Correlation Matrix",input$nobs,input$Nselect),
                            rotate = input$rotate,fm = input$fm,
                            max.iter = 100000,n.iter = input$bsnum)
                return(farst)
        })
        itemorder <- reactive({
                o1 <- farst()
                o2 <- printLoadings(o1$cis$means,sort = input$sorting,cutoff = 0)
                o3 <- as.data.frame(o2)
                return(row.names(o3))
        }) 
        # for pattren matrix
        itemorder2 <- reactive({
                o1 <- farst()
                if (input$nfactors != 1) {o2 <- printLoadings(o1$cis$means,sort = input$sorting2,cutoff = 0)}
                if (input$nfactors == 1) {
                        if (input$sorting2 == T) {o2 <- o1$cis$means[order(o1$cis$means[,1],decreasing = T),]}
                        if (input$sorting2 == F) {o2 <- unclass(o1$cis$means)}
                }
                o3 <- as.data.frame(o2)
                return(rev(row.names(o3)))
        }) 
        # for bargraph and stackedbar
        PatMat_ci <- reactive({
                farst <- farst()
                f <- list()
                if(input$nfactors != 1){
                        for (i in 1:input$nfactors){f[[i]] <- cbind(farst$cis$ci[,i],farst$cis$means[,i],farst$cis$ci[,i+input$nfactors])}
                        fd <- do.call(cbind,f)
                        test <- cbind(fd,
                                      melt(unclass(farst$communality)), # some problems
                                      melt(unclass(farst$uniquenesses)),
                                      melt(unclass(farst$complexity)))
                        nam <- list()        
                        aa <- names(as.data.frame(unclass(farst$cis$means)))
                        for (i in 1:input$nfactors){nam[[i]] <- c("LB",aa[i],"UB")}   #for (i in 1:input$nfactors){nam[[i]] <- c(paste0(aa[i],"_LB"),aa[i],paste0(aa[i],"_UB"))} 
                        manc <- do.call(c,nam)        
                        colnames(test)[1:I(input$nfactors *3)] <- manc
                        colnames(test)[I(input$nfactors *3+1):I(input$nfactors *3+3)] <- c("h2","u2","com")
                        order <- itemorder()
                        test <- test[c(order),]
                }
                if (input$nfactors == 1){
                        f<- cbind(farst$cis$ci[,1],farst$cis$means[,1],farst$cis$ci[,2])
                        test <- cbind(f,
                                      melt(unclass(farst$communality)), # some problems
                                      melt(unclass(farst$uniquenesses)),
                                      melt(unclass(farst$complexity)))
                        nam <- list()        
                        aa <- names(as.data.frame(unclass(farst$cis$means)))
                        nam <- c(paste0(aa[1],"_Lower"),aa[1],paste0(aa[1],"_Upper"))      
                        colnames(test)[1:I(1*3)] <- nam
                        colnames(test)[I(1*3+1):I(1*3+3)] <- c("h2","u2","com")
                        if(input$sorting == T){test <- test[order(test[,2],decreasing = T),]}
                        
                }
                return(test)
        }) 
        output$textfa <- renderTable({print(PatMat_ci())},rownames = T)
        output$downloadSave_PatMat <- downloadHandler(filename = "PatternMatrix.csv",content = function(file) {
                        write.csv(PatMat_ci(),file,row.names = T)
                })
        FactCorr <- reactive({
                FactCorr <- as.data.frame(unclass(farst()$Phi))
                return(FactCorr)
        })
        output$factcor <- renderTable({print(FactCorr())},rownames = T) # Ci ?
        output$downloadSave_FactorCorr <- downloadHandler(filename = "FactorCorr.csv",content = function(file) {
                        write.csv(FactCorr(),file,row.names = T)
                })
        ### Factor Diagram
        observe(output$Diag <- renderPlot({
                return(fa.diagram(farst(),
                                  simple = input$sim,
                                  cut = input$cutt,
                                  sort = input$so,
                                  errors = input$errarr,
                                  main = " ",
                                  #rsize = input$rs,
                                  e.size = input$es))
                
        },height = input$ploth3,width = input$plotw3))
        ### Factor Bargraph
        observe(output$BFig <- renderPlot({
                order <- itemorder2()
                return(bargraph(farst(),order = order,nf = input$nfactors,highcol = input$highcol,lowcol = input$lowcol,ci = input$barci))
        },height = input$ploth5,width = input$plotw5))
        ### Factor stackBar
        observe(output$SFig <- renderPlot({
                order <- itemorder2()
                return(stackbar(M2(),farst(),order = order,highcol = input$highcol,lowcol = input$lowcol))
        },height = input$ploth4,width = input$plotw4))
        ### SE Investigation
        q <- reactive({
                return(efa(covmat =M2(), n.obs = ifelse(input$datatype == "Correlation Matrix",input$nobs,input$Nselect),
                           factors=input$nfactors, 
                           #dist='ordinal',fm='ml',
                           rotation='CF-quartimax', 
                           merror='YES'))})
        v <- reactive({return(efa(covmat =M2(),n.obs = ifelse(input$datatype == "Correlation Matrix",input$nobs,input$Nselect), 
                                  factors=input$nfactors, 
                                  #dist='ordinal',fm='ml',
                                  rotation='CF-varimax',   
                                  merror='YES'))})
        output$PointTable <- renderTable(print(PointT_new(q(),v(),M2(),nbf = input$nfactors)),rownames = T)
        observe(output$SEFig <- renderPlot({return(SEplot(q(),v(),M2(),nbf = input$nfactors))},
        height = input$ploth6,
        width = input$plotw6))

        
})

