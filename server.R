library(shiny)
library(corrplot)
library(psych)
library(reshape2)
library(ggplot2)
library(moments)
library(grid)
library(gridExtra)
library(shinythemes)
library(EFAutilities)
options(shiny.sanitize.errors = FALSE)
file.sources = list.files(path = "functions/",pattern="*.R")
RSE <- read.csv("data/RSE_naomit.csv")
sapply(paste0("functions/",file.sources),source)


shinyServer(function(input, output) {
        
        ### Read Data 
        
        # Argument names:
        ArgNames <- reactive({
                Names <- names(formals(input$readFunction)[-1])
                Names <- Names[Names!="..."]
                return(Names)
        })
        # Argument selector:
        output$ArgSelect <- renderUI({
                if (length(ArgNames())==0) return(NULL)
                selectInput("arg","Argument:",ArgNames())
        })
        # Arg text field:
        output$ArgText <- renderUI({
                fun__arg <- paste0(input$readFunction,"__",input$arg)
                if (is.null(input$arg)) return(NULL)
                Defaults <- formals(input$readFunction)
                if (is.null(input[[fun__arg]]))
                {
                        textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
                } else {
                        textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
                }
        })
        # Data import:
        Dataset <- reactive({
                if (is.null(input$file)) {
                        # User has not uploaded a file yet
                        return(RSE[,1:10])
                }
                args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
                argList <- list()
                for (i in seq_along(args)){argList[[i]] <- eval(parse(text=input[[args[i]]]))}
                names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
                argList <- argList[names(argList) %in% ArgNames()]
                Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
                if (input$datatype == "Correlation Matrix"){
                        row.names(Dataset) <- Dataset[,1]
                        Dataset <- Dataset[,-1]
                }
                Dataset <- na.omit(Dataset)
                
                return(Dataset)
        })
        #if (is.null(Dataset)){Dataset <- RSE}
        # Select variables:
        output$varselect <- renderUI({
                if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
                # Variable selection:    
                selectInput("vars", "Variables to use:",
                            names(Dataset()), names(Dataset()), multiple =TRUE)
                       
        })
        # Select variables:
        output$Nselect <- renderUI({
                if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
                # Variable selection:    
                #selectInput("vars", "Variables to use:",
                #            names(Dataset()), names(Dataset()), multiple =TRUE)
                sliderInput("Nselect", "Number of Observations to use", 1, dim(Dataset())[1], 256,
                            step = 1, round = FALSE,
                            format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                            width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                            timezone = NULL, dragRange = TRUE)
        })
        
        # Reactive D
        D <- reactive({
                if (is.null(input$vars) || length(input$vars)==0){
                        D <- NULL}
                else{if(input$datatype == "Correlation Matrix"){D <- Dataset()[input$vars,input$vars,drop=FALSE]}
                        else{D <- Dataset()[sample(dim(Dataset())[1],input$Nselect),input$vars,drop=FALSE]}}
                return(D)
        })

        #D <- na.omit(D)
        # Show table:
        output$table <- renderTable({
                #if (input$datatype == "Raw Data Frame"){
                #        if (is.null(input$vars) || length(input$vars)==0) return(NULL)
                #        return(Dataset()[,input$vars,drop=FALSE])
                #}
                #if (input$datatype == "Correlation Matrix"){
                #        if (is.null(input$vars) || length(input$vars)==0) return(NULL)
                #        return(Dataset()[input$vars,input$vars,drop=FALSE])
                #}
                return(D())
                
        },rownames = T)
        
        output$downloadSave_SelectData <- downloadHandler(filename = "SelectedData.csv",content = function(file) {write.csv(Dataset()[,input$vars,drop=FALSE], file,row.names=FALSE)})
        ### Data Exploration
        
        ## NA detetcion
        #NaR <- reactive({
        #        dta <- Dataset()[,input$vars,drop=FALSE]
        #        NAreport <- as.data.frame(apply(apply(dta,2,is.na),2,sum)/input$Nselect)
        #        colnames(NAreport) <- "NA Ratio"
        #        return(NAreport)})
        #output$na_table <- renderTable({
        #        if (input$datatype == "Correlation Matrix") { stop("Your input is a correlation matrix. Could not detect the NA response.")}
        #        print(NaR())
        #        },rownames = T)
        #output$downloadSave_NaR <- downloadHandler(filename = "NaReport.csv",content = function(file) {
        #                write.csv(NaR(),file,row.names = T)
        #        })
        
        
        # Reactive M
        M <- reactive({
                if(input$datatype == "Correlation Matrix"){M <- as.matrix(D())}
                if(input$datatype == "Raw Data Frame"){
                        if(input$cortype == "Pearson") {M <- cor(as.matrix(D()))} 
                        if(input$cortype == "tetrachoric"){M <- tetrachoric(D())$rho}
                        if(input$cortype == "polychoric"){M <- polychoric(D())$rho}
                }
                return(M)
        })
        # Reactive Summary For Download
        SumTable <- reactive({
                if (input$datatype == "Correlation Matrix") { stop("Your input is a correlation matrix. Could not show the response summary of each item.")}
                if (!all(sapply(D(),class) %in% c("numeric","integer"))) { stop("All input variables should be numeric or integer")}
                dta_desc <- apply(D(),2,my_summary)
                row.names(dta_desc) <- c("Mean","SD","Skewness","Kurtosis")
                rst <- as.data.frame(t(dta_desc))
                rst <- round(rst,3)
                return(rst)
        })
        output$sum_table <- renderTable({print(SumTable())}, rownames = T)
        output$downloadSave_summary <- downloadHandler(filename = "Summary.csv",content = function(file) {
                        write.csv(SumTable(),file,row.names = T)
                })
        # distribution of itmes
        observe(output$itemdist <- renderPlot({
                if (input$datatype == "Correlation Matrix") { stop("Your input is a correlation matrix. Could not show the response distribution of each item.")}
                if (!all(sapply(D(),class) %in% c("numeric","integer"))) { stop("All input variables should be numeric or integer")}
                dtalong <- melt(D())
                colnames(dtalong) <- c("Item", "Response")
                ggplot(dtalong, aes(x = Response, fill = Item))+
                        geom_histogram(bins = input$binsnum)+
                        facet_wrap(~Item)+
                        theme_default()+
                        labs(list(y = "Count"))
        },height = input$ploth1,width = input$plotw1))
        # Item Response
        observe(output$itemplot <- renderPlot({
                if (input$datatype == "Correlation Matrix") { stop("Your input is a correlation matrix. Could not show the response distribution of each item.")}
                if (!all(sapply(D(),class) %in% c("numeric","integer"))) { stop("All input variables should be numeric or integer")}
                dtbl <- apply(D(),2,table)
                d <- D()
                #dtbl <- apply(D1,2,table)
                if (class(dtbl) == "list"){
                        dtbl <- do.call(rbind, dtbl)
                        dtbll <- melt(dtbl)
                        names(dtbll) <- c("Item","Response","Ratio")
                        dtbll$Ratio <- dtbll$Ratio/input$Nselect
                        if (length(levels(as.factor(dtbll$Response))) > 20) {
                                stop("More than 20 levels response. Continuous response data Could not be shwon in this plot ")
                        }}
                else {
                        dtbll <- melt(dtbl)
                        names(dtbll) <- c("Response","Item","Ratio")
                        dtbll$Ratio <- dtbll$Ratio/input$Nselect}
                ggplot(dtbll,aes(x = reorder(Item, Ratio, max), 
                                 y = Ratio, ymin = 0.25, ymax = Ratio, color = Item))+
                        geom_pointrange()+
                        geom_hline(yintercept = 0.25, linetype = "dotted")+
                        facet_grid(. ~ Response)+
                        coord_flip()+
                        labs(list(x = "Item"))+
                        theme_default()
        },height = input$ploth1,width = input$plotw1))
        # Correlation Matrix
        observe(output$distPlot <- renderPlot({
                corrplot(M(),order=input$rodermethod, method=input$method,type="upper",tl.pos = "lt")
                corrplot(M(),add=TRUE, type="lower",
                         method="number",order=input$rodermethod,
                         diag=FALSE,
                         tl.pos="n", cl.pos="n")
        },height = input$ploth1,width = input$plotw1)) 
         ### Factor Retention
        observe(output$nfPlot <- renderPlot({faplot(M(),n.obs = input$Nselect,quant = input$qpa, fm = input$fm, n.iter = input$npasim)},height = input$ploth2,width = input$plotw2))
        VssTable <- reactive({
                Vs<- VSS(M(),n = input$maxn,
                         #rotate = "promax", fm = "ml",
                         plot = F, n.obs = input$Nselect)
                mapvss <- data.frame(nFactor = row.names(Vs$vss.stats),VSS1 = Vs$cfit.1, VSS2 = Vs$cfit.2, MAP = Vs$map)
                otherindex <- Vs$vss.stats[,c(1:14)]
                VssTable <- cbind(mapvss,otherindex)
                return(VssTable)
        })
        output$nfTable <- renderTable({print(VssTable())},rownames = F)
        output$downloadSave_nfTable <- downloadHandler(filename = "Vss_Table.csv",content = function(file) {
                write.csv(VssTable(),file,row.names = F)
        })
        ### Factor Extraction and Rotation
        # reactive factor analysis Results
        farst <- reactive({
                farst <- fa(M(),input$nfactors,n.obs =  input$Nselect,
                            rotate = input$rotate,fm = input$fm,
                            max.iter = 100000,n.iter = input$bsnum)
                return(farst)
        })
        itemorder <- reactive({
                o1 <- farst()
                o2 <- printLoadings(o1$cis$means,sort = input$sorting,cutoff = 0)
                o3 <- as.data.frame(o2)
                return(row.names(o3))
        }) # for pattren matrix
        itemorder2 <- reactive({
                o1 <- farst()
                if (input$nfactors != 1) {o2 <- printLoadings(o1$cis$means,sort = input$sorting2,cutoff = 0)}
                if (input$nfactors == 1) {
                        if (input$sorting2 == T) {o2 <- o1$cis$means[order(o1$cis$means[,1],decreasing = T),]}
                        if (input$sorting2 == F) {o2 <- unclass(o1$cis$means)}
                }
                o3 <- as.data.frame(o2)
                return(rev(row.names(o3)))
        }) # for bargraph and stackedbar
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
                        for (i in 1:input$nfactors){nam[[i]] <- c(paste0(aa[i],"_Lower"),aa[i],paste0(aa[i],"_Upper"))}       
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
        },height = input$ploth4,width = input$plotw4))
        ### Factor stackBar
        observe(output$SFig <- renderPlot({
                order <- itemorder2()
                return(stackbar(M(),farst(),order = order,highcol = input$highcol,lowcol = input$lowcol))
        },height = input$ploth4,width = input$plotw4))
        ### SE Investigation
        q <- reactive({efa(x=D(), factors=input$nfactors, dist='ordinal',fm='ml',rotation='CF-quartimax', merror='YES')})
        v <- reactive({efa(x=D(), factors=input$nfactors, dist='ordinal',fm='ml',rotation='CF-varimax', merror='YES')})
        output$PointTable <- renderTable(print(PointT(q(),v(),D())),rownames = T)
        output$SETable <- renderTable(print(SET(q(),v(),D())),rownames = T)
        observe(output$SEFig <- renderPlot({
                #order <- itemorder2()
                return(SEplot(q(),v(),D()))
        },height = input$ploth5,width = input$plotw5))

        
})


