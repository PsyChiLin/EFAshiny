library(shiny)
library(corrplot)
library(psych)
library(reshape2)
library(ggplot2)
library(moments)
library(grid)
library(gridExtra)

source("faplot.R")
source("bargraph.R")
source("printLoadings.R")
source("theme_default.R")
source("stackbar.R")
source("my_summary.R")
#source("fa.diagram.ext.R")

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
                        return(data.frame())
                }
                
                args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
                
                argList <- list()
                for (i in seq_along(args))
                {
                        argList[[i]] <- eval(parse(text=input[[args[i]]]))
                }
                names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
                
                argList <- argList[names(argList) %in% ArgNames()]
                
                Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
                return(Dataset)
        })
        # Select variables:
        output$varselect <- renderUI({
                
                if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
                
                # Variable selection:    
                selectInput("vars", "Variables to use:",
                            names(Dataset()), names(Dataset()), multiple =TRUE)            
        })
        # Show table:
        output$table <- renderTable({
                if (is.null(input$vars) || length(input$vars)==0) return(NULL)
                return(Dataset()[,input$vars,drop=FALSE])
        },rownames = T)
        output$downloadSave_SelectData <- downloadHandler(
                filename = "SelectedData.csv",
                content = function(file) {
                        write.csv(Dataset()[,input$vars,drop=FALSE], file,row.names=FALSE)
                }
        )
        ### Data Exploration
        
        # NA detetcion
        NaR <- reactive({
                dta <- Dataset()[,input$vars,drop=FALSE]
                NAreport <- as.data.frame(apply(apply(dta,2,is.na),2,sum)/input$nobs)
                colnames(NAreport) <- "NA Ratio"
                return(NAreport)})
        output$na_table <- renderTable({print(NaR())},rownames = T)
        output$downloadSave_NaR <- downloadHandler(
                filename = "NaReport.csv",
                content = function(file) {
                        write.csv(NaR(),file,row.names = T)
                }
        )
        # Reactive D
        D <- reactive({
                if (is.null(input$vars) || length(input$vars)==0){D <- NULL} else{     D <- Dataset()[,input$vars,drop=FALSE]}
                D <- na.omit(D)
                return(D)
        })
        # Reactive M
        M <- reactive({
                if(input$cortype == "Pearson") {M <- cor(as.matrix(D()))} 
                if(input$cortype == "tetrachoric"){M <- tetrachoric(D())$rho}
                if(input$cortype == "polychoric"){M <- polychoric(D())$rho}
                return(M)
        })
        # Reactive Summary For Download
        SumTable <- reactive({
                dta_desc <- apply(D(),2,my_summary)
                row.names(dta_desc) <- c("Mean","SD","Skewness","Kurtosis")
                rst <- as.data.frame(t(dta_desc))
                rst <- round(rst,3)
                return(rst)
        })
        output$sum_table <- renderTable({print(SumTable())}, rownames = T)
        output$downloadSave_summary <- downloadHandler(
                filename = "Summary.csv",
                content = function(file) {
                        write.csv(SumTable(),file,row.names = T)
                }
        )
        # distribution of itmes
        output$itemdist <- renderPlot({
                dtalong <- melt(D())
                colnames(dtalong) <- c("Item", "Response")
                ggplot(dtalong, aes(x = Response, fill = Item))+
                        geom_histogram(bins = input$binsnum)+
                        facet_wrap(~Item)+
                        theme_default()+
                        labs(list(y = "Count"))
        })
        # Item Response
        output$itemplot <- renderPlot({
                dtbl <- apply(D(),2,table)
                d <- D()
                #dtbl <- apply(D1,2,table)
                if (class(dtbl) == "list"){
                        dtbl <- do.call(rbind, dtbl)
                        dtbll <- melt(dtbl)
                        names(dtbll) <- c("Item","Response","Ratio")
                        dtbll$Ratio <- dtbll$Ratio/input$nobs}
                else {
                        dtbll <- melt(dtbl)
                        names(dtbll) <- c("Response","Item","Ratio")
                        dtbll$Ratio <- dtbll$Ratio/input$nobs}
                ggplot(dtbll,aes(x = reorder(Item, Ratio, max), 
                                 y = Ratio, ymin = 0.25, ymax = Ratio, color = Item))+
                        geom_pointrange()+
                        geom_hline(yintercept = 0.25, linetype = "dotted")+
                        facet_grid(. ~ Response)+
                        coord_flip()+
                        labs(list(x = "Item"))+
                        theme_default()
        })
        # Correlation Matrix
        output$distPlot <- renderPlot({
                corrplot(M(),order=input$rodermethod, method=input$method,type="upper",tl.pos = "lt")
                corrplot(M(),add=TRUE, type="lower",
                         method="number",order=input$rodermethod,
                         diag=FALSE,
                         tl.pos="n", cl.pos="n")
        }) 
       
         ### Factor Retention
        output$nfPlot <- renderPlot({
                faplot(M(),n.obs = input$nobs,quant = input$qpa, fm = input$fm, n.iter = input$npasim)
                #VSS(sim.item(nvar=24),fm="minres",plot = F)[,c(1,2,3,6,7,8,11)]
        })
        
        ### Factor Extraction and Rotation
        # reactive factor analysis Results
        farst <- reactive({
                farst <- fa(M(),input$nfactors,n.obs =  input$nobs,
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
                o2 <- printLoadings(o1$cis$means,sort = input$sorting2)
                o3 <- as.data.frame(o2)
                return(row.names(o3))
        }) # for bargraph
        itemorder3 <- reactive({
                o1 <- farst()
                o2 <- printLoadings(o1$cis$means,sort = T)
                o3 <- as.data.frame(o2)
                return(row.names(o3))
        }) # For stack MuST be TRUE
        PatMat_ci <- reactive({
                farst <- farst()
                f <- list()
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
        output$downloadSave_FactorCorr <- downloadHandler(
                filename = "FactorCorr.csv",
                content = function(file) {
                        write.csv(FactCorr(),file,row.names = T)
                }
        )
        ### Factor Bargraph
        output$BFig <- renderPlot({
                order <- itemorder2()
                return(bargraph(farst(),order = order,highcol = input$highcol,lowcol = input$lowcol))
        })
        ### Factor stackBar
        output$SFig <- renderPlot({
                order <- itemorder3()
                return(stackbar(M(),farst(),order = order))
        })
        ### Factor Diagram
        output$Diag <- renderPlot({
                return(fa.diagram(farst()))
        })
})


