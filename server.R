library(shiny)
library(corrplot)
library(psych)
library(reshape2)
library(ggplot2)
library(moments)
source("faplot.R")
## VSS
### Style
### correlation matrix

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
        
        ### Data Exploration
        
        # Reactive D
        D <- reactive({
                if (is.null(input$vars) || length(input$vars)==0){D <- NULL} else{     D <- Dataset()[,input$vars,drop=FALSE]}
                return(D)
        })
        # Reactive M
        M <- reactive({
                if(input$cortype == "Pearson") {M <- cor(as.matrix(D()))} 
                if(input$cortype == "tetrachoric"){M <- tetrachoric(D())$rho}
                if(input$cortype == "polychoric"){M <- polychoric(D())$rho}
                return(M)
        })
        output$sum_table <- renderTable({
                my_summary <- function(x) {
                        funs <- c(mean, sd, skewness, kurtosis)
                        sapply(funs, function(f)f(x, na.rm = TRUE))
                }
                dta_desc <- apply(D(),2,my_summary)
                row.names(dta_desc) <- c("Mean","SD","Skewness","Kurtosis")
                rst <- as.data.frame(t(dta_desc))
                round(rst,3)
        }, rownames = T)
        # distribution of itmes
        output$itemdist <- renderPlot({
                dtalong <- melt(D())
                colnames(dtalong) <- c("Item", "Response")
                ggplot(dtalong, aes(x = Response, fill = Item))+
                        geom_histogram()+
                        facet_wrap(~Item)+
                        theme_bw()+
                        labs(list(y = "Count"))
        })
        # Item Response
        output$itemplot <- renderPlot({
                dtbllist <- apply(D(),2,table)
                dtbl <- do.call(rbind, dtbllist)
                dtbll <- melt(dtbl)
                names(dtbll) <- c("Item","Response","Ratio")
                dtbll$Ratio <- dtbll$Ratio/input$nobs
                ggplot(dtbll,aes(x = reorder(Item, Ratio, max), 
                                 y = Ratio, ymin = 0.25, ymax = Ratio, color = Item))+
                        geom_pointrange()+
                        geom_hline(yintercept = 0.25, linetype = "dotted")+
                        facet_grid(.~ Response)+
                        coord_flip()+
                        labs(list(x = "Item"))+
                        theme_bw()
        })
        # Correlation Matrix
        output$distPlot <- renderPlot({
                corrplot(M(),order="AOE", method=input$method,type="upper",tl.pos="d")
                corrplot(M(),add=TRUE, type="lower", method="number",order="AOE",
                         diag=FALSE,tl.pos="n", cl.pos="n")
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
        
        output$textfa <- renderTable({ 
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
                return(test)
        },rownames = T)
        
        output$factcor <- renderTable({ 
                # plus ci
                return(as.data.frame(unclass(farst()$Phi))) ### print
        },rownames = T)
        
        
        #### Factor Diagram
        output$Diag <- renderPlot({
                #
                return(fa.diagram(farst()))
        })
        
        ### Download dump:
        
        output$downloadDump <- downloadHandler(
                filename = "Rdata.R",
                content = function(con) {
                        
                        assign(input$name, Dataset()[,input$vars,drop=FALSE])
                        
                        dump(input$name, con)
                }
        )
        
        ### Download save:
        
        output$downloadSave <- downloadHandler(
                filename = "Rdata.RData",
                content = function(con) {
                        
                        assign(input$name, Dataset()[,input$vars,drop=FALSE])
                        
                        save(list=input$name, file=con)
                }
        )
        
})


