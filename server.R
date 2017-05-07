library(shiny)
library(corrplot)
library(psych)
source("faplot.R")

### VSS
### Style
### correlation matrix

shinyServer(function(input, output) {
        
        ### Argument names:
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
        
        ## Arg text field:
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
        
        
        ### Data import:
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
       # Dataset <- ifelse(is.null(input$vars) || length(input$vars)==0,Dataset[,input$vars,drop=FALSE],NULL)
        output$distPlot <- renderPlot({
                if (is.null(input$vars) || length(input$vars)==0){D <- NULL} 
                D <- Dataset()[,input$vars,drop=FALSE]
                if(input$cortype == "Pearson") {M <- cor(as.matrix(D))} 
                if(input$cortype == "tetrachoric"){
                        M <- tetrachoric(as.matrix(Dataset()))$rho
                }
                if(input$cortype == "polychoric"){
                        M <- polychoric(as.matrix(Dataset()))$rho
                }
                corrplot(M,order="AOE", method=input$method,type="upper",tl.pos="d")
                corrplot(M,add=TRUE, type="lower", method="number",order="AOE",
                         diag=FALSE,tl.pos="n", cl.pos="n")
        })
        output$nfPlot <- renderPlot({
                if (is.null(input$vars) || length(input$vars)==0){D <- NULL} 
                D<- Dataset()[,input$vars,drop=FALSE]
                if(input$cortype == "Pearson") {M <- cor(as.matrix(D))} 
                if(input$cortype == "tetrachoric"){M <- tetrachoric(as.matrix(D))$rho}
                if(input$cortype == "polychoric"){M <- polychoric(as.matrix(D))$rho}
                faplot(M,n.obs = input$nobs,quant = input$qpa, fm = input$fm, n.iter = input$npasim)
        })
        output$textfa <- renderTable({ 
                if (is.null(input$vars) || length(input$vars)==0){D <- NULL} 
                D<- Dataset()[,input$vars,drop=FALSE]
                if(input$cortype == "Pearson") {M <- cor(as.matrix(D))} 
                if(input$cortype == "tetrachoric"){M <- tetrachoric(as.matrix(D))$rho}
                if(input$cortype == "polychoric"){M < polychoric(as.matrix(D))$rho}
                farst <- fa(M,input$nfactors,n.obs =  input$nobs,
                            rotate = input$rotate,fm = input$fm )
                test <- cbind(unclass(farst$loadings),
                              melt(unclass(farst$communality)), # some problems
                              melt(unclass(farst$uniquenesses)),
                              melt(unclass(farst$complexity)))
                leng <- dim(unclass(farst$loadings))[2]
                colnames(test)[(leng+1):(leng+3)] <- c("h2","u2","com")
                #print(farst)
                print(farst$phi)
                return(test)
        },rownames = T)
        
        output$factcor <- renderTable({ 
                if (is.null(input$vars) || length(input$vars)==0){D <- NULL} 
                D<- Dataset()[,input$vars,drop=FALSE]
                if(input$cortype == "Pearson") {M <- cor(as.matrix(D))} 
                if(input$cortype == "tetrachoric"){M <- tetrachoric(as.matrix(D))$rho}
                if(input$cortype == "polychoric"){M < polychoric(as.matrix(D))$rho}
                farst <- fa(M,input$nfactors,n.obs =  input$nobs,
                            rotate = input$rotate,fm = input$fm )
                return(as.data.frame(unclass(farst$Phi))) ### print
        },rownames = T)
        
        
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


