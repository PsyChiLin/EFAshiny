library(shiny)
shinyUI(fluidPage(
        #theme = "bootstrap.css",
        # Header:
        #headerPanel(img(src="mish.png",)),
        titlePanel(title=div(img(src="mish.png", height = 40, width = 40),"Identify Latent Factors using Exploratory Factor Analysis")),
        # Input in sidepanel:
        tabsetPanel("Argument",
                tabPanel("Read Data",
                        sidebarPanel(
                        # HTML('<style type="text/css">.row-fluid .span4{width: 26%;}</style>'),
                        # Select filetype:
                        selectInput("readFunction", "Function to read data:", c("read.csv","read.table")),
                        # Argument selecter:
                        htmlOutput("ArgSelect"),
                        # Argument field:
                        htmlOutput("ArgText"),
                        # Upload data:
                        fileInput("file", "Upload data-file:"),
                        # correlation matrix or
                        #radioButtons('datatype', 'Type of Data',
                        #             c("Raw Data Frame",
                        #               "Correlation Matrix"),
                        #             "Raw Data Frame"),
                        #checkboxInput('corrrm', 'Correlation Matrix ?', FALSE),
                        # Variable selection:
                        htmlOutput("varselect"),
                        br()
                        #textInput("name","Dataset name:","Data"),
                        #downloadLink('downloadDump', 'Download source'),
                        #downloadLink('downloadSave', 'Download binary'),
                        ),
                        mainPanel(tableOutput("table"))),
                #tabPanel("Exploration"),
                tabPanel("Data Explorations",
                         sidebarPanel(
                                 numericInput('nobs', "Number of Observations", 300,min = 1),
                                 selectInput("cortype","Correlations Methods",
                                             c("Pearson", "tetrachoric", "polychoric")),
                                 selectInput("method","Display Methods",
                                             c("circle", "square", "ellipse",
                                               "number", "shade","color", "pie"))
                        ),
                        mainPanel(navbarPage("",
                                             tabPanel("Summary",tableOutput("sum_table")),
                                             tabPanel("Distribution", plotOutput("itemdist"),plotOutput("itemplot")),
                                             #tabPanel(,),
                                             tabPanel("Correlation Matrix", plotOutput("distPlot"))))
                ),
                tabPanel("Retention",
                         sidebarPanel(
                                 numericInput('qpa',"Quantile of Parallel analysis", 0.99 , min = 0 , max = 1),
                                 numericInput("npasim","Number of simulated analyses to perform",20,min = 1)
                                 ),
                         mainPanel(plotOutput("nfPlot"))),
                tabPanel("Extraction and Rotation", 
                        sidebarPanel(
                        numericInput('nfactors', "Number of Factors", 3,min = 1),
                        selectInput("fm","Factor Extraction Methods",
                                    c("pa", "ml", "minres","uls", "wls","gls", "minchi","minrank"),
                                    selected = "pa"),
                        selectInput("rotate","Rotation Methods",
                                    c("none",
                                      "varimax", "quartimax", "bentlerT",
                                      "equamax", "varimin", "geominT","bifactor",
                                      "Promax", "oblimin", "simplimax",
                                      "bentlerQ", "geominQ","biquartimin","cluster"),
                                    selected = "Promax")
                        ),
                        mainPanel(navbarPage("",
                                   tabPanel("Patten Matrix", tableOutput("textfa")),
                                   tabPanel("Factors Inter-correlation",tableOutput("factcor"))))
                        ))
))