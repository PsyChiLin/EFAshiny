library(shiny)
shinyUI(fluidPage(
        #theme = "bootstrap.css",
        # Header:
        #headerPanel(img(src="mish.png",)),
        titlePanel(title=div(img(src="mish.png", height = 40, width = 40),"Identify Latent Stuctures using the Exploratory Factor Analysis")),
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
                        #correlation matrix or
                        radioButtons('datatype', 'Type of Data',
                                     c("Raw Data Frame",
                                       "Correlation Matrix"),
                                     "Raw Data Frame"),
                        # Variable selection:
                        htmlOutput("varselect"),
                        br(),
                        #textInput("name","Dataset name:","Data"),
                        downloadLink('downloadSave_SelectData', 'Download Selected Data')
                        ),
                        mainPanel(tableOutput("table"))),
                #tabPanel("Exploration"),
                tabPanel("Data Explorations",
                         sidebarPanel(
                                 numericInput('nobs', "Number of Observations",263,min = 1),
                                 br(),
                                 numericInput("binsnum","Number of bins",10),
                                 br(),
                                 selectInput("cortype","Correlations Methods",
                                             c("Pearson", "tetrachoric", "polychoric")),
                                 selectInput("method","Display Methods",
                                             c("circle", "square", "ellipse",
                                               "number", "shade","color", "pie")),
                                 selectInput("rodermethod","Reorder Methods",
                                             c("original", "AOE", "FPC",
                                               "hclust", "alphabet"),"AOE"),
                                 downloadLink('downloadSave_NaR', 'Download NA Report'),
                                 br(),
                                 downloadLink('downloadSave_summary', 'Download Summary Table')
                        ),
                        mainPanel(navbarPage("",
                                             tabPanel("NA Report",tableOutput("na_table")),
                                             tabPanel("Summary",tableOutput("sum_table")),
                                             #tabPanel("NA detection"),
                                             tabPanel("Distribution", plotOutput("itemdist")),
                                             tabPanel("Response",plotOutput("itemplot")),
                                             tabPanel("Correlation Matrix", plotOutput("distPlot"))))
                ),
                tabPanel("Retention",
                         sidebarPanel(
                                 numericInput('qpa',"Quantile of Parallel analysis", 0.99 , min = 0 , max = 1,step = 0.1),
                                 numericInput("npasim","Number of simulated analyses to perform",20,min = 1, step = 10),
                                 br(),
                                 numericInput("maxn", "Max Number of Factor For VSS",4,min = 1),
                                 downloadLink('downloadSave_nfTable', 'Download VSS Table')),
                         mainPanel(navbarPage("",
                                              tabPanel("Sree Plot",plotOutput("nfPlot")),
                                              tabPanel("Very Simple Structure",tableOutput("nfTable"))))),
                tabPanel("Extraction and Rotation", 
                        sidebarPanel(
                        numericInput('nfactors', "Number of Factors", 2,min = 1),
                        selectInput("fm","Factor Extraction Methods",
                                    c("pa", "ml", "minres","uls", "wls",
                                      "gls", "minchi","minrank"),
                                    selected = "pa"),
                        selectInput("rotate","Rotation Methods",
                                    c("none",
                                      "varimax", "quartimax", "bentlerT",
                                      "equamax", "varimin", "geominT","bifactor",
                                      "Promax", "oblimin", "simplimax",
                                      "bentlerQ", "geominQ","biquartimin","cluster"),
                                    selected = "Promax"),
                        numericInput("bsnum","Number of Bootstraps",20, min = 20,step = 100),
                        checkboxInput("sorting", "Sort", T),
                        downloadLink('downloadSave_PatMat', 'Download Pattern Matrix'),
                        br(),
                        downloadLink('downloadSave_FactorCorr', 'Download Factor Correlations')),
                        mainPanel(navbarPage("",
                                   tabPanel("Patten Matrix", tableOutput("textfa")),
                                   tabPanel("Factors Correlations",tableOutput("factcor"))))
                        ),
                tabPanel("Diagram",
                         sidebarPanel(
                                 numericInput('cutt', "CutOff Value", 0.33,min = 0,step = 0.1), 
                                 checkboxInput("so", "Sort", T),
                                 checkboxInput("errarr", "Errors Arrows", T),
                                 checkboxInput("sim", "Simple Structure", T),
                                 #numericInput('rs', "size of rectangles",0.15,min = 0,step = 0.01),
                                 numericInput('es', "size of ellipses",0.05,min = 0,step = 0.01)

                         ),
                         mainPanel(plotOutput("Diag"))),
                tabPanel("Visualizations",
                         sidebarPanel(
                                 textInput("highcol","Color of Postive Loadings",value = "red"),
                                 textInput("lowcol","Color of Negative Loadings",value = "blue"),
                                 checkboxInput("sorting2", "Sort", T)
                         ),
                         mainPanel(navbarPage("",
                                              tabPanel("Bargraph",plotOutput("BFig")),
                                              tabPanel("StackedBar",plotOutput("SFig"))))
                         )
               )
))