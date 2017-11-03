library(shiny)
library(shinythemes)
shinyUI(fluidPage(
        theme = shinytheme("flatly"),
        #theme = "bootstrap.css",
        # Header:
        #headerPanel(img(src="mish.png",)),
        #titlePanel(title=div(img(src="mish.png", height = 40, width = 40),"Identify Latent Stuctures using the Exploratory Factor Analysis")),
        # Input in sidepanel:
        navbarPage("EFAshiny",inverse = F, fluid = T,#type = "pills",
                   tabPanel("Introduction"),
                   tabPanel("Demo"),
                   tabPanel("Data Input",
                        sidebarPanel(width = 3,
                        # HTML('<style type="text/css">.row-fluid .span4{width: 26%;}</style>'),
                        # Select filetype:
                        #selectInput("readFunction", "Function to read data:", c("read.csv","read.table")),
                        # Argument selecter:
                        #htmlOutput("ArgSelect"),
                        # Argument field:
                        #htmlOutput("ArgText"),
                        # Upload data:
                        fileInput("file", "Upload data-file:"),
                        radioButtons('dataformat', 'Data Format',
                                     c("csv",
                                       "txt"),
                                     "csv"),
                        radioButtons('hdr', "Header of variable",
                                     c("TRUE","FALSE"),"TRUE"),
                        #correlation matrix or
                        radioButtons('datatype', 'Type of Data',
                                     c("Raw Data",
                                       "Correlation Matrix"),
                                     "Raw Data"),
                        # Variable selection:
                        htmlOutput("varselect"),
                        #htmlOutput("Nselect"),
                        br(),
                        #textInput("name","Dataset name:","Data"),
                        downloadLink('downloadSave_SelectData', 'Download Selected Data')
                        ),
                        mainPanel(tableOutput("table"))),
                #tabPanel("Exploration"),
                tabPanel("Data Summary",
                         sidebarPanel(width = 3,
                                 #numericInput('nobs', "Number of Observations",263,min = 1),
                                 #br(),
                                 #numericInput("binsnum","Number of bins",10),
                                 #br(),
                                 selectInput("cortype","Type of Correlations",
                                             c("Pearson", "tetrachoric", "polychoric")),
                                 #selectInput("method","Display Methods",
                                 #            c("circle", "square", "ellipse",
                                 #              "number", "shade","color", "pie")),
                                 #selectInput("rodermethod","Reorder Methods",
                                 #            c("original", "AOE", "FPC",
                                 #              "hclust", "alphabet"),"AOE"),
                                 br(),
                                 numericInput("ploth1", " Plot Height",800,min = 1),
                                 numericInput("plotw1", " Plot Width",800,min = 1),
                                 #downloadLink('downloadSave_NaR', 'Download NA Report'),
                                 br(),
                                 downloadLink('downloadSave_summary', 'Download Summary Table')
                        ),
                        mainPanel(tabsetPanel("",
                                             #tabPanel("NA Report",tableOutput("na_table")),
                                             tabPanel("Numeric Statistic",tableOutput("sum_table")),
                                             #tabPanel("NA detection"),
                                             tabPanel("Distribution", plotOutput("itemdist")),
                                             #tabPanel("Response",plotOutput("itemplot")),
                                             tabPanel("Correlation Matrix", plotOutput("distPlot"))))
                ),
                tabPanel("Factor Retention",
                         sidebarPanel(width = 3,
                                      selectInput("FRmethod", "Method of Factor Retention", 
                                                  c("Scree Plot and Parallel Analysis",
                                                    "Numeric Rules",
                                                    "Exploratory Graph Analysis"),
                                                  "Scree Plot and Parallel Analysis"),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Scree Plot and Parallel Analysis'",
                                              numericInput('qpa',"Quantile of Parallel analysis", 0.99 , min = 0 , max = 1,step = 0.1),
                                              numericInput("npasim","Number of simulated analyses to perform",200,min = 1, step = 10),
                                              #br(),
                                              #htmlOutput("Nselect"),
                                              br(),
                                              numericInput("ploth2", " Plot Height",300,min = 1),
                                              numericInput("plotw2", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Numeric Rules'",
                                              numericInput("maxn", "Max Number of Factor For Estimation",4,min = 1)
                                              #br(),
                                              #htmlOutput("Nselect")
                                              #numericInput('qpa',"Quantile of Parallel analysis", 0.99 , min = 0 , max = 1,step = 0.1),
                                              #numericInput("npasim","Number of simulated analyses to perform",200,min = 1, step = 10),
                                              #numericInput("ploth2", " Plot Height",300,min = 1),
                                              #numericInput("plotw2", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Exploratory Graph Analysis'",
                                              numericInput("npasim","Number of simulated analyses to perform",10,min = 1,max = 1000, step = 10),
                                              br(),
                                              #htmlOutput("Nselect"),
                                              #br(),
                                              numericInput("ploth2", " Plot Height",300,min = 1),
                                              numericInput("plotw2", " Plot Width",700,min = 1)
                                      ),
                                      htmlOutput("Nselect")
                                      #sliderInput("Nselect", "Sample Size", 1, dim(D())[1],dim(D())[1],step = 1)
                                      ),
                                      #            format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                                      #             width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                      #            timezone = NULL, dragRange = TRUE)
                                     
                                 #downloadLink('downloadSave_nfTable', 'Download VSS Table')),
                         mainPanel(conditionalPanel(
                                 condition = "input.FRmethod == 'Scree Plot and Parallel Analysis'",
                                 plotOutput("nfPlot")),
                                 conditionalPanel(
                                         condition = "input.FRmethod == 'Numeric Rules'",
                                         tableOutput("nfTable")),
                                 conditionalPanel(
                                         condition = "input.FRmethod == 'Exploratory Graph Analysis'",
                                         plotOutput("EGAplot"))
                                 
                                 )),
                        # mainPanel(tabsetPanel("",
                        #                      tabPanel("Sree Plot",plotOutput("nfPlot")),
                        #                     tabPanel("Very Simple Structure",tableOutput("nfTable")),
                        #                      tabPanel("Exploratory Graph Analysis",plotOutput("EGAplot"))))),
                tabPanel("Extraction and Rotation", 
                        sidebarPanel(width = 3,
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
                        mainPanel(tabsetPanel("",
                                   tabPanel("Patten Matrix", tableOutput("textfa")),
                                   tabPanel("Factors Correlations",tableOutput("factcor"))))
                        ),
                tabPanel("Diagram",
                         sidebarPanel(width = 3,
                                 numericInput('cutt', "CutOff Value", 0.33,min = 0,step = 0.1), 
                                 checkboxInput("so", "Sort", T),
                                 checkboxInput("errarr", "Errors Arrows", T),
                                 checkboxInput("sim", "Simple Structure", T),
                                 #numericInput('rs', "size of rectangles",0.15,min = 0,step = 0.01),
                                 numericInput('es', "size of ellipses",0.05,min = 0,step = 0.01),
                                 br(),
                                 numericInput("ploth3", " Plot Height",500,min = 1),
                                 numericInput("plotw3", " Plot Width",700,min = 1)

                         ),
                         mainPanel(plotOutput("Diag"))),
                tabPanel("Factor Loadings",
                         sidebarPanel(width = 3,
                                 textInput("highcol","Color of Postive Loadings",value = "red"),
                                 textInput("lowcol","Color of Negative Loadings",value = "blue"),
                                 checkboxInput("sorting2", "Sort", T),
                                 checkboxInput("barci","Confidence Interval",T),
                                 br(),
                                 numericInput("ploth4", " Plot Height",500,min = 1),
                                 numericInput("plotw4", " Plot Width",700,min = 1)
                         ),
                         mainPanel(tabsetPanel("",
                                              tabPanel("Bargraph",plotOutput("BFig")),
                                              tabPanel("StackedBar",plotOutput("SFig"))))
                         ),
                tabPanel("Investigation of Zhang & Preacher (2015)",
                         sidebarPanel(width = 3,
                                      numericInput("ploth5", " Plot Height",500,min = 1),
                                      numericInput("plotw5", " Plot Width",700,min = 1)
                                      ),
                         mainPanel(tabsetPanel("",
                                      tabPanel("Point Estimate",tableOutput("PointTable")),
                                      tabPanel("Standard Errors Estimate",tableOutput("SETable")),
                                      tabPanel("Standard Errors Plot",plotOutput("SEFig"))))
                ),
                         
                br(),
                br(),
                tabPanel("Authors",
                         mainPanel(h4("Developed by :"),
                                   h5("Department of Psychology, National Taiwan University, Taiwan"),
                                   helpText(a("Chi-Lin Yu",href="https://github.com/PsyChiLin/ggerp")),
                                   h5("Institute of Education, National Cheng Kung University, Taiwan"),
                                   helpText(a("Ching-Fan Sheu",href = "http://140.116.183.121/~sheu/")),
                                   br(),
                                   h4("Example Data :"),
                                   helpText(a("Rosenberg Self-Esteem Scale", href = "https://www.dropbox.com/s/hpksg1zev5021z1/RSE.zip?dl=0"))
                         )
                          )
)))