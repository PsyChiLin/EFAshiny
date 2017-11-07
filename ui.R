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
                   #tabPanel("Introduction",
                   #         br(),
                   #         h4("Example Data :"),
                   #         helpText(a("Rosenberg Self-Esteem Scale", href = "https://www.dropbox.com/s/hpksg1zev5021z1/RSE.zip?dl=0"))),
                   tabPanel("Introduction",
                            sidebarPanel(width = 4,
                                         h4("Welcome to EFAshiny !",align = "center" ),
                                         strong("EFAshiny"),("is a easy-to-use tool that provides reasonable flow to implement exploratory factor analysis (EFA) and facilitate proper interpretation offactors."),
                                         br(),
                                         h5("The features of EFAshiny included : "),
                                         tags$li((strong("Quick data summary"))),
                                         tags$li(strong("Graphical and numerical retention methods")),
                                         tags$li(strong("Lots of extraction and rotation methods")),
                                         tags$li(strong("Confidence intervals of factor loadings")),
                                         tags$li(strong("Visualizations of factor loadings")),
                                         #h5("Examples of EFAshiny are shown in left panel."),
                                         br(),
                                         p("Upload your own data to start the analyses."),
                                         p("If these is no data to upload, we provide default demonstrations of in every single step by using the dataset of",
                                           span(helpText(a("Rosenberg Self-Esteem Scale (Rosenberg, 1965).",
                                                           href = "https://www.dropbox.com/s/hpksg1zev5021z1/RSE.zip?dl=0")))),
                                         br(),
                                         h4("Enjoy your EFAshiny journey!",align = "center")
                                         ),
                            mainPanel(tabsetPanel("",
                                                  tabPanel("Demo",div(img(src="Demo_Full.png", height = 550, width = 650))),
                                                  tabPanel("References",
                                                           br(),
                                                           tags$li("Chang, W., Cheng, J., Allaire, J. J., Xie, Y., & McPherson, J. (2017). shiny: Web application framework for R."),
                                                           tags$li("Bartholomew, D.J., Knott, M., Irini Moustaki, I. (2011). Latent Variable Models and Factor Analysis. A Unified Approach. Wiley."),
                                                           tags$li("Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new approach for estimating the number of dimensions in psychological research. PloS one, 12(6)."),
                                                           tags$li("Henson, R. K., & Roberts, J. K. (2006). Use of exploratory factor analysis in published research: Common errors and some comment on improved practice. Educational and Psychological measurement, 66(3), 393-416."),
                                                           tags$li("Revelle, W. (2014). psych: Procedures for personality and psychological research. Northwestern University, Evanston. R package version, 1(1)."),
                                                           tags$li("Rosenberg, M. (1965). Rosenberg self-esteem scale (RSE). Acceptance and commitment therapy. Measures package, 61, 52."),
                                                           tags$li("Wickham, H. (2016). ggplot2: elegant graphics for data analysis. Springer."),
                                                           tags$li("Zhang, G., & Preacher, K. J. (2015). Factor rotation and standard errors in exploratory factor analysis. Journal of Educational and Behavioral Statistics, 40(6), 579-603."))
                                    ))),
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
                        conditionalPanel(
                                condition = "input.datatype == 'Correlation Matrix'",
                                numericInput('nobs', "Number of Observations",144,min = 1)
                        ),
                        #htmlOutput("Nselect"),
                        br(),
                        #textInput("name","Dataset name:","Data"),
                        downloadLink('downloadSave_SelectData', 'Download Selected Data')
                        ),
                        mainPanel(tableOutput("table"))),
                #tabPanel("Exploration"),
                tabPanel("Data Summary",
                         sidebarPanel(width = 3,
                                 
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
                                              #numericInput('qpa',"Quantile of Parallel analysis", 0.99 , min = 0 , max = 1,step = 0.1),
                                              radioButtons('qpa',"Quantile of Parallel analysis",
                                                           c(0.99,0.95,0.5),0.99),
                                              numericInput("npasim","Number of simulated analyses to perform",200,min = 1, step = 100),
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
                                      conditionalPanel(
                                           condition = "input.datatype == 'Raw Data'",
                                           htmlOutput("Nselect")
                                      )
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
                        numericInput("bsnum","Number of Bootstraps",200, min = 20,step = 100),
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
                                      selectInput("FLplot", "Factor Loadings Visualizations", 
                                                  c("Factor Loadings and Correlation Matrix",
                                                    "Bootstrapping Factor Loadings",
                                                    "SE and Factor Loadings"),
                                                  "Bootstrapping Factor Loadings"),
                                      conditionalPanel(
                                              condition = "input.FLplot == 'Factor Loadings and Correlation Matrix'",
                                              checkboxInput("sorting2", "Sort", T),
                                              br(),
                                              numericInput("ploth4", " Plot Height",500,min = 1),
                                              numericInput("plotw4", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FLplot == 'Bootstrapping Factor Loadings'",
                                              textInput("highcol","Color of Postive Loadings",value = "firebrick"),
                                              textInput("lowcol","Color of Negative Loadings",value = "chartreuse4"),
                                              checkboxInput("sorting2", "Sort", T),
                                              checkboxInput("barci","Confidence Interval",T),
                                              br(),
                                              numericInput("ploth4", " Plot Height",500,min = 1),
                                              numericInput("plotw4", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FLplot == 'SE and Factor Loadings'",
                                              br(),
                                              numericInput("ploth4", " Plot Height",500,min = 1),
                                              numericInput("plotw4", " Plot Width",700,min = 1)
                                      )
                         ),
                         mainPanel(
                                 conditionalPanel(
                                         condition = "input.FLplot == 'Factor Loadings and Correlation Matrix'",
                                         plotOutput("SFig")),
                                 conditionalPanel(
                                         condition = "input.FLplot == 'Bootstrapping Factor Loadings'",
                                         plotOutput("BFig")),
                                 conditionalPanel(
                                         condition = "input.FLplot== 'SE and Factor Loadings'",
                                         tabsetPanel("",
                                                     tabPanel("SE Figure",plotOutput("SEFig")),
                                                     tabPanel("Point Estimation",tableOutput("PointTable"))))
                                         #plotOutput())
                                 #
                         )),
                br(),
                br(),
                tabPanel("Authors",
                         mainPanel(h4("Developed by :"),
                                   h5("Department of Psychology, National Taiwan University, Taiwan"),
                                   helpText(a("Chi-Lin Yu",href="https://github.com/PsyChiLin/ggerp")),
                                   h5("Institute of Education, National Cheng Kung University, Taiwan"),
                                   helpText(a("Ching-Fan Sheu",href = "http://140.116.183.121/~sheu/"))
                         )
                )
)))


#tabPanel("Investigation of Zhang & Preacher (2015)",
#         sidebarPanel(width = 3,
#                      numericInput("ploth5", " Plot Height",500,min = 1),
#                      numericInput("plotw5", " Plot Width",700,min = 1)
#                      ),
#         mainPanel(tabsetPanel("",
#                      tabPanel("Point Estimate",tableOutput("PointTable")),
#                      tabPanel("Standard Errors Estimate",tableOutput("SETable")),
#                      tabPanel("Standard Errors Plot",plotOutput("SEFig"))))
#),