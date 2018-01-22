library(shiny);library(corrplot);library(psych)
library(reshape2);library(ggplot2);library(moments)
library(grid);library(gridExtra);library(shinythemes)
library(EFAutilities);library(EGA);

shinyUI(fluidPage(
        theme = shinytheme("flatly"),
        navbarPage("EFAshiny",inverse = F, fluid = T,
                   tabPanel("Introduction",
                            sidebarPanel(width = 4,
                                         h4("Welcome to EFAshiny !",align = "center" ),
                                         strong("EFAshiny"),("implements exploratory factor analysis (EFA) to facilitate interpretation of factors."),
                                         br(),
                                         h5("The features of EFAshiny include : "),
                                         tags$li((strong("Quick data summary"))),
                                         tags$li(strong("Graphical and numerical retention methods")),
                                         tags$li(strong("Lots of extraction and rotation methods")),
                                         tags$li(strong("Confidence intervals of factor loadings")),
                                         tags$li(strong("Visualizations of factor loadings")),
                                         br(),
                                         p("Upload your own data to start the analyses."),
                                         p("A default demonstration uses a",
                                           helpText(a("Rosenberg Self-Esteem Scale",
                                                      href = "https://github.com/PsyChiLin/EFAshiny/blob/master/RSE/RSE.csv")),
                                           "dataset."),
                                         br(),
                                         h4("Have fun with EFAshiny!",align = "center")
                                         ),
                            mainPanel(tabsetPanel(id  ="",
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
                        fileInput("file", "Upload data-file:"),
                        radioButtons('dataformat', 'Data Format',
                                     c("csv",
                                       "txt"),
                                     "csv"),
                        radioButtons('hdr', "Header of variable",
                                     c("TRUE","FALSE"),"TRUE"),
                        # correlation matrix
                        radioButtons('datatype', 'Type of Data',
                                     c("Raw Data",
                                       "Correlation Matrix"),
                                     "Raw Data"),
                        # variable selection
                        htmlOutput("varselect"),
                        conditionalPanel(
                                condition = "input.datatype == 'Correlation Matrix'",
                                numericInput('nobs', "Number of Observations",144,min = 1)
                        ),
                        br(),
                        downloadLink('downloadSave_SelectData', 'Download Selected Data')
                        ),
                        mainPanel(tableOutput("table"))),
                tabPanel("Data Summary",
                         sidebarPanel(width = 3,
                                 selectInput("cortype","Type of Correlations",
                                             c("Pearson", "tetrachoric", "polychoric")),
                                 br(),
                                 numericInput("ploth1", " Plot Height",800,min = 1),
                                 numericInput("plotw1", " Plot Width",800,min = 1),
                                 br(),
                                 downloadLink('downloadSave_summary', 'Download Summary Table')
                        ),
                        mainPanel(tabsetPanel(id  = "",
                                             tabPanel("Numeric Statistic",tableOutput("sum_table")),
                                             tabPanel("Distribution", plotOutput("itemdist")),
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
                                              radioButtons('qpa',"Quantile of Parallel analysis",
                                                           c(0.99,0.95,0.5),0.99),
                                              numericInput("npasim","Number of simulated analyses to perform",200,min = 1, step = 100),
                                              br(),
                                              numericInput("ploth2", " Plot Height",300,min = 1),
                                              numericInput("plotw2", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Numeric Rules'",
                                              numericInput("maxn", "Max Number of Factor For Estimation",4,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Exploratory Graph Analysis'",
                                              numericInput("npasim","Number of simulated analyses to perform",10,min = 1,max = 1000, step = 10),
                                              br(),
                                              numericInput("ploth2", " Plot Height",300,min = 1),
                                              numericInput("plotw2", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                           condition = "input.datatype == 'Raw Data'",
                                           htmlOutput("Nselect")
                                      )
                                      ),
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
                        mainPanel(tabsetPanel(id  = "",
                                   tabPanel("Patten Matrix", tableOutput("textfa")),
                                   tabPanel("Factors Correlations",tableOutput("factcor"))))
                        ),
                tabPanel("Diagram",
                         sidebarPanel(width = 3,
                                 numericInput('cutt', "CutOff Value", 0.33,min = 0,step = 0.1), 
                                 checkboxInput("so", "Sort", T),
                                 checkboxInput("errarr", "Errors Arrows", T),
                                 checkboxInput("sim", "Simple Structure", T),
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
                                              numericInput("ploth5", " Plot Height",500,min = 1),
                                              numericInput("plotw5", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FLplot == 'SE and Factor Loadings'",
                                              br(),
                                              numericInput("ploth6", " Plot Height",500,min = 1),
                                              numericInput("plotw6", " Plot Width",700,min = 1)
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
                                         tabsetPanel(id  = "",
                                                     tabPanel("SE Figure",plotOutput("SEFig")),
                                                     tabPanel("Point Estimation",tableOutput("PointTable"))))
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