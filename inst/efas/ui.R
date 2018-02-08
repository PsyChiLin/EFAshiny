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
if(!require(plotly)) {require(plotly)}
if(!require(ggcorrplot)) {require(ggcorrplot)}
if(!require(shinyAce)) {require(shinyAce)}
if(!require(RCurl)) {require(RCurl)}

code = '
### Overview
This tutorial aims to demonstrate how to perform the fundamental analyses in [`EFAshiny`](https://psychilin.shinyapps.io/EFAshiny/) using R code. By following each line in this tutorial, you can learn basic R programming and can implement automated processing script in future analyses. Also, you can further use the code in `EFAshiny` within a script pupeline without launching the app. However, we still suggest you to use [`EFAshiny`](https://psychilin.shinyapps.io/EFAshiny/) APP :)) You can also have a look at our [tutorial](https://github.com/PsyChiLin/EFAshiny) of `EFAshiny` GUI to make an easy comparison while writing the code. Have fun with [`EFAshiny`](https://psychilin.shinyapps.io/EFAshiny/) and `R` !

### Preparation
First of all, we should load all the packages and fucntions in `EFAshiny` into `R` session. If you do not have these package installed, please use `install.package("packagename")` to perfrom installations.
```{r, message=FALSE, warning=FALSE}
require(ggplot2);require(psych);require(corrplot);require(reshape2);require(moments);require(gridExtra)
require(qgraph);require(bootnet);require(igraph);require(ggcorrplot);require(RCurl)
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/my_summary.R")
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/faplot.R")
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/bootEGA.R")
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/bargraph.R")
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/stackbar.R")
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/printLoadings.R")
source("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/inst/efas/functions/theme_default.R")
```

### Data Input
Read the demonsration data [`RSE`](https://github.com/PsyChiLin/EFAshiny/blob/master/RSE/RSE.csv) into `R`. We assign it to a object called `dta`. Then using `head` to observe and explore this data. See our [tutorial](https://github.com/PsyChiLin/EFAshiny) for data description.
```{r}
dta <- read.csv(text=getURL("https://raw.githubusercontent.com/PsyChiLin/EFAshiny/master/RSE/RSE.csv"))
head(dta)
```

### Data Summary
Several kinds of data summary can be perform in `EFAshiny`. We demonstrate them using basic code. The package `ggplot2` is a useful package for plotting. We use it to plot the histograms, the density plots, and the correlation matrix. You can also check the documentation of [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf).

##### Numeric Statistic
```{r}
NumericStatistic <- apply(dta,2,my_summary)
row.names(NumericStatistic) <- c("Mean","SD","Skewness","Kurtosis","Median","MAD")
NumericStatistic <- as.data.frame(t(NumericStatistic))
NumericStatistic <- round(NumericStatistic,3)
NumericStatistic
```

##### Histogram
```{r, message=FALSE}
dta_long <- melt(dta)
colnames(dta_long) <- c("Item", "Response")
Histogram <- ggplot(dta_long, aes(x = Response, fill = Item))+
geom_histogram(bins = 10)+
facet_wrap(~Item)+
theme_default()
Histogram
```

##### Density Plot
```{r, message=FALSE}
DensityPlot <- ggplot(dta_long, aes(x = Response, fill = Item))+
geom_density()+
facet_wrap(~Item)+
theme_default()
DensityPlot 
```

##### Correlation Matrix
```{r, fig.height=7, fig.width=7}
CorMat <- cor(as.matrix(dta))
corrplot(CorMat,order="hclust",type="upper",method="ellipse",
tl.pos = "lt",mar = c(2,2,2,2))
corrplot(CorMat,order="hclust",type="lower",method="number",
diag=FALSE,tl.pos="n", cl.pos="n",add=TRUE,mar = c(2,2,2,2))
```

##### ggcorplot
```{r}
ggcorrplot(CorMat, hc.order = T,type = "lower", lab = TRUE,
colors = c("#E46726", "white", "#6D9EC2"))
```

### Factor Retention
We provide several factor retention methods in `EFAshiny`. Those analyses can also be performed using R code. You can adopt these methods for your own use. 

##### Scree Plot and Parallel Analysis
```{r, message=FALSE}
PA <- faplot(CorMat,n.obs = 256, quant = 0.95)
PA[[1]]
```

##### Numeric Rules
```{r}
NumericRule <- VSS(CorMat,n = 4, plot = F, n.obs = 256)
temp1 <- data.frame(nFactor = row.names(NumericRule$vss.stats), 
VSS1 = NumericRule$cfit.1, VSS2 = NumericRule$cfit.2, 
MAP = NumericRule$map)
temp2 <- NumericRule$vss.stats[,c(6:8,11)]
NumericRule <- cbind(temp1,temp2)
NumericRule
```

##### Exploratory Graph Analysis (EGA)
```{r}
EGArst <- bootEGA(data = dta, n = 10, medianStructure = TRUE, plot.MedianStructure = TRUE, 
ncores = 4, layout = "spring")
plot(EGArst$plot)
```

### Extarction and Rotation
The key analyses in EFA is factor extarction and rotation. We teach you to perform the analyses using `psych` package. Using these lines, you can obtain basic results of EFA. Can also see the code in `EFAshiny` server and `psych` package for details.  
```{r}
EFArst <- fa(CorMat,2,n.obs=256, rotate = "promax",fm = "pa", n.iter = 200)
EFArst
```

### Visualization
By passing the results of EFA from aforementioned analyses into the well-established functions, you can easily visualize these results from EFA. Definitely, `EFAshiny` can help you to do these automatically.

##### Diagram
```{r,message=FALSE}
fa.diagram(EFArst,simple = T,cut = 0.33,
sort = T,errors = T,e.size = 0.05)
```

##### Bootstrapping Factor Loadings
```{r,message=FALSE}
order <- rev(row.names(as.data.frame(printLoadings(EFArst$cis$means,sort = T,cutoff = 0)))) # define the order of the variable
bargraph(EFArst,order = order,nf = 2,highcol = "firebrick",lowcol = "chartreuse4",ci = T)
```

##### Factor Loadings and Correlation Matrix
```{r,message=FALSE}
stackbar(CorMat,EFArst,order = order,highcol = "firebrick",lowcol = "chartreuse4")
```

##### Your own analyses
```{r}
# Put your own code here...
```

'
shinyUI(fluidPage(
        theme = shinytheme("flatly"),
        navbarPage("EFAshiny",inverse = F, fluid = T,
                   tabPanel("Introduction",
                            sidebarPanel(width = 4,
                                         h4("Welcome to EFAshiny !",align = "center" ),
                                         strong("EFAshiny"),("implements exploratory factor analysis (EFA) to facilitate interpretation of factors."),
                                         br(),
                                         h5("The features of EFAshiny include : "),
                                         tags$li((strong("Easy-to-follow analysis flow"))),
                                         tags$li((strong("Quick data summary"))),
                                         tags$li(strong("Graphical and numerical retention methods")),
                                         tags$li(strong("Lots of extraction and rotation methods")),
                                         tags$li(strong("Confidence intervals of factor loadings")),
                                         tags$li(strong("Visualizations of factor loadings")),
                                         tags$li(strong("Appropriate default arguments")),
                                         br(),
                                         p("Upload your own data to start the analyses."),
                                         p("A default demonstration uses a Rosenberg Self-Esteem Scale datset."),
                                         br(),
                                         helpText(a("Tutorial",href = "https://github.com/PsyChiLin/EFAshiny")),
                                         helpText(a("Rosenberg Self-Esteem Scale",href = "https://github.com/PsyChiLin/EFAshiny/blob/master/RSE/RSE.csv")),
                                         br(),
                                         h4("Have fun with EFAshiny!",align = "center")
                                         ),
                            mainPanel(tabsetPanel(id  ="",
                                                  tabPanel("Demo",div(img(src="Demo_Full.png", height = 550, width = 650))),
                                                  tabPanel("References",
                                                           br(),
                                                           tags$li("Auguie, B. (2017). gridExtra: Miscellaneous Functions for Grid Graphics, 2016. R package version, 2.3."),
                                                           tags$li("Bartholomew, D.J., Knott, M., Irini Moustaki, I. (2011). Latent Variable Models and Factor Analysis. A Unified Approach. Wiley."),
                                                           tags$li("Cattell, R. B. (1966). The scree test for the number of factors. Multivar Behav Res, 1(2), 245-276."), 
                                                           tags$li("Chang, W. (2016). shinythemes: Themes for Shiny. R package version 1.1.1."),
                                                           tags$li("Chang, W., Cheng, J., Allaire, J. J., Xie, Y., & McPherson, J. (2017). shiny: Web application framework for R. R package version 1.0.0."),
                                                           tags$li("Csardi, G., & Nepusz, T. (2006). The igraph software package for complex network research. InterJournal, Complex Systems, 1695(5), 1-9."),
                                                           tags$li("Epskamp, S., Cramer, A. O. J., Waldorp, L.J., Schmittmann, V.D., & Borsboom, D. (2012). qgraph: Network Visualizations of Relationships in Psychometric Data. Journal of Statistical Software, 48(4), 1-18."),
                                                           tags$li("Epskamp, S. (2017). bootnet: Bootstrap methods for various network estimation routines. R package version 1.0.1"),
                                                           tags$li("Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new approach for estimating the number of dimensions in psychological research. PloS one, 12(6), e0174035."),
                                                           tags$li("Henson, R. K., & Roberts, J. K. (2006). Use of exploratory factor analysis in published research: Common errors and some comment on improved practice. Educational and Psychological measurement, 66(3), 393-416."),
                                                           tags$li("Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. Psychometrika, 30(2), 179-185."),
                                                           tags$li("Komsta, L., & Novomestky, F. (2013). moments: moments, cumulants, skewness, kurtosis and related tests. R package version 0.13."),
                                                           tags$li("Makowski, (2018). The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science. Journal of Open Source Software, 3(22), 470."),
                                                           tags$li("Revelle, W. (2017) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, R package version 1.7.8."),
                                                           tags$li("Rosenberg, M. (1965). Rosenberg self-esteem scale (RSE). Acceptance and commitment therapy. Measures package, 61, 52."),
                                                           tags$li("Taiyun Wei and Viliam Simko (2017). R package 'corrplot': Visualization of a Correlation Matrix. R package version 0.84."),
                                                           tags$li("Velicer, W. F. (1976). Determining the number of components from the matrix of partial correlations. Psychometrika, 41(3), 321-327."),
                                                           tags$li("Wickham, H. (2016). reshape2: Flexibly Reshape Data: A Reboot of the Reshape Package. R package version 1.4.2."),
                                                           tags$li("Wickham, H. (2016). ggplot2: elegant graphics for data analysis. Springer."),
                                                           tags$li("Zhang, G., & Preacher, K. J. (2015). Factor rotation and standard errors in exploratory factor analysis. Journal of Educational and Behavioral Statistics, 40(6), 579-603."),
                                                           tags$li("Zhang, G. (2014). Estimating standard errors in exploratory factor analysis. Multivariate Behavioral Research, 49, 339-353."))
                                                           
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
                                 numericInput("ploth1", " Plot Height",650,min = 1),
                                 numericInput("plotw1", " Plot Width",700,min = 1),
                                 br(),
                                 downloadLink('downloadSave_summary', 'Download Summary Table')
                        ),
                        mainPanel(tabsetPanel(id  = "",
                                             tabPanel("Numeric Statistic",tableOutput("sum_table")),
                                             tabPanel("Histogram", plotlyOutput("itemdist")),
                                             tabPanel("Density Plot", plotlyOutput("itemdensity")),
                                             tabPanel("Correlation Matrix", plotOutput("distPlot")),
                                             tabPanel("ggcorrplot", plotOutput("ggcorPlot"))))
                ),
                tabPanel("Factor Retention",
                         sidebarPanel(width = 3,
                                      selectInput("FRmethod", "Method of Factor Retention", 
                                                  c("Scree Plot and Parallel Analysis",
                                                    "Numeric Rules",
                                                    "Exploratory Graph Analysis",
                                                    "Summary"),
                                                  "Scree Plot and Parallel Analysis"),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Scree Plot and Parallel Analysis'",
                                              radioButtons('qpa',"Quantile of Parallel analysis",
                                                           c(0.99,0.95,0.5),0.95),
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
                                              selectInput("egalayout", "Layout", 
                                                          c("spring","circle","groups"),
                                                          "spring"),
                                              br(),
                                              numericInput("ploth2", " Plot Height",300,min = 1),
                                              numericInput("plotw2", " Plot Width",700,min = 1)
                                      ),
                                      conditionalPanel(
                                              condition = "input.FRmethod == 'Summary'"
                                      ),
                                      conditionalPanel(
                                           condition = "input.datatype == 'Raw Data'",
                                           htmlOutput("Nselect")
                                      )
                                      ),
                         mainPanel(
                                 conditionalPanel(
                                         condition = "input.FRmethod == 'Scree Plot and Parallel Analysis'",
                                         plotOutput("nfPlot")),
                                 conditionalPanel(
                                         condition = "input.FRmethod == 'Summary'",
                                         tableOutput("nfsum")),
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
                                              checkboxInput("sorting3", "Sort", T),
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
                tabPanel("Editor",
                         sidebarPanel(width = 4,
                                      shinyAce::aceEditor("rmd", mode = "markdown", value = code),
                                      shiny::actionButton("eval", "Run"),
                                      br(),
                                      br(),
                                      helpText(a("Download example",href = "https://github.com/PsyChiLin/EFAshiny/blob/master/EFAshiny_RCodeDemo.Rmd")),
                                      helpText(a("Online example",href = "http://rpubs.com/Chi-Lin/EFAshiny_R_Code_Demo")),
                                      br()
                                      ),
                         mainPanel(shiny::htmlOutput("knitr"))
                ),
                br(),
                br(),
                tabPanel("Authors",
                         mainPanel(h4("Developed by :"),
                                   h5("Department of Psychology, National Taiwan University, Taiwan"),
                                   helpText(a("Chi-Lin Yu",href="https://github.com/PsyChiLin/EFAshiny")),
                                   h5("Institute of Education, National Cheng Kung University, Taiwan"),
                                   helpText(a("Ching-Fan Sheu",href = "http://140.116.183.121/~sheu/"))
                         )
                )
)))