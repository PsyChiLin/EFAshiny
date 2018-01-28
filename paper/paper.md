---
title: 'EFAshiny: An User-Friendly Shiny Application for Exploratory Factor Analysis'
authors:
- affiliation: 1
  name: Chi-Lin Yu
  orcid: 0000-0002-4381-7163
- affiliation: 2
  name: Ching-Fan Sheu
  orcid: null
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
bibliography: paper.bib
tags:
- Exploratory Factor Analysis (EFA)
- Data Management
- Data Visualization
- R
- Shiny
- ggplot2
affiliations:
- index: 1
  name: Department of Psychology, National Taiwan University, Taiwan
- index: 2
  name: Institute of Education, National Cheng Kung University, Taiwan
---

# Summary

`EFAshiny` is an user-friendly web application for exploratory factor analysis (EFA) [@efa]. The objectives of `EFAshiny` are to streamline the routine data analyses in EFA and to allow users to easily understand and interact with their data.

With graphical user interface (GUI) in an interactive `shiny` [@shiny] framework (Figure 1), `EFAshiny` provides a step by step analysis flow to automate the procedure of EFA. A variety of approaches to manage, explore, analyze and visualize data is provided in `EFAshiny` through wrapping together various `R` [@r] packages, such as `ggplot2` [@ggplot2], `psych` [@psych], `corrplot` [@corp], and `EGA` [@EGA]. `EFAshiny` also allow users to estimate confidence intervals of factor loadings, thereby enhancing the functionality and practicality of the analyses. Finally, results of analysis in tables and graphs are all presented on-line and can be easily exported.

Key features of `EFAshiny` include:

- An easy-to-use GUI to free users from R scripting
- A step by step analysis flow to easily and systematically perform EFA 
- Quick data explorations with numeric summary and graphics
- Graphical and numerical factor retention methods
- Plenty of extraction and rotation methods
- Confidence intervals calculation of factor loadings
- Visualizations of factor loadings and Visualizations of correlation matrix
- Default arguments from recommendations in the previous literature [@efaguide]
- Demonstrations using a typical item-level scale dataset 

Our application is primarily aimed at behavioral science researchers who want to analyze and visualize datasets with a set of associated variables (e.g., item-level scale dataset) using EFA. It is also noted that `EFAshiny` can be used to provide EFA-based connectivity analyses in neuroimaging data, such as event related potentials (ERPs) and functional near-infrared spectroscopy (fNIRS).

In conclusion, by using `EFAshiny`, users can obtain insights into the data and the results of EFA without having worry about data processing or programming. Documentation, tutorials and usages can be found on [**our page**](https://github.com/PsyChiLin/EFAshiny). 

# References

![The GUI of `EFAshiny`](Introduction.png)