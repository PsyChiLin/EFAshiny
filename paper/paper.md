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

`EFAshiny` is an user-friendly web application for exploratory factor analysis (EFA) [@efa]. The `EFAshiny` graphical user interface (GUI) in `shiny` [@shiny] is designed to free users from scripting by wrapping together various `R` [@r] packages, such as `ggplot2` [@ggplot2], `psych` [@psych], `corrplot` [@corp], and `EGA` [@EGA], for data management, EFA, and graphics. The major focus is on item-level scale data, which usually require the implementation of EFA. In an easy-to-follow GUI (Figure 1), `EFAshiny` implments a step by step analysis flow and a vareity of appropaches to explore, analyze and visualize data and results. Resonable default setting are provided according to proffered recommendations in the literature [@efaguide] are provided. Without having worry about data processing or programming users can obtain insights into the data and the results of EFA. Also, results of analysis in tables and graphs are all presented on-line and can be exported. Documentation, tutorials and usages can be found on [**our page**](https://github.com/PsyChiLin/EFAshiny). 

Key features of `EFAshiny` include:

- An user-friendly GUI for users without programming experiences
- An easy-to-follow step by step analysis flow to easily perform EFA 
- Quick data explorations with numeric summary and graphics
- Graphical and numerical factor retention methods
- Lots of extraction and rotation methods
- Confidence intervals calculation of factor loadings
- Visualizations of factor loadings with confidence intervals and correlation matrix
- Default arguments from recommendations in the literature
- Demonstrations using a typical item-level scale dataset 

Our application is primarily aimed at behavioral science researchers who want to analyze and visualize item-level scales or datasets with a set of associated variables using EFA. It is also noted that `EFAshiny` can be used to provide EFA-based connectivity analyses in neuroimaging data, such as event related potentials (ERPs) and functional near-infrared spectroscopy (fNIRS).

# References

![The GUI of `EFAshiny`](Introduction.png)