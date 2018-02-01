---
title: 'EFAshiny: An User-Friendly Shiny Application for Exploratory Factor Analysis'
authors:
- affiliation: 1
  name: Chi-Lin Yu
  orcid: 0000-0002-4381-7163
- affiliation: 2
  name: Ching-Fan Sheu
  orcid: 0000-0002-5978-5768
date: "1 February 2018"
output: pdf_document
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

`EFAshiny` is a user-friendly web application for exploratory factor analysis (EFA) [@efa]. 
The motivation to create `EFAshiny` is to streamline the routine work flow of EFA so that 
users unfamiliar with R can perform the analysis interactively in a web browser.

Employing the graphical user interface (GUI) of `shiny` [@shiny] framework (Figure 1), `EFAshiny` provides an integrated platform to perform EFA with a drop-down menu, offering a number of choices to manage, explore, analyze and visualize data. `EFAshiny` automates these processes by wrappings together several `R` [@r] packages, such as `ggplot2` [@ggplot2], `psych` [@psych], `corrplot` [@corp], and `EGA` [@EGA], etc. For example, users can point-and-click to obtain graphical display of confidence intervals for factor loadings, which is not available in many commercial software.  Moreover, results of analysis are presented on-line as tables and graphs and they can be saved and exported by the user.

Key features of `EFAshiny` include:

- An easy-to-use GUI to free users from scripting in R
- A step by step analysis flow to perform EFA 
- Quick ways to summarize data by tables or graphs
- Several ways to explore factor retention numerically or graphically
- Several ways to explore factor extraction and rotation numerically or graphically
- A display of confidence intervals for factor loadings
- Several ways to link visualization of correlation matrix with factor structure
- Default options are chosen according to recommendations in the literature [@efaguide]
- A demonstration using a real psychological scale dataset 

Although the `EFAshiny` application is primarily aimed at behavioral researchers who want to perform EFA on a set of associated variables (e.g., item-level scale dataset), it can also be used to explore FA-based connectivity analyses [@mclaughlin1992potential] in instrument data, such as event related potentials (ERPs) and functional near-infrared spectroscopy (fNIRS).

In conclusion, `EFAshiny` allows researchers to harness the combined power of many R packages together for performing interactive EFA and obtaining numerical and graphical results in a user-friendly menu-driven GUI. Documentation, tutorials and usages can be found on 
[**our page**](https://github.com/PsyChiLin/EFAshiny). 

# References

![The GUI of `EFAshiny`](Introduction.png)