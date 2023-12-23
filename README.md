# Public transit mobility as a leading indicator of COVID-19 transmission in 40 cities during the first wave of the pandemic

## Purpose of this repository

This repository contains code and data to reproduce the analyses presented in the manuscript "Public transit mobility as a leading indicator of COVID-19 transmission in 40 cities during the first wave of the pandemic" by Soucy et al., with the exception of tables S1 and S2.

**Code and data for the previous version of this manuscript, entitled "Estimating effects of physical distancing on the COVID-19 pandemic using an urban mobility index" and available on [medRxiv](https://www.medrxiv.org/content/10.1101/2020.04.05.20054288v3), may be found [here](https://github.com/jeanpaulrsoucy/covid-19-mobility/tree/2e38f6584e5e31d6c68ee4b765469bb412855c6f).**

## Requirements

All code is written in the programming language [R](https://www.r-project.org/). The easiest way to run it is to use the [RStudio](https://rstudio.com/) IDE. An `.Rproj` file is included with this repository for ease of use with RStudio. The scripts should run with any modern version of R.

The R packages required to reproduce the tables and figures at listed at the top of their respective scripts. They must be installed using `install.packages` or similar functionality within RStudio prior to running the script.

## Reproducing tables and figures

Run `3_analysis.R` and `4_extra.R` to create the output tables and figures.

There is no reason to run `1_download-data.R` or `2_clean-data.R` unless you wish to refresh the data used in the analysis (which is already available in the `data` directory). Note that rerunning these scripts may cause the analysis script to break if there are changes to the underlying datasets.

## Tables

- [Table 1](tab/tab_1.csv)
- [Table S3](tab/tab_s3.csv)

## Figures

- [Figure 1](fig/fig_1.png)
- [Figure 2](fig/fig_2.png)
- [Figure 3](fig/fig_3.png)
- [Figure S1](fig/fig_s1.png)
- [Figure S2](fig/fig_s2.png)
- [Figure S3](fig/fig_s3.png)
