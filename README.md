# Estimating effects of physical distancing on the COVID-19 pandemic using an urban mobility index

Code and data to support "Estimating effects of physical distancing on the COVID-19 pandemic using an urban mobility index" by Soucy et al. The preprint is currently available on [medRxiv](https://www.medrxiv.org/content/10.1101/2020.04.05.20054288v1).

# Scripts

## 1_download-data.R

This script download mobility data from [Citymapper](https://citymapper.com/cmi) and cumultive case data from many sources. Unless one desires to update the data, there is no reason to run this script. Data are already available in the directory `data`.

## 2_clean-data.R

This script processes the downloaded mobility data (`data/cmi`) and cumulative case data (`data/cases`) into two files placed in the main `data` directory: `cmi_clean.csv` and `cases_clean.csv`. Again, there is no reason to run this script unless one desires to update the data. These files are already available in the directory `data`.

## 3_analysis.R

This script runs all of the analyses and produces all of the results and figures used in the manuscript.

## theme.R

This script defines a ggplot theme and colour palette used to generate the figures.

##

# Contents

In addition to the scripts contained within the primary directory, this repository contains the following directories:

## data

This folder contains mobility and case data downloaded using `1_download-data.R`, raw case data downloaded manually rather than via the aforementioned script (`data/cases/raw`), and built-in datasets (such as `locations.csv`, which contain additional information about cities includes in the mobility dataset).
