# Ames Housing Price Analysis

This repository contains the final project for **STA302: Methods of Data Analysis**. The project uses multiple linear regression to study how housing characteristics are associated with sale prices in Ames, Iowa.

## Research question

To what extent do above-ground living area, total basement area, overall quality, house age, fireplaces, and full bathrooms explain variation in house sale prices under normal sale conditions?

## Data

The analysis uses the Ames housing data provided by the [`AmesHousing`](https://cran.r-project.org/package=AmesHousing) R package. The original dataset contains 2,930 properties and 82 variables describing residential sales in Ames, Iowa, from 2006 to 2010.

## Analysis

The project includes:

- data cleaning and exploratory analysis;
- multiple linear regression and model selection;
- residual and influence diagnostics;
- coefficient interpretation;
- test-set validation.

The selected model uses log sale price as the response and includes above-ground living area, total basement area, house age, squared house age, overall quality, and fireplace category. It explains approximately 87.2% of the variation in log sale prices and achieved a test-set R-squared of approximately 0.81.

## Repository files

- [`STA_302_ProjectReport (1).Rmd`](<STA_302_ProjectReport (1).Rmd>) — complete R Markdown source, including the written report and analysis.
- [`STA_302_ProjectReport.R`](STA_302_ProjectReport.R) — R code extracted from the R Markdown file.
- [`STA302_ProjectReport (1).pdf`](<STA302_ProjectReport (1).pdf>) — final rendered report.

## Running the analysis

The analysis requires R and the following packages:

```r
install.packages(c(
  "tidyverse", "AmesHousing", "broom", "knitr", "patchwork",
  "gridExtra", "car"
))
```

Run the standalone analysis from the project directory with:

```r
source("STA_302_ProjectReport.R")
```

Alternatively, open the `.Rmd` file in RStudio and knit it to PDF. Rendering the PDF requires a LaTeX installation such as TinyTeX.

## Authors

- Yizhou Qian
- Yichen Wang
- Lei Zhu
- Zhaotong Pan
