
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scipub

<!-- badges: start -->

<!-- badges: end -->

This package contains functions for summarizing data for scientific
publication. This includes making a “Table 1” to summarize demographics
across groups, correlation tables with significance indicated by stars,
and extracting formatted statistical summarizes from simple tests for
in-text notation. The package also includes functions for Winsorizing
data based on a Z-statistic cutoff.

Functions:  
apastat - Format simple statistic test results for scientific
publication  
correltable - Create correlation table (with stars for significance) for
scientific publication  
FullTable1 - Create Table1 of group summary with stats for scientific
publication  
winsorZ\_find - Identify outliers based on z-score cutoff that are
Winsorized by the `winsorZ` function  
winsorZ - Winsorize outliers based on z-score cutoff to next most
extreme non-outlier value

## Installation

\#You can install the released version of scipub from
[CRAN](https://CRAN.R-project.org) with:

\#`r #install.packages("scipub") #`

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dpagliaccio/scipub")
```

## Example

This is a basic example:

``` r
library(scipub)
correltable(data=diamonds, vars= c("carat","depth","price"),var_names= c("Carat","Depth","Price ($)"),tri="lower", colnum=TRUE)
```
