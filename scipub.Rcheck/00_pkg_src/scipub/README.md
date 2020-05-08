
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
correltable(data=psydat, html=TRUE)
#> Warning: Converting non-numeric columns to factor: Sex,Income
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
#> <thead>
#> <tr>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey;'> </th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Age</th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Sex</th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Income</th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Height</th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>iq</th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>depressT</th>
#> <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>anxT</th>
#> </tr>
#> </thead>
#> <tbody>
#> <tr>
#> <td style='text-align: left;'>Age</td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'>t=-1.86</td>
#> <td style='text-align: center;'>F=4.17*</td>
#> <td style='text-align: center;'>.41***</td>
#> <td style='text-align: center;'>.09***</td>
#> <td style='text-align: center;'>.02</td>
#> <td style='text-align: center;'>-.01</td>
#> </tr>
#> <tr>
#> <td style='text-align: left;'>Sex</td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'>χ2=0.72</td>
#> <td style='text-align: center;'>t=0.83</td>
#> <td style='text-align: center;'>t=1.25</td>
#> <td style='text-align: center;'>t=-4.87***</td>
#> <td style='text-align: center;'>t=-5.76***</td>
#> </tr>
#> <tr>
#> <td style='text-align: left;'>Income</td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'>F=1.15</td>
#> <td style='text-align: center;'>F=364.33***</td>
#> <td style='text-align: center;'>F=31.18***</td>
#> <td style='text-align: center;'>F=16.26***</td>
#> </tr>
#> <tr>
#> <td style='text-align: left;'>Height</td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'>.04*</td>
#> <td style='text-align: center;'>-.01</td>
#> <td style='text-align: center;'>-.01</td>
#> </tr>
#> <tr>
#> <td style='text-align: left;'>iq</td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'>-.08***</td>
#> <td style='text-align: center;'>-.06***</td>
#> </tr>
#> <tr>
#> <td style='text-align: left;'>depressT</td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'></td>
#> <td style='text-align: center;'>.61***</td>
#> </tr>
#> <tr>
#> <td style='border-bottom: 2px solid grey; text-align: left;'>anxT</td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> <td style='border-bottom: 2px solid grey; text-align: center;'></td>
#> </tr>
#> </tbody>
#> <tr><td colspan='8' style='text-align: left;'>
#> Note. This table presents Pearson correlation coefficients with pairwise deletion. N=4 missing Sex. N=404 missing Income. N=7 missing Height. N=179 missing iq. N=8 missing depressT. N=8 missing anxT.  Group differences for continuous and categorical variables are indicated by t-statistic/ANOVA F and chi-squared, respectively. * p<.05, ** p<.01, *** p<.001</td></tr>
#> </table>
```
