
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ura

<!-- badges: start -->

[![R-CMD-check](https://github.com/bengoehring/ura/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bengoehring/ura/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`ura` provides a set of tools for calculating inter-rater reliability
(IRR) statistics by rater, allowing for real-time monitoring of rater
progress and accuracy. While far from the first package to provide users
access to IRR diagnostics (e.g.,
[irr](https://cran.r-project.org/web/packages/irr/irr.pdf)), `ura` aims
to help researchers and supervisors efficiently monitor the coding
progress of their research assistants. Please refer to the [associated
working
paper](https://bengoehring.github.io/files/perspectives-paper.pdf) for
additional information about effectively monitoring research assistants’
progress.

## Installation

`ura` is currently only available on GitHub. You can install it from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bengoehring/ura")
```

## Usage Examples

``` r
library(ura)
## basic example code
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```
