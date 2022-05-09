
# rgabriel

<!-- badges: start -->
[![CRAN status](http://www.r-pkg.org/badges/version/rgabriel)](https://cran.r-project.org/package=rgabriel) [![Download counter](http://cranlogs.r-pkg.org/badges/rgabriel)](https://cran.r-project.org/package=rgabriel) [![](https://cranlogs.r-pkg.org/badges/grand-total/rgabriel)](https://cran.r-project.org/package=rgabriel) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Build status](https://api.travis-ci.org/yufree/rgabriel.svg?branch=master)](https://travis-ci.org/yufree/rgabriel)
<!-- badges: end -->

The goal of rgabriel is to analyze multi-level one-way experimental designs where there are unequal sample sizes and population variance homogeneity can not be assumed.

## Installation

You can install the development version of rgabriel from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("yufree/rgabriel")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rgabriel)
## basic example code
g <- c(1:40)
f <- c(rep(1,3),rep(2,12),rep(3,15),rep(4,5),rep(5,5))
gabriel.plot(g,f,rgabriel(g,f))
```

