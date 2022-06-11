
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggvwline

<!-- badges: start -->
<!-- badges: end -->

The purpose of `ggvwline` is to provide a set of layer functions for
drawing variable-width curves.

## Installation

You can install the development version of ggvwline from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Hy4m/ggvwline")
```

## Example

This is a basic example which shows you how to draw variable-width
curves:

``` r
library(ggvwline)
library(ggplot2)
tt <- seq(0, 2 * pi, length.out = 102)[-c(1, 102)]
dd <- data.frame(x = cos(tt),
                 y = sin(tt),
                 g = rep(LETTERS[1:4], each = 25))

ggplot(dd, aes(x, y, fill = g)) +
  geom_vwline() +
  coord_fixed()
```

<img src="man/figures/README-example-1.png" width="100%" />
