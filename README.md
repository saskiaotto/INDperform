
<!-- README.md is generated from README.Rmd. Please edit that file -->

# INDperform <img src="man/figures/logo.png" align="right" width="100" height="112" />

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/saskiaotto/INDperform.svg?branch=master)](https://travis-ci.com/saskiaotto/INDperform)
[![CRAN
status](https://www.r-pkg.org/badges/version/INDperform)](https://CRAN.R-project.org/package=INDperform)
[![](http://cranlogs.r-pkg.org/badges/INDperform)](https://cran.r-project.org/package=INDperform)
<!-- badges: end -->

## Overview

INDperform is an R package for validating the performance of ecological
state indicators and assessing the ecological status based on a suite of
indicators.

## Installation

Install the CRAN version:

``` r
install.packages("INDperform")
```

**NOTE**: The 0.2.0 version is currently only available from the CRAN
archive, so for now please go for option \#2:

Or install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("saskiaotto/INDperform")
```

If you encounter a clear bug, please file a minimal reproducible example
on github. For questions email me any time.

## Usage

**For more information, documentation and examples of use, please see
INDperform website at <https://saskiaotto.github.io/INDperform/>**

For guidance on how to apply the functions step-by-step see also the
[INDperform
cheatsheet](https://raw.githubusercontent.com/saskiaotto/cheatsheets/476bad4a8876939a7b3e1784a5bf61567ed4a715/Cheatsheet_INDperform_v0.1.0.pdf).
We are currently working on the Vignette but if you want more
information on the framework for quantifying IND performances and its
statistical tools implemented in this package see

*Otto, S.A., Kadin, M., Casini, M., Torres, M.A., Blenckner, T. (2018):
A quantitative framework for selecting and validating food web
indicators. Ecological Indicators, 84: 619-631, doi:
[https://doi.org/10.1016/j.ecolind.2017.05.045](http://www.sciencedirect.com/science/article/pii/S1470160X1730300X)*

## Important News

Version 0.2.0 has been released on CRAN 2019-02-10\! The new version
includes a few internal changes as adjustments to updated packages it
depends on. Major changes changes include a new NRMSE calculation based
on the standard deviation and back-transformation (see
<https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/>
for the motivation), an NRMSE stand-alone function (`nrmse()`) and a
function that allows the calculation of the distance matrix averaged
across groups (i.e. a weighted distance matrix) (`dist_sc_group()`). For
more information see the
[news](https://github.com/saskiaotto/INDperform/blob/master/NEWS.md)
file.
