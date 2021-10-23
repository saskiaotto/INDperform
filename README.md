
<!-- README.md is generated from README.Rmd. Please edit that file -->

# INDperform <img src="man/figures/logo.png" align="right" width="100" height="112" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/saskiaotto/INDperform/actions/workflows/check-full.yaml/badge.svg)](https://github.com/saskiaotto/INDperform/actions/workflows/check-full.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/INDperform)](https://CRAN.R-project.org/package=INDperform)
[![](https://cranlogs.r-pkg.org/badges/INDperform)](https://cran.r-project.org/package=INDperform)
<!-- badges: end -->

<!-- The following does not work for GitHub README files -->

<!-- <a href="https://saskiaotto.github.io/INDperform/"  -->

<!-- style="box-shadow: 0px 20px 14px -7px #9c9993; -->

<!--    background:linear-gradient(120deg, #D77020, #ffdf1f, #5F8539 ); -->

<!--    background-color:#9c9993; -->

<!--    border-radius:8px; -->

<!--    cursor:pointer; -->

<!--    color:#ffffff; -->

<!--    font-family:Arial; -->

<!--    font-size:20px; -->

<!--    font-weight:bold; -->

<!--    padding:13px 32px; -->

<!--    text-decoration:none; -->

<!--    text-shadow:0px 1px 0px #4c4c4c; -->

<!--    margin: auto; -->

<!--    display: block; -->

<!--    width: 40%;">INDperform website</a> -->

## Overview

INDperform is an R package for validating the performance of ecological
state indicators and assessing the ecological status based on a suite of
indicators.

## Installation

Install the development version from GitHub using ‘remotes’:

``` r
# install.packages("remotes")
remotes::install_github("saskiaotto/INDperform")
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
[https://doi.org/10.1016/j.ecolind.2017.05.045](https://www.sciencedirect.com/science/article/abs/pii/S1470160X1730300X)*

## Important News

In Version 0.2.2 some adjustments were made to account for changes in
packages INDperform depends on and a minor bug fixed in the NRMSE model
prediction plot. Some of the plotting functions include now also titles
in the individual panels. A data validation routine was added to check
for unwanted characters in indicator or pressure names which caused
models to not build. For more details see the
[NEWS](https://saskiaotto.github.io/INDperform/news/index.html) file.

-----

In Version 0.2.1 a minor bug with different internal test results under
different R versions was fixed by modifying some tests. But this bug did
not affect the modelling results or performance of the previous version.

-----

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
