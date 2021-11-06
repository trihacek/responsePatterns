
<!-- README.md is generated from README.Rmd. Please edit that file -->

# responsePatterns

<!-- badges: start -->
<!-- badges: end -->

Some survey respondents tend to respond carelessly which complicates
data analysis. This package provides functions that make it easier to
explore resposes and identify those that may be problematic. Two core
functions of this package provide two approaches to the problem of
careless responses detection: - *rp.acors()* is based on the
auto-correlation approach that allows for a probabilistic detection of
repetitive patterns a in survey data. This function calculates
auto-correlation coefficients for all lags up to the value defined by
the max.lag parameter for each observation (respondent). Subsequently,
it assigns a percentile value to each observation (respondent) based
either on the highest absolute auto-correlation or the sum of absolute
auto-correlations. - *rp.patterns()* takes a more mechanistic approach.
It searches for repetitive patterns in the data using an iterative
algorithm. Patterns are defined based on the data: if a sequence of
values occurs more than once within an observation, it is considered a
repetition. The algorithm counts the number of repetitions for each
pattern length and then weighs this sum by the length of the pattern
(longer patterns are assigned higher weight). The total score for each
respondent is determined as the sum of these pattern length-specific
scores and is standardized to take a value between 0 and 1.

Both approaches yield scores that serve as estimates of how problematic
the observations potentially are. However, no conclusions should be made
without a closer inspection of the problematic responses. Any decision
about removing or downweighing an observation should be based on visual
inspection of the responses, the specifics of the instrument used to
collect the data, familiarity with the whole data set and the context of
the data collection process.

Both core functions return an S4 oblect of class ResponsePatterns. The
package offers several utility functions that allow to inspect to
results of the analysis more closely: - *summary* provides an overview
of the object; - *rp.indices* extracts indices and, optionally,
coefficients, from the object; - *rp.select* reorders observations and
selects those equal of above a defined percentile; - *rp.save2csv*
exports indices and, optionally, coefficients and data into a CSV file;
- *rp.hist* plots a histogram of the main “suspicion” index (see
documentation for further details); - *rp.plot* plots an individual
response for easier visual inspection and allows for graphical
formatting that eases this inspection; - *rp.plots2pdf* exports a
collection of individual plots into a PDF file.

The package comes with an N = 100 data set (*rp.simdata*) that contains
an ID variable (called “optional_ID”) and simulated responses to 20
items. We use this data set to demonstrate the functionality of the
package.

## Installation

You can install the released version of responsePatterns from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("responsePatterns")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("trihacek/responsePatterns")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(responsePatterns)
#> This is responsePatterns 0.0.0.9000. Note: this is BETA software! Please mind that the package may not be stable and report any bugs! For questions and issues, please see github.com/trihacek/responsePatterns.
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
