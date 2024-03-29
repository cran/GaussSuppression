% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RangeLimits.R
\name{RangeLimitsDefault}
\alias{RangeLimitsDefault}
\title{Default range limit function}
\usage{
RangeLimitsDefault(
  ...,
  rangePercent = 0,
  rangeMin = 0,
  primary,
  num,
  freq,
  freqVar,
  dominanceVar = NULL,
  intervalVar = NULL
)
}
\arguments{
\item{...}{Unused parameters}

\item{rangePercent}{Required interval width expressed as a percentage}

\item{rangeMin}{Minimum required width of the interval}

\item{primary}{primary}

\item{num}{num}

\item{freq}{freq}

\item{freqVar}{freqVar}

\item{dominanceVar}{dominanceVar}

\item{intervalVar}{Numerical variable(s) for interval calculations.
When \code{NULL}, \code{dominanceVar}, first \code{numVar} or \code{freqVar} will be used.}
}
\value{
matrix with named columns
}
\description{
Preliminary function
}
\examples{
dat <- SSBtoolsData("magnitude1")
dat["num2"] <- 1:nrow(dat)

SuppressDominantCells(data = dat, 
    numVar = "value", 
    formula = ~sector2 * geo + sector4 * eu, 
    contributorVar = "company", 
    n = 1:2, k = c(80, 99), 
    output = RangeOutputFunction, rangePercent = 10, rangeMin = 1)

SuppressDominantCells(data = dat, 
    numVar = c("value", "num2"), 
    formula = ~sector2 * geo + sector4 * eu, 
    contributorVar = "company", 
    n = 1:2, k = c(80, 99), 
    output = RangeOutputFunction, 
    intervalVar = c("value","freq", "num2"), 
    rangePercent = c(10, 10, 30), rangeMin = c(1, 0.2222, 2.222))

}
