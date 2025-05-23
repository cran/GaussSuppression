% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MagnitudeRule.R
\name{FindDominantCells}
\alias{FindDominantCells}
\title{Method for finding dominant cells according to (possibly multiple) n,k
dominance rules.}
\usage{
FindDominantCells(
  x,
  inputnum,
  num,
  n,
  k,
  charVar_groups,
  samplingWeight,
  tauArgusDominance = FALSE,
  returnContrib = FALSE,
  maxContribution = NULL
)
}
\arguments{
\item{x}{model matrix describing relationship between input and published
cells}

\item{inputnum}{vector of numeric contributions for each of the input records}

\item{num}{vector of numeric values for each of the published cells}

\item{n}{vector of integers describing n parameters in n,k rules. Must be
same length as \code{k} parameter.}

\item{k}{vector of numeric values describing k parameters in n,k rules, where
percentages are described as numbers less than 100. Must be same length as
\code{n} parameter.}

\item{charVar_groups}{vector describing which input records should be grouped}

\item{samplingWeight}{vector of sampling weights associated to input records}

\item{tauArgusDominance}{logical value, default \code{FALSE}. determines how to
handle sampling weights in the dominance rule (see details).}

\item{returnContrib}{logical value, default \code{FALSE}. If \code{TRUE} return value is
the percentage of the first n contributors}

\item{maxContribution}{Possible precalculated output from \code{MaxContribution} as input.
To speed up.}
}
\value{
logical vector describing which publish-cells need to be suppressed.
}
\description{
Supports functionality for grouping contributions according to holding
variables, as well as calculating dominance in surveys with a given sampling
weight. Two methods are implemented, depending on whether the sampling
weights sum to total population. The parameter \code{tauArgusDominance}
determines this. If \code{FALSE}, unweighted contributions are compared to weighted
cell values. If \code{TRUE}, the method described in  in the
book "Statistical Disclosure Control" (Hundepool et al 2012, p. 151) is used.
}
