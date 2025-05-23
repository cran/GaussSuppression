% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Intervals.R
\name{ComputeIntervals}
\alias{ComputeIntervals}
\title{Function for calculating intervals for suppressed tables.}
\usage{
ComputeIntervals(
  x,
  z,
  primary,
  suppressed,
  minVal = NULL,
  lpPackage = "lpSolve",
  gaussI = TRUE,
  allInt = FALSE,
  sparseConstraints = TRUE
)
}
\arguments{
\item{x}{ModelMatrix, as output from SSBtools::ModelMatrix}

\item{z}{numerical vector with length ncol(x). Corresponds to table cell values}

\item{primary}{Vector indicating primary suppressed cells. Can be logical or
integer. If integer vector, indicates the columns of x which are considered
primary suppressed.}

\item{suppressed}{Vector indicating all suppressed cells. Can be logical or
integer. If integer vector, indicates the columns of x which are considered
suppressed.}

\item{minVal}{a known minimum value for table cells. Default NULL.
Note that 'minVal' is interpreted as the limiting value for all suppressed cells.
Specifying 'minVal=0' would be redundant, as a minimum value of 0 is anyway
assumed for inner cells (see details).}

\item{lpPackage}{The name of the package used to solve linear programs. Currently,
'lpSolve' (default), 'Rsymphony',  'Rglpk' and 'highs'  are supported.}

\item{gaussI}{Boolean vector. If TRUE (default), GaussIndependent is used to
reduce size of linear program.}

\item{allInt}{Integer variables when TRUE.
See \code{all.int} parameter in \code{lpSolve} and \code{types} parameter in \code{Rsymphony} and \code{Rglpk}.}

\item{sparseConstraints}{When TRUE, a sparse constraint matrix will be input to the
solver. In the case of \code{lpSolve}, the sparse matrix is represented in triplet form
as a dense matrix with three columns, and the \code{dense.const} parameter is utilized.}
}
\description{
This function solves linear programs to determine interval boundaries
for suppressed cells.
}
\details{
Default in for \code{bounds} parameter in \code{Rsymphony_solve_LP} and \code{Rglpk_solve_LP}:
\emph{The default for each variable is a bound between 0 and \code{Inf}.}
Details in \code{lpSolve}: \emph{Note that every variable is assumed to be \verb{>= 0}!}
}
\author{
Øyvind Langsrud and Daniel Lupp
}
