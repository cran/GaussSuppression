% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrimaryFromRisky.R
\name{PrimaryFromRiskyDefault}
\alias{PrimaryFromRiskyDefault}
\title{Additional primary cells based on risky primary cells}
\usage{
PrimaryFromRiskyDefault(x, y, risky, candidates, allDims = FALSE)
}
\arguments{
\item{x}{The model matrix}

\item{y}{A vector of numeric values with a length equal to \code{nrow(x)}}

\item{risky}{Indices to columns in \code{x} corresponding to primary cells classified
as risky (interval limits not reached)}

\item{candidates}{Indices to columns in \code{x} that are candidates for becoming
additional primary cells. Higher order cells must be included
so that parent-child relationships are seen.}

\item{allDims}{When TRUE, a primary cell is added for each dimension.
can be specified as a vector of length  \code{length(risky)}}
}
\value{
Additional primary cells as indices to columns in \code{x}.
}
\description{
The algorithm uses parent-child relationships found from the model matrix (\code{x})
}
\details{
For a single \code{risky} cell,  the algorithm can be formulated as:
\itemize{
\item Consider this cell as a \code{child} and identify all \code{parents} that are present in \code{candidates}.
\item Remove parents who are also parents of other parents (i.e., eliminate higher-level parents).
\item Identify the children of these remaining parents that are included in \code{candidates}.
\item Select the child that has the smallest value in the numeric variable (\code{y}).
}

For several \code{risky} cells, coordination takes place. See the comment below the examples.
}
\examples{

# Example inspired by suppression with maxN = 5
d1 <- SSBtoolsData("d1")
mm <- SSBtools::ModelMatrix(d1, dimVar = 1:2, crossTable = TRUE)
x <- mm$modelMatrix
y <- Matrix::crossprod(x, d1$freq)

risky <- c(13, 15, 40, 45)
candidates <- c(1:12, 14, 16, 17, 19, 21, 21, 24, 26:37, 39, 42, 44)

info <- rep("", length(y))
info[risky ] <- "risky"
info[candidates] <- "c"
cbind(mm$crossTable, y=as.vector(y), info)

PrimaryFromRiskyDefault(x = x, y = y, risky = risky, candidates = candidates)
PrimaryFromRiskyDefault(x = x, y = y, risky = 40, candidates = candidates)

# The last solution (39) is not included in the first (28, 35). 
# This is because 39 is not needed when 35 is already included.

}
\keyword{internal}
