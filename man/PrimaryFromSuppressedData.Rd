% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrimaryFromSuppressedData.R
\name{PrimaryFromSuppressedData}
\alias{PrimaryFromSuppressedData}
\alias{ForcedFromSuppressedData}
\alias{NotPrimaryFromSuppressedData}
\title{\code{primary} and \code{forced} from suppressed data}
\usage{
PrimaryFromSuppressedData(
  x,
  crossTable,
  suppressedData,
  forcedData = FALSE,
  totCode = FindTotCode2(x, crossTable),
  ...
)

ForcedFromSuppressedData(..., forcedData = TRUE)

NotPrimaryFromSuppressedData(..., forcedData = TRUE)
}
\arguments{
\item{x}{A (sparse) dummy matrix}

\item{crossTable}{crossTable generated by parent function}

\item{suppressedData}{A data frame or a list of data frames as output from \code{\link{GaussSuppressionFromData}}.
If the variable \code{suppressed} is not included, all rows are considered suppressed.}

\item{forcedData}{When \code{TRUE}, the suppressed coding is swapped.}

\item{totCode}{A named list of totals codes}

\item{...}{Unused parameters}
}
\value{
Logical vector to be used as \code{\link[SSBtools]{GaussSuppression}} input
}
\description{
Function for \code{\link{GaussSuppressionFromData}}
}
\details{
\code{ForcedFromSuppressedData}  uses \code{forcedData = TRUE} and hence a  vector to be use as forced is generated.
\code{NotPrimaryFromSuppressedData} is similar, but \code{TRUE} elements are replaced by \code{NA}'s.
Hence the result can be used as an extra primary vector to ensure that code combinations
not suppressed according to \code{suppressedData} are forced not to be primary suppressed.

The variables used in \code{suppressedData} in addition to \code{"suppressed"} are those with matching names in \code{crossTable}. Others are ignored.
For variables in \code{crossTable} not in \code{suppressedData}, only totals are considered.
Others rows are ignored when mathing with \code{suppressedData}.

When suppressedData is a list, the final result is the union of individual results of each data frame.
}
\examples{

z2 <- SSBtoolsData("z2")

# Data to be used as suppressedData
a <- GaussSuppressionFromData(z2, c(1, 3, 4), 5, protectZeros = FALSE)

# For alternative ways to suppress the same table
b1 <- GaussSuppressionFromData(z2, 1:4, 5)
b2 <- GaussSuppressionFromData(z2, 1:4, 5, primary = c(PrimaryDefault, PrimaryFromSuppressedData),
                               suppressedData = a)
b3 <- GaussSuppressionFromData(z2, 1:4, 5, primary = c(PrimaryDefault, PrimaryFromSuppressedData),
                               suppressedData = a, forced = ForcedFromSuppressedData)
b4 <- GaussSuppressionFromData(z2, 1:4, 5, 
             primary = c(PrimaryDefault, PrimaryFromSuppressedData, NotPrimaryFromSuppressedData),
                               suppressedData = a, forced = ForcedFromSuppressedData)

# Reducing data to rows mathing a
b1r <- b1[SSBtools::Match(a[1:2], b1[1:2]), ]
b2r <- b2[SSBtools::Match(a[1:2], b2[1:2]), ]
b3r <- b3[SSBtools::Match(a[1:2], b3[1:2]), ]
b4r <- b4[SSBtools::Match(a[1:2], b4[1:2]), ]


# Look at rows where new suppression is different from that in a  

# Both TRUE and FALSE changed
cbind(a, b1r)[b1r$suppressed != a$suppressed, c(1:5, 9:10)]

# Only  FALSE changed to TRUE (suppression is preserved)
cbind(a, b2r)[b2r$suppressed != a$suppressed, c(1:5, 9:10)]

# Only change is due to new primary suppression rule (protectZeros = TRUE) 
cbind(a, b3r)[b3r$suppressed != a$suppressed, c(1:5, 9:10)]

# No changes 
cbind(a, b4r)[b4r$suppressed != a$suppressed, c(1:5, 9:10)]


}
