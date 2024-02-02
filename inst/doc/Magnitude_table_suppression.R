## ----include = FALSE----------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
htmltables <- TRUE
if (htmltables) {
    source("GaussKable.R")
    source("KableMagnitudeTable.R")
    P <- function(...) G(timevar = "geo", ...)
    M <- function(...) KableMagnitudeTable(..., numVar = "value", timevar = "geo", singletonMethod = "none")
} else {
    P <- function(...) cat("Formatted table not avalable")
    M <- P
}

## -----------------------------------------------------------------------------
library(GaussSuppression)
dataset <- SSBtoolsData("magnitude1")
dataset


## ----echo=FALSE---------------------------------------------------------------
M(caption = '**Table 1**: Input data with the 20 contributions.',
  dataset, formula = ~sector4:geo-1)

## -----------------------------------------------------------------------------
SuppressFewContributors(data=dataset, 
                        numVar = "value", 
                        dimVar= c("sector4", "geo"), 
                        maxN=1)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 2**: Output from `SuppressFewContributors` with `maxN = 1` (number of contributors in parenthesis)',
  data=dataset, 
  numVar = "value", 
  dimVar= c("sector4", "geo"), 
  maxN = 1,
  fun = SuppressFewContributors,
  print_expr = 'paste0(value, " (",nAll ,") ")')

## -----------------------------------------------------------------------------
SuppressDominantCells(data=dataset, 
                      numVar = "value", 
                      dimVar= c("sector4", "geo"), 
                      n = 1, k = 80, allDominance = TRUE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 3**: Output from `SuppressDominantCells` <br> with `n = 1` and `k = 80`  <br> (percentage from largest contribution in parenthesis)',
  data=dataset, 
  numVar = "value", 
  dimVar= c("sector4", "geo"), 
  n=1, k=80, allDominance = TRUE,
  fun = SuppressDominantCells,
  print_expr = 'paste0(value, " (",round(100*`primary.1:80`) ,"%) ")')

## -----------------------------------------------------------------------------
output <- SuppressDominantCells(data=dataset, 
                                numVar = "value", 
                                dimVar= c("sector4", "sector2", "geo", "eu"), 
                                n = 1:2, k = c(80, 99))
head(output)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 4**: Output from `SuppressDominantCells` <br> with `n = 1:2` and `k = c(80, 99)`',
  data=dataset, 
  numVar = "value", 
  dimVar= c("sector4", "sector2", "geo", "eu"), 
  n = 1:2, k = c(80, 99), 
  fun = SuppressDominantCells,
  print_expr = 'value')

## -----------------------------------------------------------------------------
dimlists <- SSBtools::FindDimLists(dataset[c("sector4", "sector2", "geo", "eu")])
dimlists

## -----------------------------------------------------------------------------
output <- SuppressDominantCells(data=dataset, 
                                numVar = "value", 
                                hierarchies = dimlists,  
                                n = 1:2, k = c(80, 99))

## -----------------------------------------------------------------------------
output <- SuppressFewContributors(data=dataset, 
                                  numVar = "value", 
                                  formula = ~sector2*geo + sector4*eu, 
                                  maxN=2,
                                  removeEmpty = FALSE)
head(output)
tail(output)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 5**: Output from `SuppressFewContributors` with `maxN = 2` <br> (number of contributors in parenthesis)',
                                  data=dataset, 
                                  numVar = "value", 
                                  formula = ~sector2*geo + sector4*eu, 
                                  maxN=2,
                                  removeEmpty = FALSE,
                                  fun = SuppressFewContributors,
                                  print_expr = 'paste0(value, " (",nAll ,") ")')

## ----echo=FALSE---------------------------------------------------------------
M(caption = '**Table 6**: The "value" data aggregated according to hierarchy and contributor',
  dataset, dimVar = c("sector4", "sector2", "geo", "eu"),   contributorVar = "company")

## -----------------------------------------------------------------------------
output <- SuppressFewContributors(data=dataset, 
                                  numVar = "value", 
                                  dimVar = c("sector4", "sector2", "geo", "eu"),
                                  maxN=2,
                                  contributorVar = "company")
head(output)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 7**: Output from `SuppressFewContributors` with `maxN = 2` and with `contributorVar = "company"` (number contributors in parenthesis)',
                                  data=dataset, 
                                  numVar = "value", 
                                  dimVar = c("sector4", "sector2", "geo", "eu"), 
                                  maxN=2,
                                  contributorVar = "company",
                                  fun = SuppressFewContributors,
                                  print_expr = 'paste0(value, " (",nAll ,") ")')

## -----------------------------------------------------------------------------
output <- SuppressDominantCells(data=dataset, 
                                numVar = "value", 
                                formula = ~sector2*geo + sector4*eu,
                                contributorVar = "company",
                                n = 1:2, k = c(80, 99))
head(output)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 8**: Output from `SuppressDominantCells`  with `n = 1:2` and <br> `k = c(80, 99)` and with `contributorVar = "company"`',
  data=dataset, 
  numVar = "value", 
  formula = ~sector2*geo + sector4*eu,
  contributorVar = "company",
  n = 1:2, k = c(80, 99), 
  fun = SuppressDominantCells,
  print_expr = 'value')

## -----------------------------------------------------------------------------
output <- SuppressDominantCells(data=dataset,
                                numVar = "value", 
                                formula = ~sector4*geo + sector2*eu,
                                contributorVar = "company",
                                n = 1:2, k = c(80, 99))
head(output)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 9**: Output from `SuppressDominantCells`  with `n = 1:2` and `k = c(80, 99)` and with `contributorVar = "company"`',
  data=dataset, 
  numVar = "value", 
  formula = ~sector4*geo + sector2*eu,
  contributorVar = "company",
  n = 1:2, k = c(80, 99), 
  fun = SuppressDominantCells,
  print_expr = 'value')

