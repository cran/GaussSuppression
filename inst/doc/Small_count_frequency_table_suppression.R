## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
htmltables = TRUE
if(htmltables){
  source("GaussKable.R")
  P = function(...) G(fun = SuppressSmallCounts, timevar = "geo", ...)
} else { 
  P = function(...) cat("Formatted table not avalable")
}

## -----------------------------------------------------------------------------
library(GaussSuppression)
dataset <- SSBtoolsData("example1")
dataset_a <- dataset[dataset$year == "2014", -4]
dataset_b <- dataset[dataset$year == "2015", -4]
dataset_a

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo"), 
                    freqVar = "freq", 
                    maxN = 1)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_a, # caption = "Table 1", 
                    dimVar = c("age", "geo"), 
                    freqVar = "freq", 
                    maxN = 1)

## -----------------------------------------------------------------------------
microdata_a <- SSBtools::MakeMicro(dataset_a, "freq")[-4]
output <- SuppressSmallCounts(data = microdata_a, 
                              dimVar = c("age", "geo"), 
                              maxN = 1)

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo", "eu"), 
                    freqVar = "freq", 
                    maxN = 2)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_a, # caption = "Table 2",  
                    dimVar = c("age", "geo", "eu"), 
                    freqVar = "freq", 
                    maxN = 2)

## -----------------------------------------------------------------------------
dimlists <- SSBtools::FindDimLists(dataset_a[c("age", "geo", "eu")])
dimlists

## ----eval=FALSE---------------------------------------------------------------
#  SuppressSmallCounts(data = dataset_a[c("age", "geo", "freq")],
#                      hierarchies = dimlists,
#                      freqVar = "freq",
#                      maxN = 2)

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_a, 
                    formula = ~age:eu + geo, 
                    freqVar = "freq", 
                    maxN = 2)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_a, # caption = "Table 3", 
                    formula = ~age:eu + geo, 
                    freqVar = "freq", 
                    maxN = 2)

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 4,
                    protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_a, # caption = "Table 4", 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 4,
                    protectZeros = FALSE)

## -----------------------------------------------------------------------------
output <- SuppressSmallCounts(data = dataset_a[-3, ], 
                              dimVar = c("age", "geo", "eu"),  
                              freqVar = "freq", 
                              maxN = 4,
                              extend0 = FALSE, 
                              structuralEmpty = TRUE)

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 3,
                    protectZeros = FALSE, 
                    secondaryZeros = TRUE)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_a, # caption = "Table 5",  
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 3,
                    protectZeros = FALSE, 
                    secondaryZeros = TRUE)

## -----------------------------------------------------------------------------
  dataset_b

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_b[-2,  ], 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 2,
                    extend0 = FALSE, 
                    structuralEmpty = TRUE)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_b[-2,  ], # caption = "Table 6", 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 2,
                    extend0 = FALSE, 
                    structuralEmpty = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  SuppressSmallCounts(data = dataset_b[-2,  ],
#                      formula = ~age * (geo + eu),
#                      freqVar = "freq",
#                      maxN = 2,
#                      extend0 = FALSE,
#                      structuralEmpty = TRUE,
#                      removeEmpty = FALSE)

## -----------------------------------------------------------------------------
SuppressSmallCounts(data = dataset_b, 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 2)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset_b, # caption = "Table 7", 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 2)

## -----------------------------------------------------------------------------
output <- SuppressSmallCounts(data = dataset, 
                              formula = ~age*geo*year + eu*year,  
                              freqVar = "freq", 
                              maxN = 1, 
                              protectZeros = FALSE)
head(output)

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset, # caption = "Table 8",
                    formula = ~age*geo*year + eu*year,
                    freqVar = "freq", 
                    maxN = 1,
                    protectZeros = FALSE)

