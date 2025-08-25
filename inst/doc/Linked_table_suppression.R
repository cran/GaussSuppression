## ----include = FALSE----------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
htmltables = TRUE
if(htmltables){
  source("GaussKable.R")
  P = function(..., timevar= "geo", fun = SuppressSmallCounts) G(fun = fun, timevar = timevar, ...)
} else { 
  P = function(...) cat("Formatted table not avalable")
}

SuppressSmallCounts1 <- function(withinArg, item, formula, ...){
  SuppressLinkedTables(..., fun = SuppressSmallCounts, withinArg = withinArg)[[item]]
}



## -----------------------------------------------------------------------------
library(GaussSuppression)
dataset <- SSBtoolsData("example1")
dataset <- dataset[c(1, 2, 4, 6, 8, 10, 12, 13, 14, 15), ]
dataset$freq = c(6, 8, 9, 1, 2, 4, 3, 7, 2, 2)
print(dataset)

## ----echo=FALSE---------------------------------------------------------------
f1 <- ~age*eu*year
f2 <- ~geo*year

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset, formula = f1, freqVar = "freq", maxN = 2, extend0 = FALSE, timevar = "eu")
P(data = dataset, formula = f2, freqVar = "freq", maxN = 2, extend0 = FALSE,timevar = "geo")

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset, fun = SuppressSmallCounts1,
  withinArg = list(list(formula = f1), list(formula = f2)), 
  freqVar = "freq", maxN = 2, extend0 = FALSE, linkedGauss = "consistent",
  formula = f1, item = 1, timevar = "eu")
P(data = dataset, fun = SuppressSmallCounts1,
  withinArg = list(list(formula = f1), list(formula = f2)), 
  freqVar = "freq", maxN = 2, extend0 = FALSE, linkedGauss = "consistent",
  formula = f2, item = 2, timevar = "geo")

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset, fun = SuppressSmallCounts1,
  withinArg = list(list(formula = f1), list(formula = f2)), 
  freqVar = "freq", maxN = 2, extend0 = FALSE, linkedGauss = "super-consistent",
  formula = f1, item = 1, timevar = "eu")
P(data = dataset, fun = SuppressSmallCounts1,
  withinArg = list(list(formula = f1), list(formula = f2)), 
  freqVar = "freq", maxN = 2, extend0 = FALSE, linkedGauss = "super-consistent",
  formula = f2, item = 2, timevar = "geo")

## -----------------------------------------------------------------------------
output <- SuppressLinkedTables(data = dataset,
              fun = SuppressSmallCounts, 
              withinArg = list(table_1 = list(dimVar = c("age", "eu", "year")), 
                               table_2 = list(dimVar = c("geo", "year"))),
              freqVar = "freq", 
              maxN = 2,
              extend0 = FALSE, 
              removeEmpty = TRUE,
              linkedGauss = "super-consistent")

print(output[["table_1"]])
print(output[["table_2"]])

## -----------------------------------------------------------------------------
h_age  <- SSBtools::FindDimLists(dataset["age"])[[1]]
h_geo  <- SSBtools::FindDimLists(dataset["geo"])[[1]]
h_eu   <- SSBtools::FindDimLists(dataset["eu"])[[1]]
h_year <- SSBtools::FindDimLists(dataset["year"])[[1]]
  
print(h_age)
print(h_geo)
print(h_eu)
print(h_year)


## ----eval = FALSE-------------------------------------------------------------
# output <- SuppressLinkedTables(data = dataset,
#               fun = SuppressSmallCounts,
#               withinArg =
#                 list(table_1 = list(hierarchies = list(age = h_age, eu = h_eu, year = h_year)),
#                      table_2 = list(hierarchies = list(geo = h_geo, year = h_year))),
#               freqVar = "freq",
#               maxN = 2,
#               extend0 = FALSE,
#               removeEmpty = TRUE,
#               linkedGauss = "super-consistent")

## ----eval = FALSE-------------------------------------------------------------
# output <- SuppressLinkedTables(data = dataset,
#               fun = SuppressSmallCounts,
#               withinArg = list(table_1 = list(formula = ~age*eu*year),
#                                table_2 = list(formula = ~geo*year)),
#               freqVar = "freq",
#               maxN = 2,
#               extend0 = FALSE,
#               linkedGauss = "super-consistent")

## -----------------------------------------------------------------------------
output <- SuppressSmallCounts(data = dataset,
              formula = list(table_1 = ~age*eu*year, table_2 = ~geo*year),   
              freqVar = "freq",
              maxN = 2,
              extend0 = FALSE,
              linkedGauss = "super-consistent") 
print(output[c(1, 6:7, 12, 19, 23, 25:28), ])

## -----------------------------------------------------------------------------
output <-  tables_by_formulas(data = dataset,
              table_fun = SuppressSmallCounts,                
              table_formulas = list(table_1 = ~age*eu*year, table_2 = ~geo*year),   
              freqVar = "freq",
              maxN = 2,
              extend0 = FALSE,
              linkedGauss = "super-consistent",
              substitute_vars = list(region = c("geo", "eu"))) 
              
print(output[c(1, 6:7, 12, 19, 23, 25:28), ])

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
lpPackage <- "highs" 
if (!requireNamespace(lpPackage, quietly = TRUE)) {
  cat(paste0("Note: The final part of this vignette requires the suggested package '", lpPackage, "' which is not installed. That part has been skipped.\n"))
  knitr::knit_exit()
}

## ----echo=FALSE---------------------------------------------------------------
P(data = dataset, fun = SuppressSmallCounts1,
  withinArg = list(list(formula = f1), list(formula = f2)), 
  freqVar = "freq", maxN = 2, extend0 = FALSE, linkedGauss = "super-consistent",
  lpPackage = "highs", rangeMin = 4,
  formula = f1, item = 1, timevar = "eu",
  print_expr = 'ifelse(is.na(lo), freq, paste0(freq, " [", lo, ", ", up, "]"))')
P(data = dataset, fun = SuppressSmallCounts1,
  withinArg = list(list(formula = f1), list(formula = f2)), 
  freqVar = "freq", maxN = 2, extend0 = FALSE, linkedGauss = "super-consistent",
  lpPackage = "highs", rangeMin = 4,
  formula = f2, item = 2, timevar = "geo",
  print_expr = 'ifelse(is.na(lo), freq, paste0(freq, " [", lo, ", ", up, "]"))')

## -----------------------------------------------------------------------------
output <- SuppressLinkedTables(data = dataset,
                               fun = SuppressSmallCounts, 
                               withinArg = list(table_1 = list(dimVar = c("age", "eu", "year")), 
                                                table_2 = list(dimVar = c("geo", "year"))),
                               freqVar = "freq", 
                               maxN = 2,
                               extend0 = FALSE, 
                               removeEmpty = TRUE,
                               linkedGauss = "super-consistent",
                               lpPackage = "highs", 
                               rangeMin = 4)

print(output[["table_1"]])
print(output[["table_2"]])

