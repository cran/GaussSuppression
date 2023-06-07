## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
htmltables <- TRUE
if(htmltables){
  source("GaussKable.R")
  P <- function(..., timevar = 2) G(fun = GaussSuppressionFromData, timevar = timevar, 
                       freqVar = "freq", primary = FALSE, protectZeros = FALSE,
                         s = c(LETTERS, "county-1", "county-2", "county-3", "small", "BIG",
                               "other", "wages", "assistance", "pensions"), 
                       ...) 
} else { 
  P <- function(...) cat("Formatted table not avalable")
}

## ----setup--------------------------------------------------------------------
library(SSBtools)
library(GaussSuppression)

dataset <- SSBtools::SSBtoolsData("d2s")

microdata <- SSBtools::MakeMicro(dataset, "freq")

head(dataset)
nrow(dataset)
head(microdata)
nrow(microdata)

## ----echo=FALSE---------------------------------------------------------------
d2ws <- SSBtools::SSBtoolsData("d2ws")
KableTable(caption = '**Table 1**:  `dataset` reshaped to wide format.',
  data = d2ws, nvar = 3, header = c("regional variables", "main_income")) 

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = "region",
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = NULL, #caption = '**Table 2**: `dimVar = "region"`',
  data=dataset, 
  dimVar = "region")

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "main_income"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = '**Table 3**: `dimVar = c("region", "main_income")`',
  data=dataset, 
  dimVar = c("region", "main_income"))

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "county"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = NULL, # caption = '**Table 4**: `dimVar = c("region", "county")`',
  data=dataset, 
  dimVar = c("region", "county"))

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "county", "size"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = NULL, # caption = '**Table 4**: `dimVar = c("region", "county", "size")`',
  data=dataset, 
  dimVar = c("region", "county", "size"))

## -----------------------------------------------------------------------------
output <- GaussSuppressionFromData(data = dataset,
                                   dimVar = c("region", "county", "size", "main_income"),
                                   freqVar = "freq",
                                   primary = FALSE,
                                   protectZeros = FALSE)
head(output)

## ----echo=FALSE---------------------------------------------------------------
P(caption = NULL,  # caption = '**Table 6**: `dimVar = c("region", "county", "size", "main_income")` ',
  data=dataset, 
  dimVar = c("region", "county", "size", "main_income"))

## -----------------------------------------------------------------------------
FindDimLists(dataset[c("region", "county")])
FindDimLists(dataset[c("region", "county", "size")])

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         dimVar = c("region", "county", "size"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
region_dim <- data.frame(levels = c("@", "@@", rep("@@@", 2), rep("@@", 4)),
                         codes = c("Total", "AB", LETTERS[1:6]))
region_dim

income_dim <- data.frame(levels = c("@", "@@", "@@", "@@@", "@@@", "@@@"),
                         codes = c("Total", "wages", "not_wages", "other", "assistance", "pensions"))
income_dim
SSBtools::DimList2Hrc(income_dim)
SSBtools::DimList2Hierarchy(income_dim)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         hierarchies = list(region = region_dim, main_income = income_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = NULL, #caption = '**Table 7**: `hierarchies = list(region = region_dim, main_income = income_dim)`',
  data=dataset, 
  hierarchies = list(region = region_dim, main_income = income_dim))

## -----------------------------------------------------------------------------
region2_dim <- data.frame(levels = c("@", rep(c("@@", rep("@@@", 2)), 2), rep("@@", 2)),
                          codes = c("Total", "AD", "A", "D",  
                                    "BF", "B", "F", 
                                    "C", "E"))
region2_dim

GaussSuppressionFromData(data = dataset,
                         hierarchies = list(region = region_dim, region = region2_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption = NULL, #caption = '**Table 8**: `hierarchies = list(region = region_dim, region = region2_dim)`',
  data=dataset, 
  hierarchies = list(region = region_dim, region = region2_dim))

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         hierarchies = list(region = region_dim, region = region2_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ region + county,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption =  NULL, # caption = '**Table 9**: `formula = ~ region + county`',
  data=dataset, 
  formula = ~ region + county)

## -----------------------------------------------------------------------------

GaussSuppressionFromData(data = microdata,
                         formula = ~ county * main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
  GaussSuppressionFromData(data = microdata,
                         dimVar = c("county" , "main_income"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)


## ----echo=FALSE---------------------------------------------------------------
P(caption =  NULL, 
  data=dataset, 
  formula = ~ county * main_income)

## -----------------------------------------------------------------------------

GaussSuppressionFromData(data = microdata,
                         formula = ~ county + main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption =  NULL, 
  data=dataset, 
  formula = ~ county + main_income)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ county:main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE---------------------------------------------------------------
P(caption =  NULL, 
  data=dataset, 
  formula = ~ county:main_income)

## -----------------------------------------------------------------------------
dataset$income2 <- ifelse(dataset$main_income == "wages", "wages", "not_wages")
microdata$income2 <- ifelse(microdata$main_income == "wages", "wages", "not_wages")
head(dataset)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         formula = ~ region * income2 + (county + size) * main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE, warning = FALSE----------------------------------------------
P(caption = NULL, #caption = '**Table 13**: `formula = ~ region * income2 + (county + size) * main_income`',
  data=dataset, 
  formula = ~ region * income2 + (county + size) * main_income)

## -----------------------------------------------------------------------------
set.seed(12345)
microdata$num <- sample(0:1000, nrow(microdata), replace = TRUE) 

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ region * income2 + (county + size) * main_income,
                         numVar = "num",
                         primary = FALSE,
                         protectZeros = FALSE)

## ----echo=FALSE, warning = FALSE----------------------------------------------
P(caption = '**Table 14**: `formula = ~ region * income2 + (county + size) * main_income` <br> In each cell: `num` with frequencies in parenthesis.',
  data=microdata, 
  formula = ~ region * income2 + (county + size) * main_income,
  numVar = "num",
  print_expr = 'paste0(num, " (", sprintf("%3d",freq) ,") ")')

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ region * income2 + (county + size) * main_income,
                         freqVar = "freq",
                         numVar = "num",
                         primary = FALSE,
                         protectZeros = FALSE)

