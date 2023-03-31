## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SSBtools)
library(GaussSuppression)

dataset <- SSBtools::SSBtoolsData("d2")
microdata <- SSBtools::MakeMicro(dataset, "freq")

head(dataset)
nrow(dataset)
head(microdata)
nrow(microdata)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = "region",
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "main_income"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "county"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "county", "k_group"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
FindDimLists(dataset[c("region", "county")])
FindDimLists(dataset[c("region", "county", "k_group")])

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         dimVar = c("region", "county", "k_group"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
region_dim <- data.frame(levels = c("@", "@@", rep("@@@", 3), rep("@@", 8)),
                         codes = c("Total", "ABC", LETTERS[1:11]))
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

## -----------------------------------------------------------------------------
region2_dim <- data.frame(levels = c("@", rep(c("@@", rep("@@@" ,3)),2), rep("@@", 5)),
                          codes = c("Total", "ACE", "A", "C", "E", 
                                    "BDF", "B", "D", "F", 
                                    "G", "H", "I", "J", "K"))
region2_dim

GaussSuppressionFromData(data = dataset,
                         hierarchies = list(region = region_dim, region = region2_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

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


## -----------------------------------------------------------------------------

GaussSuppressionFromData(data = microdata,
                         formula = ~ county + main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ county : main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
dataset$income2 <- ifelse(dataset$main_income == "wages", "wages", "not_wages")
microdata$income2 <- ifelse(microdata$main_income == "wages", "wages", "not_wages")
head(dataset)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = dataset,
                         formula = ~ region * income2 + (county + k_group) * main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
set.seed(12345)
microdata$num <- sample(0:1000, nrow(microdata), replace = TRUE) 

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ region * income2 + (county + k_group) * main_income,
                         numVar = "num",
                         primary = FALSE,
                         protectZeros = FALSE)

## -----------------------------------------------------------------------------
GaussSuppressionFromData(data = microdata,
                         formula = ~ region * income2 + (county + k_group) * main_income,
                         freqVar = "freq",
                         numVar = "num",
                         primary = FALSE,
                         protectZeros = FALSE)

