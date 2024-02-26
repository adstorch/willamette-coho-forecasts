# call functions ----------------------------------------------------------
## create a list of functions
fun.sources = list(
  "Functions\\load_pack_fun.R",
  "Functions\\round_fun.R",
  "Functions\\dat_fun.R"
)

## load functions
sapply(
  fun.sources,
  source,
  .GlobalEnv
  )

# call packages -----------------------------------------------------------
load_pack_fun()

# data steps --------------------------------------------------------------
## query data
### select: (1) "Willamette Falls Dam", (2) "Coho", (3a) "January 1, 2000",
### (3b) December 31 of the year prior to when you want to forecast,
### (4) "Daily counts; save file as .xlsx to "\\~Data\\cohoDat_orig.xlsx"
browseURL("https://www.fpc.org/webapps/adultsalmon/Q_adultcounts_dataquery.php")

## read-in raw data and manipulate
### 
begyr <- 2000
endyr <- 2023
dat_fun(begyr,endyr)

# call models -------------------------------------------------------------
## create a list of models to source
mod.sources = list(
  "Models\\Kalman Filter.R",
  "Models\\log-log LM.R",
  "Models\\untransformed LM.R"
)

## source models
sapply(
  mod.sources,
  source,
  .GlobalEnv
)

## create a list of model objects to iterate over
### these should be the same names assigned in the model files
modList <- c(willCohoKF.logmod,
             willCohoLM.logmod,
             willCohoLM.mod)

