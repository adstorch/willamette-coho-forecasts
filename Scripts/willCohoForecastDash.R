# call functions ----------------------------------------------------------
## create a list of functions
# fun.sources = lapply(
#   list(
#   "load_pack_fun.R",
#   "round_fun.R",
#   "dat_fun.R"
# ),
# function(x) paste0("Functions\\",x)
# )

## load functions
sapply(
  lapply(
    list(
      ### functions to source
      "load_pack_fun.R",
      "round_fun.R",
      "dat_fun.R"
    ),
    function(x) paste0("Functions\\",x)
  ),
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
# mod.sources = lapply(
#   list(
#     "Kalman Filter.R",
#     "log-log LM.R",
#     "untransformed LM.R"
#   ),
#   function(x) paste0("Models\\",x)
# )

## source models
sapply(
  lapply(
    list(
      ### models to source
      "Kalman Filter.R"
      # "log-log LM.R",
      # "untransformed LM.R"
    ),
    function(x) paste0("Models\\",x)
  ),
  source,
  .GlobalEnv
)

## create a list of model objects to iterate over
### these should be the same names assigned in the model files
modList <- list(
  willCohoKF.logmod
                # "willCohoLM.logmod" = willCohoLM.logmod,
                # "willCohoLM.mod" = willCohoLM.mod
                )


print(deparse(quote(modList)))
names(modList[1])
textConnection(willCohoKF.logmod)
