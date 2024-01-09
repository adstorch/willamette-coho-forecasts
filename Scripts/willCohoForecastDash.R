# call functions ----------------------------------------------------------
source("Functions\\load_pack_fun.R")
source("Functions\\round_fun.R")
source("Functions\\dat_fun.R")

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
