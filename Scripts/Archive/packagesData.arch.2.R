# call packages -----------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "tidymv",
              "car",
              "ggfortify",
              "gridExtra",
              "grid",
              "ggpubr",
              "openair", 
              "cvTools",
              "extrafont",
              "remotes",
              "R2jags",
              "dplyr",
              "broom.mixed",
              "boot",
              "ggmcmc",
              "scales",
              "postpack",
              "MCMCvis",
              "HDInterval",
              "stringr",
              "lubridate")# keep

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# query data --------------------------------------------------------------
## select: (1) "Willamette Falls Dam", (2) "Coho", (3a) "January 1, 2000",
## (3b) December 31 of the year prior to when you want to forecast, (4) "Daily counts
## save file as .xlsx to "\\~Data\\cohoDat_orig.xlsx"
browseURL("https://www.fpc.org/webapps/adultsalmon/Q_adultcounts_dataquery.php")

# call functions ----------------------------------------------------------
source("Functions\\round_fun.R")

# data steps --------------------------------------------------------------
## read-in raw data and manipulate
willCohoRaw.dat <- read.xlsx(
        "Data\\cohoDat_orig.xlsx",
        detectDates = TRUE) %>% 
  head(-2) %>% 
  mutate(
    Date = as.Date(Date)
    ) %>% 
  mutate(
    year = as.numeric(
      year(Date)
    )
  ) %>% 
  select(
    year,
    CohoAdult,
    CohoJack
  ) %>% 
  rename(
    ret_year = year,
    adult_cnt = CohoAdult,
    jack_cnt = CohoJack
  ) %>%
  group_by(
    ret_year
  ) %>%
  summarise(
    adult_cnt = sum(adult_cnt),
    jack_cnt = sum(jack_cnt)
  ) 

willCohoInp.dat <- willCohoRaw.dat %>%
  add_row(
    ret_year = tail(
      willCohoRaw.dat$ret_year,1### NEED TO FIX THIS
    )+1,
    adult_cnt = NA,
    jack_cnt = NA
  ) %>%
  mutate(
    jack_cnt = lag(jack_cnt),
    jack_year = lag(ret_year)
  ) %>%
  relocate(jack_year,.after = ret_year)

  

# willCohoRaw.dat$date2 <- as.Date(willCohoRaw.dat$Date)
# 
# willCohoRaw.datSub <- subset(willCohoRaw.dat, date2>"2000-01-15")
# str(willCohoRaw.dat)
# 
# 
#       as.numeric(
#         willCohoRaw.dat$Date
#       ),
#       origin = "1899-12-30"
#     )
# 
# 
# as.Date(willCohoRaw.dat$Date)
# 
# 
# 
# 
#   colClasses=c(
#     rep(
#       "character",
#       2
#     ),
#     rep(
#       "numeric",
#       2
#     )
#   )
# ) %>% 
#   head(-2)
# str(willCohoRaw.dat)
# 
# willCohoRaw.dat <- read.csv(
#   text=paste0(
#     head(
#       readLines(
#         "Data\\cohoDat_orig.csv"
#       ),
#       -2), 
#     collapse="\n"
#   ),
#   colClasses=c("character","Date","numeric","numeric"
#   )
# )
# 
# willCohoRaw.dat$Date2 <- as.Date(willCohoRaw.dat$Date)

## data manipulation
willCohoManip.dat <- willCohoRaw.dat %>% 
  # head(-2)
  # mutate(
  #   Date = as.Date(
  #     as.numeric(
  #       Date
  #     ),
  #     origin = "1899-12-30"
  #   )
  # ) %>% 
  # mutate(
  #   year = as.numeric(
  #     str_sub(Date, start= -4)
  #   )
  # ) %>% 
  # select(
  #   year,
  #   CohoAdult,
  #   CohoJack
  # ) %>% 
  # rename(
  #   ret_year = year
  # ) %>%
  # rename(
  #   adult_cnt = CohoAdult
  # ) %>%
  # rename(
  #   jack_cnt = CohoJack
  # )
  # group_by(
  #   ret_year
  # ) %>% 
  # summarise(
  #   adult_cnt = sum(adult_cnt),
  #   jack_cnt = sum(jack_cnt)
  # )

## reformat data
# willCohoInp.dat <- willCohoManip.dat %>% 
#   add_row(
#     ret_year = tail(
#           willCohoManip.dat$ret_year,1
#         )+1,
#     adult_cnt = NA,
#     jack_cnt = NA
#   ) %>% 
  mutate(
    jack_cnt = lag(jack_cnt)
  ) %>% 
  mutate(
    jack_year = lag(ret_year)
  ) %>% 
  relocate(jack_year,.after = ret_year)  


library(httr)
library(tidyverse)

POST(
  url = "https://www.fpc.org/web/apps/adultsalmon/R_adultcounts_annualtotalsquery_results.php",
  encode = "json",
  body = list(
    site = "WFA",
    startyear = "2000",
    endyear = "2023",
    species = "coho"
  )
) -> res

out <- content(res)

readr::read_fwf(
  file = out$report, 
  fwf_widths(
    widths = c(5, 5, 5, 5),
    col_names = c("Dam", "Year", "CohoAdult", "CohoJack")
  ),
  skip = 1
) %>% 
  glimpse()


library(httr2)
library(jsonlite)

URL <- "https://www.fpc.org/DataReqs/web/apps/adultsalmon/direct_download.php"

res <- POST(URL,
           query=list(site = "WFA",
                      startyear = "2000",
                      endyear = "2023",
                      species = "coho"))

dat <- fromJSON(content(res, as="text"))

str(dat$result$addressMatches)

URL <- "http://geocoding.geo.census.gov/geocoder/locations/address"

res <- req_perform(URL,
           query=list(street="3211 Providence Dr",
                      city="Anchorage",
                      state="AK",
                      zip="99508",
                      benchmark=3))

dat <- fromJSON(content(res, as="text"))

str(dat$result$addressMatches)

test <- request("https://www.fpc.org/web/apps/adultsalmon/R_adultcounts_annualtotalsquery_results.php") %>% 
  req_perform()
 <httr2_response>

test$request$body

querymaker <- function(site, startyear, endyear, species){
  paste0('{\n  region(start: ', start, ', stop: ', stop, ', chrom: "', chrom, '", reference_genome: ', ref_genome, ') {\n    clinvar_variants {\n      clinical_significance\n      clinvar_variation_id\n      gnomad {\n        exome {\n          ac\n          an\n          filters\n        }\n        genome {\n          ac\n          an\n          filters\n        }\n      }\n      gold_stars\n      hgvsc\n      hgvsp\n      in_gnomad\n      major_consequence\n      pos\n      review_status\n      transcript_id\n      variant_id\n    }\n    variants(dataset: ', dataset_id, ') {\n      consequence\n      flags\n      gene_id\n      gene_symbol\n      hgvs\n      hgvsc\n      hgvsp\n      lof\n      lof_filter\n      lof_flags\n      pos\n      rsids\n      transcript_id\n      transcript_version\n      variant_id\n      exome {\n        ac\n        ac_hemi\n        ac_hom\n        an\n        af\n        filters\n        populations {\n          id\n          ac\n          an\n          ac_hemi\n          ac_hom\n        }\n      }\n      genome {\n        ac\n        ac_hemi\n        ac_hom\n        an\n        af\n        filters\n        populations {\n          id\n          ac\n          an\n          ac_hemi\n          ac_hom\n        }\n      }\n      lof_curation {\n        verdict\n        flags\n      }\n    }\n  }\n}')
}



querymaker <- function(site, startyear, endyear, species){
  paste0('{\n  region(site: ', site, ', startyear: ', startyear, ', endyear: "', endyear, '", species: ', species, '){variants(dataset: ', dataset_id, ') {\n      variant_id\n    }\n  }\n}')
}




library(httr2)
request("https://www.fpc.org/web/apps/adultsalmon/R_adultcounts_annualtotalsquery_results.php") %>%
  req_body_json(list(site = "WFA",
                     startyear = "2000",
                     endyear = "2023",
                     species = "coho")) %>% 
  req_dry_run() %>% 
  req_perform()


  resp_body_json()


## Observations: 1,059
## Variables: 27
## $ w13             <chr> "W13", "W13", "W13", "W...
## $ sta_id          <int> 170701, 170701, 170701,...
## $ obs_dt          <int> 20000102, 20000103, 200...
## $ obs_tm          <int> 1300, 1300, 1300, 1300,...
## $ obs_type        <chr> "O", "O", "O", "O", "O"...
## $ sow             <int> 3, 1, 4, 0, 1, 5, 1, 7,...
## $ dry_temp        <int> 44, 40, 48, 26, 25, 37,...
## $ rh              <int> 66, 50, 100, 51, 53, 93...
## $ wind_dir        <int> 230, 360, 230, 360, 120...
## $ wind_sp         <int> 2, 10, 13, 14, 5, 0, 9,...
## $ fuel_10hr       <int> NA, NA, NA, NA, NA, NA,...
## $ temp_max        <int> 48, 52, 51, 54, 29, 37,...
## $ temp_min        <int> 10, 36, 28, 23, 8, 22, ...
## $ rh_max          <int> 100, 100, 100, 100, 80,...
## $ rh_min          <int> 60, 50, 100, 51, 38, 93...
## $ pp_dur          <int> 0, 0, 13, 8, 0, 8, 0, 0...
## $ pp_amt          <int> 0, 0, 150, 1020, 0, 100...
## $ wet             <chr> "N", "N", "Y", "Y", "N"...
## $ grn_gr          <chr> NA, NA, NA, NA, NA, NA,...
## $ grn_sh          <chr> NA, NA, NA, NA, NA, NA,...
## $ moist_tpe       <int> 2, 2, 2, 2, 2, 2, 2, 2,...
## $ meas_type       <int> 1, 1, 1, 1, 1, 1, 1, 1,...
## $ season_cd       <chr> NA, NA, NA, NA, NA, NA,...
## $ solar_radiation <chr> NA, NA, NA, NA, NA, NA,...
## $ wind_dir_peak   <chr> NA, NA, NA, NA, NA, NA,...
## $ wind_speed_peak <chr> NA, NA, NA, NA, NA, NA,...
## $ snow_flg        <chr> "N", "N", "N", "N", "N"...
