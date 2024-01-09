# call functions ----------------------------------------------------------
source("Functions\\load_pack_fun.R")
source("Functions\\round_fun.R")

# call packages -----------------------------------------------------------
load_pack_fun()

# query data --------------------------------------------------------------
## select: (1) "Willamette Falls Dam", (2) "Coho", (3a) "January 1, 2000",
## (3b) December 31 of the year prior to when you want to forecast, (4) "Daily counts
## save file as .xlsx to "\\~Data\\cohoDat_orig.xlsx"
browseURL("https://www.fpc.org/webapps/adultsalmon/Q_adultcounts_dataquery.php")

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
  filter(
    ret_year > 2000 & ret_year < 2011
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
      willCohoRaw.dat$ret_year,1
    )+1,
    adult_cnt = NA,
    jack_cnt = NA
  ) %>%
  mutate(
    jack_cnt = lag(jack_cnt),
    jack_year = lag(ret_year)
  ) %>%
  relocate(jack_year,.after = ret_year)
