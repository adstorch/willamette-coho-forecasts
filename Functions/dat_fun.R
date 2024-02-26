# read-in and manipulate raw data -----------------------------------------
dat_fun <- function(begyr,endyr){
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
      ret_year >= begyr & ret_year <= endyr
    ) %>% 
    group_by(
      ret_year
    ) %>%
    summarise(
      adult_cnt = sum(adult_cnt),
      jack_cnt = sum(jack_cnt)
    ) 
  
  willCohoInp.dat <<- willCohoRaw.dat %>%
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
}
