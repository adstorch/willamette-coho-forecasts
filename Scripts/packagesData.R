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
              "HDInterval")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

source("Functions\\round_fun.R")

# data steps --------------------------------------------------------------
## read-in raw data
willCohoRaw.dat <- read.xlsx(
  "Data\\cohoDat_orig.xlsx",
  sheet = 1,
  colNames = TRUE
)

willCohoManip.dat <- willCohoRaw.dat %>% 
  head(-2) %>% 
  mutate(
    Date = as.Date(
      as.numeric(
        Date
      ),
      origin = "1899-12-30"
    )
  ) %>% 
  mutate(
    year = 
      format(
        Date,
        format="%Y"
      )
  ) %>% 
  select(
    year,
    CohoAdult,
    CohoJack
  ) %>% 
  rename(
    ret_year = year
  ) %>%
  rename(
    adult_cnt = CohoAdult
  ) %>%
  rename(
    jack_cnt = CohoJack
  ) %>% 
  group_by(
    ret_year
  ) %>% 
  summarise(
    adult_cnt = sum(adult_cnt),
    jack_cnt = sum(jack_cnt)
  )

willCohoManip.dat <- willCohoManip.dat %>% 
  add_row(
    ret_year = as.character(
      as.numeric(
        tail(
          willCohoManip.dat$ret_year,1
        )
      )+1
    ),
    adult_cnt = NA,
    jack_cnt = NA
  ) %>% 
  mutate(
    jack_cnt = lag(jack_cnt)
  )
