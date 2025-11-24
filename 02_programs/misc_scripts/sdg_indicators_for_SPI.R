
library(tidyverse)
library(here)
library(flextable)
library(wbstats)
library(zoo)
library(ggpubr)
library(httr)

dir <- here()
raw_dir <- here("01_raw_data")

# parameters
upper_year <- 2024
lower_year <- 2004
window <- upper_year - lower_year # set the window

#read in iso_codes.csv and merge
iso_codes <- read_csv(here("01_raw_data","metadata", "iso_codes.csv"),
                      col_types = list(col_character(), col_character(), col_character())
)

#read WDI country metdata
country_metadata <- wb_countries()

############ SDG Data
### Get Selected SDG Data
############

### Population & Health Admin Data

#### Birth
series <- 'SG_REG_BRTH90N'
url <- "https://unstats.un.org/SDGAPI/v1/sdg/Series/DataCSV"
head <- add_headers(`Content-Type` = "application/x-www-form-urlencoded")
httr::POST(url, head, accept("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
           body = paste0("seriesCodes=", series, "&timePeriodStart=,", lower_year, "&timePeriodEnd=", upper_year),
           # content_type_json(),
           
           write_disk(paste(raw_dir, '4.2_SOAD', 'SG_REG_BRTH90N_raw.csv', sep="/"), overwrite = TRUE)
)

#read in data on births
births <- read_csv(here("01_raw_data", '4.2_SOAD', "SG_REG_BRTH90N_raw.csv")) %>%
  mutate(
    date = as.numeric(TimePeriod),
    Nature = `[Nature]`,
    geoAreaCode = as.character(GeoAreaCode)
  ) %>%
  left_join(iso_codes) %>%
  select(-country) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), everything()) %>%
  filter(!is.na(iso3c))

write_csv(births, paste(raw_dir, '4.2_SOAD', 'SG_REG_BRTH90N.csv', sep="/"))

#### Death
series <- 'SG_REG_DETH75N'
url <- "https://unstats.un.org/SDGAPI/v1/sdg/Series/DataCSV"
head <- add_headers(`Content-Type` = "application/x-www-form-urlencoded")
httr::POST(url, head, accept("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
           body = paste0("seriesCodes=", series, "&timePeriodStart=,", lower_year, "&timePeriodEnd=", upper_year),
           # content_type_json(),
           
           write_disk(paste(raw_dir, '4.2_SOAD', 'SG_REG_DETH75N_raw.csv', sep="/"), overwrite = TRUE)
)

#read in data on deaths
deaths <- read_csv(here("01_raw_data", '4.2_SOAD', "SG_REG_DETH75N_raw.csv")) %>%
  mutate(
    date = as.numeric(TimePeriod),
    Nature = `[Nature]`,
    geoAreaCode = as.character(GeoAreaCode)
  ) %>%
  left_join(iso_codes) %>%
  select(-country) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), everything()) %>%
  filter(!is.na(iso3c))


#write to csv
write_csv(deaths, paste(raw_dir, '4.2_SOAD', 'SG_REG_DETH75N.csv', sep="/"))


## Dimension 5.1: legislation and governance   

series <- 'SG_STT_FPOS'
url <- "https://unstats.un.org/SDGAPI/v1/sdg/Series/DataCSV"
head <- add_headers(`Content-Type` = "application/x-www-form-urlencoded")
httr::POST(url, head, accept("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
           body = paste0("seriesCodes=", series, "&timePeriodStart=,", lower_year, "&timePeriodEnd=", upper_year),
           # content_type_json(),
           
           write_disk(paste(raw_dir, '5.1_DILG', 'SG_STT_FPOS_raw.csv', sep="/"), overwrite = TRUE)
)
           
#read in data on statistical legislation
statistical_legislation <- read_csv(here("01_raw_data", '5.1_DILG', "SG_STT_FPOS_raw.csv")) %>%
  mutate(
    date = as.numeric(TimePeriod),
    Nature = `[Nature]`,
    geoAreaCode = as.character(GeoAreaCode)
  ) %>%
  left_join(iso_codes) %>%
  select(-country) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), everything()) %>%
  filter(!is.na(iso3c))

write_csv(statistical_legislation, paste(raw_dir, '5.1_DILG', 'SG_STT_FPOS.csv', sep="/"))


## Dimension 5.5: finance
series <- 'SG_STT_NSDSFND'
url <- "https://unstats.un.org/SDGAPI/v1/sdg/Series/DataCSV"
head <- add_headers(`Content-Type` = "application/x-www-form-urlencoded")
httr::POST(url, head, accept("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
           body = paste0("seriesCodes=", series, "&timePeriodStart=,", lower_year, "&timePeriodEnd=", upper_year),
           # content_type_json(),
           
           write_disk(paste(raw_dir, '5.5_DIFI', 'SG_STT_NSDSFND_raw.csv', sep="/"), overwrite = TRUE)
)

#read in data on statistical financing
statistical_financing <- read_csv(here("01_raw_data", "5.5_DIFI", "SG_STT_NSDSFND_raw.csv")) %>%
  mutate(
    date = as.numeric(TimePeriod),
    Nature = `[Nature]`,
    geoAreaCode = as.character(GeoAreaCode)
  ) %>%
  left_join(iso_codes) %>%
  select(-country) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), everything()) %>%
  filter(!is.na(iso3c))


write_csv(statistical_financing, paste(raw_dir, '5.5_DIFI', 'SG_STT_NSDSFND.csv', sep="/"))

