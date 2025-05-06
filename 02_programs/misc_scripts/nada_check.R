# NADA search
# written by Brian Stacy on June 24 2021

library(tidyverse)
library(httr)
library(here)

dir <- here("01_raw_data","2.4_DSDS")
UPI <- 'WB469649'

# read in NADA SPI microdata file
nada_raw_df <- read_csv(paste0(dir, "/D2.4.NADA.2023.csv"))

# get list of NADA sites to check
nada_sites <- nada_raw_df 

if (exists("working_nada_df")) {
  rm("working_nada_df")
}

# create database with working connections
for (cntry in nada_sites$iso3c) {
  temp_df <- nada_sites %>%
    filter(iso3c == cntry)

  site <- temp_df$NADA_text

  if (!is.na(site)) {
    tryCatch(
      {
        req <- GET(site)
        print(req)
        if (req$status_code == 200) {
          status <- TRUE
        } else {
          status <- FALSE
        }

        if (!exists("working_nada_df")) {
          working_nada_df <- temp_df %>%
            mutate(NADA_working = status)
        } else {
          temp_df <- temp_df %>%
            mutate(NADA_working = status)

          working_nada_df <- working_nada_df %>%
            bind_rows(temp_df)
        }
      },
      error = function(e) {}
    )
  }
}

nada_sites %>%
  left_join(working_nada_df) %>%
  write_excel_csv(paste0("C:/Users/",UPI,"/WBG/Statistical Performance Indicators (SPI) - WB Group - Documents/Data/SPI 2024/03_output/Nada2024.csv"))


# write_excel_csv(working_nada_df, 'C:/Users/wb469649/OneDrive - WBG/DECIS/SPI/Data/Nada2020.csv')
