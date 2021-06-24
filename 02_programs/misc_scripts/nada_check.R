#NADA search
#written by Brian Stacy on June 24 2021

library(tidyverse)
library(httr)


dir <- "C:/Users/wb469649/Documents/Github/SPI/01_raw_data/2.4_DSDS/"

#read in NADA SPI microdata file 
nada_raw_df <- read_csv(paste0(dir,"D2.4.NADA.csv")) 

#get list of NADA sites to check
nada_sites <- nada_raw_df %>%
  filter(date==2019) 

if (exists('working_nada_df')) {
  rm('working_nada_df')
}

#create database with working connections
for (cntry in nada_sites$iso3c) {
  
  temp_df <- nada_sites %>%
    filter(iso3c==cntry)
  
  site <- temp_df$NADA_text
  
  if (!is.na(site)) {
    tryCatch({
      req<-GET(site)
      
      if (req$status_code==200) {
        status=TRUE
      } else {
        status=FALSE
      }
      
      if (!exists('working_nada_df')) {
        working_nada_df <- temp_df %>%
          mutate(NADA_working=status)
      } else {
        temp_df <- temp_df %>%
          mutate(NADA_working=status)
        
        working_nada_df <- working_nada_df %>%
          bind_rows(temp_df)
      }
    }, error=function(e){})

    
  }
  
}


