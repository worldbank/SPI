#aggregates

#load packages
library(tidyverse)
library(here)
library(readxl)

#set paths
dir <- here()
raw_dir <- paste(dir, '01_raw_data', sep="/")
output_dir <- paste(dir, '03_output_data', sep="/")

#Create csv for Series metadata
metadata <- read_csv(paste(raw_dir, '/metadata/SPI_dimensions_sources.csv', sep=""))

metadata_full <- read_csv(paste(raw_dir, '/metadata/SPI_index_sources.csv', sep="")) %>%
  rename(source_name=descript) %>%
  bind_rows(metadata) %>%
  mutate(short_desc=str_extract(spi_indicator_description,boundary('sentence')))

SPI_index <- read_csv('https://github.com/worldbank/SPI/raw/master/03_output_data/SPI_index.csv') %>%
  filter(date>=2016)
  
SPI_2020 <- read_csv(paste0(output_dir, "/SPI_index.csv")) %>%
  filter(date==2020)

SPI_index <- SPI_index %>%
  bind_rows(SPI_2020) %>%
  arrange(-date, country)

#reshape to match databank format
databank_df <- SPI_index %>%
  pivot_longer(
    cols=matches('SPI.INDEX'),
    names_to = 'source_id',
    values_to = 'value'
  ) %>%
  left_join(metadata_full %>% select(source_id, source_name)) %>%
  #left_join(spi_footnote) %>%
  select(country, iso3c, date, source_name, source_id, value)

#a few finl touches
databank_df <- databank_df %>%
  mutate(value=if_else(value==-99,as.numeric(NA),value)) %>%
  mutate(source_id=case_when(
    source_id=="SPI.INDEX" ~ "IQ.SPI.OVRL",
    source_id=="SPI.INDEX.PIL1" ~ "IQ.SPI.PIL1",
    source_id=="SPI.INDEX.PIL2" ~ "IQ.SPI.PIL2",
    source_id=="SPI.INDEX.PIL3" ~ "IQ.SPI.PIL3",
    source_id=="SPI.INDEX.PIL4" ~ "IQ.SPI.PIL4",
    source_id=="SPI.INDEX.PIL5" ~ "IQ.SPI.PIL5"
  ))



###############
# create program to produce aggregates
###############

#read in classifications
# class_df <- read_excel(paste(raw_dir, '/misc/WB_Groups_FY21_CSCIDA.xlsx', sep="")) %>%
#   rename(
#     iso3c=WB_Country_Code,
#     country=WB_Country_Name,
#     WB_Group_Code=WB_Group_Code,
#     WB_Group_Name=WB_Group_Name
#   )

class_df <- read_excel(paste(raw_dir, '/misc/CLASS.xls', sep=""),sheet = "Groups") %>%
  rename(
    iso3c=CountryCode,
    country=CountryName,
    WB_Group_Code=GroupCode,
    WB_Group_Name=GroupName
  )

weight<-'none'
  
  #pull weights from WDI (either population or gni)
  if (weight!='none') {
    weights_df <- wbstats::wb_data(
      indicator=c(weight),
      start_date = 2016,
      end_date=2020,
      gapfill = TRUE,
      mrv=5,
      return_wide = FALSE
    ) %>%
      rename(weight=value)
    

    
    #produce weighted aggregate
    agg_df <- databank_df %>%
      left_join(class_df) %>%
      left_join(weights_df) %>%
      group_by(WB_Group_Code, source_id,source_name, date) %>%
      summarise(
        value=Hmisc::wtd.mean(value,weights = weight, na.rm=TRUE)
      )
    
  } else {
    
    #produce weighted aggregate
    agg_df <- databank_df %>%
      left_join(class_df) %>%
      group_by(WB_Group_Code,WB_Group_Name, date, source_id, source_name) %>%
      summarise(
        value=round(mean(value, na.rm=TRUE),2)
      ) %>%
      ungroup() %>%
      filter(!is.na(WB_Group_Name))
    
  }
    
write_excel_csv(agg_df, paste(output_dir, '/misc/WB_aggregates_SPI.csv', sep=""))
  
