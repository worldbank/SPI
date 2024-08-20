#This script will prepare the SPI data for loading into the World Bank's databank
# Written by Brian Stacy
# March 4, 2021

#load packages
library(tidyverse)
library(here)
library(readxl)

latest_date=2023

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
class_df <- read_excel(paste(raw_dir, '/misc/CLASS.xlsx', sep=""), sheet="Groups") %>%
  rename(
    iso3c=CountryCode,
    country=CountryName,
    WB_Group_Code=GroupCode,
    WB_Group_Name=GroupName
  )


#prgoram to do aggregations
#user must choose a group (i.e. IFC) and a weight specified as a World Bank series code (i.e. population=SP.POP.TOTL).  Type 'none' for unweighted
SPI_agg_fun  <- function(group, weight) {
  
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
    
    #create list of countires in group
    class_join_df <- class_df %>%
      filter(WB_Group_Code==group)
    
    #produce weighted aggregate
    agg_df <- databank_df %>%
      left_join(class_join_df) %>%
      left_join(weights_df) %>%
      group_by(WB_Group_Code, source_id) %>%
      summarise(
        value=Hmisc::wtd.mean(value,weights = weight, na.rm=TRUE)
      )
    
  } else {
    
    #create list of countires in group
    class_join_df <- class_df %>%
      filter(WB_Group_Code==group)
    
    #produce weighted aggregate
    agg_df <- databank_df %>%
      left_join(class_join_df) %>%
      left_join(weights_df) %>%
      group_by(WB_Group_Code, source_id) %>%
      summarise(
        value=mean(value, na.rm=TRUE)
      )
        
  }
  
  agg_df  
  
}

###############
#now using this info, create series metadata
###############
series_metadata <- metadata_full %>%
  transmute(
    `Series Code`= source_id,
    Topic	= 'Statistical Performance',
    `Indicator Name` = source_name,
    `Short definition`	= short_desc,
    `Long definition	` = spi_indicator_description,
    `Unit of measure`	= "",
    Periodicity	= "Annual",
    Base 	= "",
    Period 	= "",	
    Other 	= "",
    notes		= "",
    `Aggregation method` 	= "",	
    `Limitations and exceptions`		= "",
    `Notes from original source`		= "",
    `General comments`		= "",
    Source	= 'See SPI technical documentation',
    `Statistical concept and methodology`	= spi_indicator_scoring,
    `Development relevance` = spi_indicator_description,
    `Related source` = "",
    links	= 'worldbank.github.com/SPI',
    `Other web links`	= "",
    `Related indicators` = "",	
    License = "CC BY-4.0",
    Type = ""
    
  )

###############
#now produce data for series
###############
#import data
SPI_index <- read_csv(paste(output_dir, 'SPI_index.csv', sep="/"))

#get raw information for footnote
spi_raw <- read_csv( file = paste(output_dir, 'SPI_data.csv', sep="/")) %>%
  select(country, iso3c, date, starts_with("RAW")) %>%
  select(-ends_with("_text"),-ends_with("_nada")) 

#pivot this data
spi_footnote <- spi_raw %>%
  mutate(across(starts_with("RAW"), as.character)) %>%
  pivot_longer(
    cols=matches('RAW'),
    names_to = 'source_id',
    values_to = 'footnote'
  ) %>%
  mutate(source_id=str_replace(source_id, "RAW", "SPI")) %>% #rename
  select(country, iso3c, date, source_id, footnote)



#reshape to match databank format
databank_df <- SPI_index %>%
  pivot_longer(
    cols=matches('SPI'),
    names_to = 'source_id',
    values_to = 'value'
  ) %>%
  left_join(metadata_full %>% select(source_id, source_name)) %>%
  left_join(spi_footnote) %>%
  select(country, iso3c, date, source_name, source_id, value, footnote)

#a few finl touches
databank_df <- databank_df %>%
  mutate(value=if_else(value==-99,as.numeric(NA),value),
         footnote=if_else(footnote=="-99",as.character(NA),footnote)) 


#pivot wider
data_bank_df_wide <- databank_df %>%
  select(-footnote) %>%
  pivot_wider(
    names_from='date',
    values_from='value',
    names_sort=TRUE
  )
  
#write to excel
excel <- list('Indicators'=databank_df, 
            'Series_Metadata'=series_metadata)

writexl::write_xlsx(databank_df,
                    path=paste(output_dir, "/SPI_databank.xlsx", sep=""))

databank_df %>% filter(date==latest_date) %>% left_join(metadata) %>% filter(!is.na(pillar)) %>% writexl::write_xlsx(
  path=paste(output_dir, "/SPI_databank_latest.xlsx", sep=""))


xlsx::write.xlsx(databank_df, file=paste(output_dir, "/SPI_data.xlsx", sep=""),
                 sheetName = 'Data')

xlsx::write.xlsx(series_metadata, file=paste(output_dir, "/SPI_data.xlsx", sep=""),
                 sheetName = 'Series')
           

