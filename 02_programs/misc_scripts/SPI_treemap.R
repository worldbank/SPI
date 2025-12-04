#written by Brian Stacy on April 8, 2021

#load libraries
library(tidyverse)
library(flextable)
library(here)
#set directories
dir <- here()

raw_dir <- paste(dir, '01_raw_data', sep="/")
output_dir <- paste(dir, '03_output_data', sep="/")

#read data

SPI <- read_csv(paste0(output_dir,'/SPI_index.csv'))
metadata_card <- read_csv(paste0(raw_dir,'/metadata/SPI_card_sources.csv')) %>%
  mutate(Pillar = case_when(
    grepl('Dimension 1', descript) ~ "Pillar 1: Data Use",
    grepl('Dimension 2', descript) ~ "Pillar 2: Data Services",
    grepl('Dimension 3', descript) ~ "Pillar 3: Data Products",
    grepl('Dimension 4', descript) ~ "Pillar 4: Data Sources",
    grepl('Dimension 5', descript) ~ "Pillar 5: Data Infrastructure",
  ))


treemap_df <- SPI %>%
  filter(date==max(date)) %>%
  # mutate(SPI.DIM4.1.INDEX=(SPI.DIM4.1.CEN.INDEX + SPI.DIM4.1.SVY.INDEX)/2
  # ) %>%
  #select(-SPI.DIM4.1.CEN.INDEX,-SPI.DIM4.1.SVY.INDEX) %>%
  select(country, starts_with("SPI.DIM") ) %>%
  pivot_longer(
    cols = starts_with("SPI.DIM"),
    names_to='source_id',
    values_to='value'
  ) 

treemap_df <- treemap_df %>%
  mutate(value=if_else(value==-99,as.numeric(NA),value),
         value=case_when(
           source_id=='SPI.DIM5.1.INDEX' ~ as.numeric(NA),
           source_id=='SPI.DIM5.5.INDEX' ~ as.numeric(NA),
           TRUE ~ value
         )
  ) %>%
  
  #mutate(value=round(value,1)) %>%
  right_join(metadata_card) %>%
  mutate(color=case_when(
    value==0 ~ "#E63946",
    value>0 & value <1 ~ "#FDE74C",
    value==1 ~ "#457B9D",
    TRUE ~ "gray"
  ),
  max=1) %>%
  arrange(descript) %>%
  mutate(Score=case_when(
    Pillar == "Pillar 1: Data Use" ~ 100*value,
    Pillar == "Pillar 2: Data Services"~ 100*value/3,
    Pillar ==  "Pillar 3: Data Products"~ 100*value/4,
    Pillar == "Pillar 4: Data Sources"~ 100*value/4,
    Pillar == "Pillar 5: Data Infrastructure"~ 100*value
  )) %>%
  filter(!is.na(value)) 

#save
write_excel_csv(treemap_df, fs::path(output_dir, 'SPI_treemap.csv')
