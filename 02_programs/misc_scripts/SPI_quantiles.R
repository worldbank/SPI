# SPI Quantiles
# Brian Stacy
# 2023-04-10
# For updating charts on website

# Load libraries
library(tidyverse)
library(here)
library(wbstats)

# set directory
dir <- here()

# Load data
SPI <- read_csv(paste0(dir, "/03_output_data/SPI_index.csv")) %>%
  mutate(
    fragile_conflict = case_when(
      country %in% c(
        'Afghanistan',
        'Burkina Faso',
        'Cameroon',
        'Central African Republic',
        'Congo, Dem. Rep.',
        'Ethiopia',
        'Iraq',
        'Mali',
        'Mozambique',
        'Myanmar',
        'Niger',
        'Nigeria',
        'Somalia',
        'South Sudan',
        'Sudan',
        'Syrian Arab Republic',
        'Ukraine',
        'West Bank and Gaza',
        'Yemen, Rep.'
      ) ~ "Conflict",
      country %in% c(
        'Burundi',
        'Chad',
        'Comoros',
        'Congo, Rep.',
        'Eritrea',
        'Guinea-Bissau',
        'Haiti',
        'Kiribati',
        'Kosovo',
        'Lebanon',
        'Libya',
        'Marshall Islands',
        'Micronesia, Fed. Sts.',
        'Papua New Guinea',
        'Sao Tome and Principe',
        'Solomon Islands',
        'Timor-Leste',
        'Tuvalu',
        'Venezuela, RB',
        'Zimbabwe'
      ) ~ "Institutional and Social Fragility",
      TRUE ~ "Other"
    )
    
  )

#add gdp per capita from world bank
gdp_df <- wb_data(
  indicator="NY.GDP.PCAP.PP.KD",
  start_date=2004,
  end_date=2024 # update
)

# add quintiles of SPI.INDEX, SPI.INDEX.PIL1, SPI.INDEX.PIL2, SPI.INDEX.PIL3, SPI.INDEX.PIL4, and SPI.INDEX.PIL5, as a categorical variable

SPI_quantiles <- SPI %>% 
  filter(date>2015) %>%
  group_by(date) %>% 
  mutate(SPI.INDEX.quintile = cut(SPI.INDEX, quantile(SPI.INDEX, probs=c(0,0.2,.4,.6,.8,1), na.rm=T), right=FALSE, include.lowest=TRUE, labels=c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top Quintile")),
         SPI.INDEX.PIL1.quintile = cut(SPI.INDEX.PIL1, breaks=c(0,60,80, 85, 90, 100), right=FALSE, include.lowest=TRUE, labels=c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top Quintile")),
         SPI.INDEX.PIL2.quintile = cut(SPI.INDEX.PIL2, quantile(SPI.INDEX.PIL2, probs=c(0,1,2,3,4,5)/5, na.rm=T), right=FALSE, include.lowest=TRUE, labels=c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top Quintile")),
         SPI.INDEX.PIL3.quintile = cut(SPI.INDEX.PIL3, quantile(SPI.INDEX.PIL3, probs=c(0,1,2,3,4,5)/5, na.rm=T), right=FALSE, include.lowest=TRUE, labels=c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top Quintile")),
         SPI.INDEX.PIL4.quintile = cut(SPI.INDEX.PIL4, quantile(SPI.INDEX.PIL4, probs=c(0,1,2,3,4,5)/5, na.rm=T), right=FALSE, include.lowest=TRUE, labels=c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top Quintile")),
         SPI.INDEX.PIL5.quintile = cut(SPI.INDEX.PIL5, quantile(SPI.INDEX.PIL5, probs=c(0,1,2,3,4,5)/5, na.rm=T), right=FALSE, include.lowest=TRUE, labels=c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top Quintile"))) %>%
  select(country, region, income, fragile_conflict, iso3c, date, starts_with('SPI.INDEX'), population) %>%
  left_join(gdp_df)


#save data as csv
write_csv(SPI_quantiles, paste0(dir, "/03_output_data/SPI_quantiles.csv"))


SPI_quantiles_2024 <- SPI_quantiles %>%
  filter(date==2024) %>%
  mutate(across(is.numeric, round,1))

SPI_quantiles_2024 %>%
  mutate(`No Group` = 1,
         jitter_y=rnorm(nrow(SPI_quantiles_2024), sd=0.1),
         jitter_x=rnorm(nrow(SPI_quantiles_2024), sd=0.1)) %>%
  write_csv(paste0(dir, "/03_output_data/SPI_quantiles_2024.csv"))

# SPI Indicators data
#---------------------------
SPI |> 
  rename(`Data Use Score`            = SPI.INDEX.PIL1, 
         `Data Services Score`       = SPI.INDEX.PIL2, 
         `Data Products Score`       = SPI.INDEX.PIL3, 
         `Data Sources Score`        = SPI.INDEX.PIL4, 
         `Data Infrastructure Score` = SPI.INDEX.PIL5, 
         `SPI Overall Score`         = SPI.INDEX) %>%
  write_csv(paste0(dir, "/03_output_data/SPI_indicators_2024.csv"))
