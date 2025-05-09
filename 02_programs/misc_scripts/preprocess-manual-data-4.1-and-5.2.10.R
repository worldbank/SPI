########################################
# Input raw data files/Excel files used in analysis
# Brian Stacy
# March 27, 2022
########################################

# Load Packages
library(tidyverse)
library(readxl)
library(rsdmx)
library(wbstats)
library(here)


# Directory for SPI excel files
excel_dir <- here("01_raw_data", 
                  "4.1_SOCS", 
                  "raw")
# Directory for SPI csv files that are created
csv_dir <- here("01_raw_data", 
                "4.1_SOCS")#paste0(excel_dir, "/03_output")
out_dir <- here("01_raw_data", 
                "4.1_SOCS")
###########
# Preliminary
###########

#download country metadata from wdi for merging
country_metadata <- wb_countries()


###############################
# SPI Dimension 1: Methodology, Standards and Classifications (MSC): 
###############################


#######
# Function to Read in SPI data
#######

# User inputs five parameters.  First is a numeric code for either Dimension 1, 2, 3, or 4.  Then is a secondary numeric code for sub-indicator i.e. sub-indicator 2.
# Next the user enters the name of the sub-indicator (i.e. CRVS).  Then the year of the data (i.e. 2018), finally how many rows to skip in original excel (usually 0 or 1 skips)
#####
# Example
#####

# System of National Accounts in use
#D1.1.MSC.SNAU_2016 <- spi_loader(1,1,'SNAU', 2016,1)
#####
# End Example
#####


spi_loader <- function(variable_num1, 
                       variable_num2,
                       variable_name, 
                       skip) {
  read_excel(path  =paste(excel_dir,
                          "D",
                          paste(variable_num1,
                                variable_num2,
                                sep = "."),
                          ".MSC.",
                          variable_name,
                          ".xlsx", 
                          sep = ""),
             sheet = "Data",
             skip  = skip,
             .name_repair = 'universal') 
}


##################
# This section will read in raw excels collected by SPI team, then keep just key info and convert to csv for indexing in github
##################




#########
# 1.12 Business process

# The Generic Statistical Business Process Model (GSBPM) aims to describe 
# statistics production in a general and process-oriented way.  It is used 
# both within and between statistical offices as a common basis for work with 
# statistics production in different ways, such as quality, efficiency, standardization, 
# and process-orientation.  It is used for all types of surveys, and "business" is not 
# related to "business statistics" but refers to the statistical office, simply expressed.  
#########




D1.12.MSC.GSBP_2023 <- spi_loader(1,12,'GSBP', 0) %>%
  mutate(GSBP=Business.Process..GSBPM.,
         iso3c=Code,
         country=Country,
         date=2024) %>%
  select(iso3c,date, GSBP) %>%
  arrange(iso3c, date) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), everything()) %>%
  write_excel_csv(
    file = paste(out_dir, "/5.2_DISM/D5.2.10.GSBP.2024.csv", sep="/" ))
###############################
# SPI Dimension 2: Censuses and Surveys (CS): 
###############################

#########
# 2.1 Population census



#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.1.CEN.POPU <- 
  read_excel(path         = paste(excel_dir,
                                  "D2.1.CEN.POPU.xlsx", 
                                  sep = "/"),
             sheet        = "Data",
             skip         = 0,
             .name_repair = 'universal')

#data contains values of past census years, (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.1.CEN.POPU <- 
  D2.1.CEN.POPU %>%
  mutate(POP.CENSUS = gsub("^,*|(?<=,),|,*$", 
                           "", 
                           Years, 
                           perl = T), 
         iso3c      = Code,
         date       = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date, 
         POP.CENSUS) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())

write_excel_csv(D2.1.CEN.POPU,
                file = paste(csv_dir, 
                             "D4.1.1.CEN.POPU.2024.csv", 
                             sep = "/" ))


#########
# 2.1 Ag census



#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.2.CEN.AGRI <-
  read_excel(path = paste(excel_dir,
                          "D2.2.CEN.AGRI.xlsx", 
                          sep = "/"),
             sheet        = "Data",
             skip         = 2,
             .name_repair = 'universal')

#data contains values of past census years, (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.2.CEN.AGRI <- D2.2.CEN.AGRI %>%
  mutate(AGRI.CENSUS = gsub("^,*|(?<=,),|,*$", 
                            "", 
                            Years, 
                            perl = T), 
         iso3c       = Code,
         date        = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date, 
         AGRI.CENSUS) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())

write_excel_csv(D2.2.CEN.AGRI,
                file = paste(csv_dir, 
                             "D4.1.1.CEN.AGRI.2024.csv", 
                             sep = "/" ))


#########
# 2.3 Business/establishment census

# Business/establishment censuses provide valuable information on all economic
# activities, number of employed and size of establishments in the economy.
# Business Register information is establishment-based and includes business
# location, organization type (e.g. subsidiary or parent), industry
# classification, and operating data (e.g., receipts and employment).
#########


#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.3.CEN.BIZZ <-
  read_excel(path = paste(excel_dir,
                          "D2.3.CEN.BIZZ.xlsx", 
                          sep = "/"),
             sheet        = "Data",
             skip         = 2,
             .name_repair = 'universal')

#data contains values of past census years, (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.3.CEN.BIZZ <- 
  D2.3.CEN.BIZZ %>%
  mutate(BIZZ.CENSUS = gsub("^,*|(?<=,),|,*$", 
                            "", 
                            Years, 
                            perl = T), 
         iso3c       = Code,
         date        = 2024) %>% #remove leading and trailing commas
  select(iso3c,
         date, 
         BIZZ.CENSUS) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())



write_excel_csv(D2.3.CEN.BIZZ,
                file = paste(csv_dir, 
                             "D4.1.3.CEN.BIZZ.2024.csv", 
                             sep = "/" ))


#########
# 2.4 Household Survey on income/consumption/expenditure/budget/Integrated Survey 

# These surveys collect data on household income (including income in kind),
# consumption and expenditure.  They typically include income, expenditure, and
# consumption surveys, household budget surveys, integrated surveys.  It is
# recommended that surveys on income and expenditure be conducted at least every
# 3 to 5 years.
#########



#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.4.SVY.HOUS <-
  read_excel(path = paste(excel_dir,
                          "D2.4.SVY.HOUS.xlsx", 
                          sep = "/"),
             sheet        = "Data",
             skip         = 2,
             .name_repair = 'universal')

#data contains values of past census years, (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.4.SVY.HOUS <- 
  D2.4.SVY.HOUS %>%
  mutate(HOUS.SURVEYS = gsub("^,*|(?<=,),|,*$", 
                             "", 
                             Houseshold.survey.on.income..consumption..expenditure..budget..Integrated.Survey, 
                             perl = T), 
         iso3c   = Code,
         country = Country,
         date    = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date, 
         HOUS.SURVEYS)  %>%
  arrange(iso3c) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())

write_excel_csv(D2.4.SVY.HOUS,
                file = paste(csv_dir, 
                             "D4.1.4.SVY.HOUS.2024.csv", 
                             sep = "/" ))

#########
# 2.5 Agriculture survey

# Agricultural surveys refer to surveys of agricultural holdings based on the
# sampling frames established by the agricultural census.  These are surveys on
# agricultural land, production, crops and livestock, aquaculture, labor and
# cost, and time use.  Some issues, such as gender and food security, are of
# interest to most agriculture surveys.
#########



#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.5.SVY.AGRI <-
  read_excel(path = paste(excel_dir,
                          "/D2.5.SVY.AGRI.xlsx", 
                          sep = "/"),
             sheet        = "Data",
             skip         = 2,
             .name_repair = 'universal')

#data contains values of past census years, (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.5.SVY.AGRI <- 
  D2.5.SVY.AGRI %>%
  mutate(AGRI.SURVEYS = gsub("^,*|(?<=,),|,*$", 
                             "", 
                             Years, 
                             perl = T), 
         iso3c   = Code,
         country = Country,
         date    = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date,
         AGRI.SURVEYS) %>%
  arrange(iso3c) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())


write_excel_csv(D2.5.SVY.AGRI,
                file = paste(csv_dir, 
                             "D4.1.5.SVY.AGRI.2024.csv", 
                             sep = "/" ))


#########
# 2.6 Labor Force Survey 

# Labor force survey is a standard household-based survey of work-related
# statistics at the national and sub-national employment or unemployment levels,
# rates or trends.  The surveys also provide the characteristics of the employed
# or unemployed, including labor force status by age or gender, breakdowns
# between employees and the self-employed, public versus private sector
# employment, multiple job-holding, hiring, job creation, and duration of
# unemployment.
#########


#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.6.SVY.LABR <- 
  read_excel(path = paste(excel_dir,
                          "/D2.6.SVY.LABR.xlsx", 
                          sep = "/"),
             sheet        = "Data",
             skip         = 2,
             .name_repair = 'universal')

#data contains values of past census years, (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.6.SVY.LABR <- 
  D2.6.SVY.LABR %>%
  mutate(LABR.SURVEYS = gsub("^,*|(?<=,),|,*$", 
                             "", 
                             Years, 
                             perl = T), 
         iso3c   = Code,
         country = Country,
         date    = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date, 
         LABR.SURVEYS)  %>%
  ungroup() %>%
  arrange(iso3c) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())



write_excel_csv(D2.6.SVY.LABR,
                file = paste(csv_dir, 
                             "D4.1.6.SVY.LABR.2024.csv", 
                             sep = "/" ))


#########
# 2.7 Health/Demographic survey

# Health surveys collect information on various aspects of health of
# populations, such as health expenditure, access, utilization, and outcomes.
# They typically include Demographic and Health Surveys.  It is recommended that
# health surveys be conducted at least every 3 to 5 years.
#########



#read in four different worksheets.  Each corresponding to NSO website, MICS, DHS, or surveys in microdata library
D2.7.SVY.HLTH.NSO <- 
  read_excel(path = paste(excel_dir,
                          "/D2.7.SVY.HLTH.xlsx", 
                          sep = "/"),
             sheet        = "NSO websites",
             skip         = 2,
             .name_repair = 'universal')


#microdata library
# D2.7.SVY.HLTH.MDL<-read_excel(path=paste(excel_dir,"/D2. CS/","/2016, 2017, 2018 & 2022 - D2.7.SVY.HLTH - REV.xlsx", sep=""),
#                               sheet="2016-19 MicrodataLibrary (MDL) ",
#                               skip=2,
#                               .name_repair = 'universal')


#Now I need to join these dataframes together.  Each dataframe has columns for each year, with 1 if it is available and missing otherwise
# I will do left_joins replacing missing values if the joining dataframe has a value.
D2.7.SVY.HLTH <- D2.7.SVY.HLTH.NSO



# Now do a final reshap to make the dataset similar to other Dimension 2 datasets
D2.7.SVY.HLTH <- D2.7.SVY.HLTH %>% 
  #do some reshaping of this df to match previous version
  #create a string to match original
  mutate(HLTH.SURVEYS = gsub("^,*|(?<=,),|,*$", 
                             "", 
                             Year, 
                             perl = T), 
         iso3c   = Country,
         country = country.name,
         date    = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date, 
         HLTH.SURVEYS)  %>%
  arrange(iso3c) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())



write_excel_csv(D2.7.SVY.HLTH,
                file = paste(csv_dir, 
                             "D4.1.7.SVY.HLTH.2024.csv", 
                             sep="/" ))

#########
# 2.8 Business/establishment survey

# The business/establishment survey provides information on employment, hours,
# and earnings of employees from a sample of business establishments including
# private and public, entities that are classified based on an establishment's
# principal activity from the business or establishment census.  Establishment
# surveys include surveys of businesses, farms, and institutions.  They may ask
# for information about the establishment itself and/or employee characteristics
# and demographics.
#########


#add in 2022 data.  Because the data doesn't include HIC countries, we append this to the original database
D2.8.SVY.BIZZ <-
  read_excel(path = paste(excel_dir,
                          "/D2.8.SVY.BIZZ.xlsx", 
                          sep = "/"),
             sheet        = "Data",
             skip         = 2,
             .name_repair = 'universal')

#data contains values of past census years, 
# (i.e. ,1987, 1997, etc), need to clean these for our purposes
D2.8.SVY.BIZZ <- 
  D2.8.SVY.BIZZ %>%
  mutate(BIZZ.SURVEYS = gsub("^,*|(?<=,),|,*$", 
                             "", 
                             Years, 
                             perl = T), 
         iso3c   = Code,
         country = Country,
         date    = 2024) %>% 
  #remove leading and trailing commas
  select(iso3c,
         date, 
         BIZZ.SURVEYS)   %>%
  arrange(iso3c) %>%
  left_join(country_metadata) %>%
  select(colnames(country_metadata), 
         everything())


write_excel_csv(D2.8.SVY.BIZZ,
                path = paste(csv_dir, 
                             "D4.1.8.SVY.BIZZ.2024.csv", 
                             sep = "/" ))

