# This file will run all scripts needed to reproduce results for the Statistical
# Performance Indicators project.  Two main files will be executed:
# 1. 01-data_prep.Rmd - This file reads in the raw data needed to produce the indicators
#                       Then, the raw data is cleaned and final indicators are produced.
# 2. 02-SPI_index.Rmd - This file takes the final indicators produced in the 01-data_prep.Rmd
#                       Then, the SPI overall scores are produced, as well as dimension sub-scores.
# Each file is self-contained, meaning they can be executed independently of one another.
# All necessary packages and libraries are called within these files.
# written by Brian Stacy Jan 20,2021

# The files in this repository use the here package for directory management.
# If you do not have this install, please install before running these files.
# You can find more information here: https://here.r-lib.org/
library(here)
library(tidyverse)


# set directory path
dir <- here()

# Additionally, packages are managed using the r package renv.
# This isn't required, but recommended in order to ensure that package vintages
# are the same as used in my system
# For more information see: https://rstudio.github.io/renv/
# To use renv, run this command:
# renv::restore()

# Run 01-SPI_data_prep.Rmd.  This is a bookdown document, which simultaneously runs
# the cleaning code and produces technical documentation.
library(rmarkdown)

rmarkdown::render(input = paste0(dir,"/02_programs/01-SPI_data_prep.Rmd"))


# Run 02-SPI_index.Rmd.  This is an Rmarkdown file, which simultaneously runs the
# code to produce the index, but also creates a word document with technical details
# and various statistics.
render(input = paste0(dir,"/02_programs/02-SPI_index.Rmd"))
