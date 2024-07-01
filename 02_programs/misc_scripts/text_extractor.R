library(tidyverse)
library(rmarkdown)
library(knitr)
library(here)

dir <- here()

opts_chunk$set(list(echo = FALSE, eval = FALSE))
knit(paste0(dir, "/02_programs/02-SPI_index.Rmd"))