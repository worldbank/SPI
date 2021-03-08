Statistical Performance Indicators
================
The primary purpose of the statistical system is to help users of statistics make better decisions or to hold those decision makers accountable. In the words of Principle 1 of the Fundamental Principles of Official Statistics, the statistics must “meet the test of practical utility”, serving “the Government, the economy and the public with data about the economic, demographic, social and environmental situation.”

The new Statistical Performance Indicators (SPI) builds on the SCI, which the World Bank has regularly published since 2004.  Our new SPI will cover many of the same elements as the SCI, such as statistical methodology, source data, and periodicity, but will also expand into new areas. The goals are to offer a framework that was forward looking, measured less mature statistical systems as well as advanced systems, covered the entire national statistical system, not just the National Statistical Office (NSO), and gives countries incentives to build a modern statistical system.  We also are committing to making our project open data and open code to build confidence in our work.

The new Statistical Performance Indicators (SPI) are designed to monitor how well countries statistical systems are meeting this purpose.  By helping countries and development partners identify the strengths and weaknesses of national statistical systems the SPI can support policy advice for countries about their national statistical systems, investment decisions for donors including the World Bank, benchmarking of national statistical systems, and advocacy for national statistics.  

We identify five key pillars of a country’s statistical performance. These are data use, data services, data products, data sources, and data infrastructure . These pillars can be presented in the form of a dashboard that can help countries identify areas for development in their statistical system. Improvements in performance can be represented as a virtuous data cycle that can become self-sustaining.

<p align = "center">
<img src = "02_programs/SPI_cycle.PNG" width = "700">
</p>


Statistics have no value unless they are used. The first pillar of the SPI is therefore data use. A successful statistical system is one that produces data products that are highly used.

In order to meet user needs, the statistical system must develop a range of services that connect data users and producers and facilitate dialogue between them. The second pillar of the SPI is therefore data services that are trusted by users. A successful statistical system is one with highly valued and well used statistical services.

The dialogue between users and suppliers in turn drives the design of statistical products that are to be created including the quality of product needed for the country requirement. This will incorporate accuracy, timeliness, frequency, comparability and levels of disaggregation. The third pillar of the SPI is therefore data products. A successful statistical system is one that generates high quality statistical indicators that can also track progress for the Sustainable Development Goals (SDGs).

In order to create the products required, the statistical system needs to make use of a variety of sources from both inside and outside the government. This will include making use of typical data collection methods like censuses and surveys, but also administrative data, geospatial data, and data generated from the private sector and from citizens.  The fourth pillar of the SPI is therefore data sources.  A successful statistical system is one which draws on all types of data sources relevant to the indicators that are to be produced.

For the cycle to be complete, capability needs continuously to be reviewed to ensure that it is enough to deliver the products, services and ultimately data use required. The fifth pillar of the SPI is therefore data infrastructure. A successful statistical system is one that develops both hard infrastructure (legislation, governance, standards) and soft infrastructure (skills, partnerships) and has the financial resources to deliver.
The 5 pillars and associated 22 dimensions of the SPI are as shown in Figure 1 below.

*Figure 1: The Dimensions and Pillars that Construct the New SPI*
<p align = "center">
<img src = "02_programs/SPI_dashboard.PNG" width = "700">
</p>


A score against each element would facilitate:    

  1. Understanding of the maturity of the national statistical system in relation to others eg quintile groups of countries could be shown against each dimension    

  2. This in turn would highlight relative strengths and weaknesses of the system and give an indication of the extent to which the official statistics could be relied upon  

  3. It would also point to which other countries the country in question could learn from as it seeks to improve and create incentives to develop in a forward looking rather than backward looking way   

  4. Time series would allow assessments to be made of progress of the system and a start point for assessments of return on investment for funding given for capacity building

  5. A dynamic view encouraging continuous improvement. As countries improve the bar for what good looks like would get higher     


Key characteristics of the SPI are: (i) uses only publicly accessible data; (ii) transparent methodology; (iii) easily replicable; (iv) provides a long-time series to track progress in performance; (v) captures outcomes and supporting elements; (vi) reflects the SDGs; (vii) facilitates at-a-glance comparisons on a global scale.
We are collecting data on indicators for the 22 dimensions above. For dissemination, the SPI will be presented both in the dashboard format above and as an index for each country. Further details on the construction of the new SPI are provided in the remainder of the document.

# How to use this repository

There are several subfolders in this repository.  Below is a list of the most important files.

Run_SPI.R contains a master file to run the two most important scripts: 01-data_prep.Rmd and 02-SPI_index.Rmd.  If you just want to reproduce the results, then simply execute this file.

  1. 01_raw_data contains the raw data for the project for each indicator.  This folder contains several subfolders linked to the 22 dimensions in our framework.

  2. 02_programs contains the cleaning code to clean each indicator.  This code is consolidated into a single R Markdown file, 01-data_prep.Rmd, which also automatically writes a technical document for the indicators.  Inside this technical document, one can find the detailed description and methodology behind each indicator.  The folder also contains code to create an overall index to compare country performance.  The code to create the index is 02-SPI_index.Rmd.

  3. 03_output_data.  This folder contains a number of final output files in either csv or stata .dta format.  The most important of these files are the SPI_data.csv, which contains the data for each country containing the final indicator values for each of our Statistical Performance Indicators.  The SPI_index.csv file contains this data as well, but also includes a set of indices, including an overall score for countries, based on the values of their indicators.  



# R code

The vast majority of code in this repository is written in the R language.  The R version used was 4.0.3 (2020-10-10).  

This repository contains several files from the R package "renv".  The renv package helps manage specific package versions used to produce the results in this repository.  Because package version conflicts can make code that runs on one system not run on another system, it is important to have a list of the specific package versions used and a workflow for accessing these specific packages.  The renv package provides this.  In order to use renv, see the renv documentation here (https://rstudio.github.io/renv/articles/renv.html).  In general, the renv::restore() command should install all packages found in the renv.lock file in this repository, so that version conflicts do not cause errors.  

There may be some issue with a few packages in particular.  These are World Bank specific packages to produce maps that conform with World Bank geospatial boundaries.  These can be downloaded here (https://github.com/worldbank/wbgviz).
