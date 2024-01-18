# Written by Brian Stacy on Aug 14, 2020
# Updated by Brian Stacy on January 2, 2024
# A data visualization of the Statistical Performance Indicators
# The viz will contain an overview page showing a map with the indicator values and aggregate statistics
# The viz will also contain country report page that shows details on the indicators by country

library(markdown)
library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(wbstats)
library(shinythemes)
library(httr)
library(jsonlite)
library(shinycssloaders)
library(DT)
library(skimr)
library(ggcorrplot)
library(Hmisc)
library(flextable)
library(skimr)
library(shinyWidgets)
library(highcharter)
library(officer)

#read in data and metatdata
SPI <- read_csv('SPI_index.csv') 

end_date <- 2022

metadata_raw <- read_csv('SPI_dimensions_sources.csv')

metadata <- metadata_raw %>%
    mutate(descript=paste(SPI_indicator_id,": ", spi_indicator_name," - ",source_name , sep="")) %>%
    select(source_id,  descript, spi_indicator_description,spi_indicator_scoring)

metadata <- read_csv('SPI_index_sources.csv') %>%
  bind_rows(metadata)

metadataind <- metadata %>%
  filter(str_sub(source_id, 1, 9) %in% "SPI.INDEX") %>%
  select(source_id, descript) %>%
  rename("shortname2" = "descript")

metadata_raw <- metadata_raw %>%
  mutate(shortname = ifelse(str_sub(source_id, 1, 6) %in% "SPI.D3",
                            gsub(":.*","", source_name), source_name)) %>%
  mutate(shortname = ifelse(str_sub(source_id, 1, 6) %in% "SPI.D3",
                            gsub("GOAL","SDG", shortname), shortname),
         shortname = case_when(
           source_id == "SPI.D1.5.POV" ~"Availability of Comparable Poverty Data", 
           source_id == "SPI.D1.5.CHLD.MORT" ~"Availability of Mortality rate, under-5", 
           source_id == "SPI.D1.5.DT.TDS.DPPF.XP.ZS	" ~"Quality of Debt Reporting", 
           source_id == "SPI.D1.5.SAFE.MAN.WATER" ~"Availability of Safely Managed Water Data", 
           source_id == "SPI.D1.5.LFP	" ~"Availability of Labor Force Participation Data", 
           
           source_id == "SPI.D5.1.DILG"~ "Legislation Indicator based on SDG 17.18.2", 
           source_id == "SPI.D5.1.DILG"~ "Legislation Indicator based on SDG 17.18.2", 
           source_id == "SPI.D5.5.DIFI"~ "Statistical Plan fully funded", 
           source_id == "SPI.D5.2.5.HOUS"~ "Household consumption classification", 
           source_id == "SPI.D5.2.9.MONY"~ "Monetary & financial statistics compilation", 
           source_id == "SPI.D5.2.3.CNIN"~ "National industry classification", 
           source_id == "SPI.D5.2.6.EMPL"~ "Status of employment classification", 
           source_id == "SPI.D5.2.8.FINA"~ "Govt. finance statistics compilation", 
           TRUE ~ shortname),
         shortname= if_else(is.na(shortname), "N/A", shortname))






#create named list, which can be useful later
var.labels <- metadata$descript
names(var.labels) <- metadata$source_id

#read in TopoJSON from World Bank

load('WB_geojson.Rdata')

#country info
country_info <- wb_countries() %>%
    mutate(income=income_level,
           lending=lending_type) %>%
    filter(region!="Aggregates") %>%
    select(iso3c, country, region, income, lending) %>%
  arrange(country)

#sketch for DT tables
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th('', title = 'Row Names'),
      th("SPI Overall Score", title = 'Overall Statistical Performance Indicators Index Score'),
      th("Pillar 1: Data Use", title = 'The data use (outcome) pillar is segmented by five types of users: (i) the legislature, (ii) the executive branch, (iii) civil society (including sub-national actors), (iv) academia and (v) international bodies. Each dimension would have associated indicators to measure performance. A mature system would score well across all dimensions whereas a less mature one would have weaker scores along certain dimensions. The gaps would give insights into prioritization among user groups and help answer questions as to why the existing services are not resulting in higher use of national statistics in a particular segment.'),
      th("Pillar 2: Data Services", title = 'The data services (output) pillar is segmented by four service types: (i) the quality of data releases, (ii) the richness and openness of online access, (iii) the effectiveness of advisory and analytical services related to statistics, and (iv) the availability and use of data access services such as secure microdata access. Advisory and analytical services might incorporate elements related to data stewardship services including input to national data strategies, advice on data ethics and calling out misuse of data in accordance with the Fundamental Principles of Official Statistics.'),
      th("Pillar 3: Data Products", title = 'The data products (internal process) pillar is segmented by four topics and organized into (i) social, (ii) economic, (iii) environmental, and (iv) institutional dimensions using the typology of the Sustainable Development Goals (SDGs). This approach anchors the national statistical system’s performance around the essential data required to support the achievement of the 2030 global goals, and enables comparisons across countries so that a global view can be generated while enabling country specific emphasis to reflect the user needs of that country.	'),
      th("Pillar 4: Data Sources", title = 'The data sources (input) pillar is segmented by four types of sources generated by (i) the statistical office (censuses and surveys), and sources accessed from elsewhere such as (ii) administrative data, (iii) geospatial data, and (iv) private sector data and citizen generated data. The appropriate balance between these source types will vary depending on a country’s institutional setting and the maturity of its statistical system. High scores should reflect the extent to which the sources being utilized enable the necessary statistical indicators to be generated. For example, a low score on environment statistics (in the data production pillar) may reflect a lack of use of (and low score for) geospatial data (in the data sources pillar). This type of linkage is inherent in the data cycle approach and can help highlight areas for investment required if country needs are to be met.'),
      th("Pillar 5: Data Infrastructure", title = 'The data infrastructure (capability) pillar includes hard and soft infrastructure segments, itemizing essential cross cutting requirements for an effective statistical system. The segments are: (i) legislation and governance covering the existence of laws and a functioning institutional framework for the statistical system; (ii) standards and methods addressing compliance with recognized frameworks and concepts; (iii) skills including level of skills within the statistical system and among users (statistical literacy); (iv) partnerships reflecting the need for the statistical system to be inclusive and coherent; and (v) finance mobilized both domestically and from donors.'),

    )
  )
))




# Define UI for application that draws a histogram
ui <- navbarPage(id='container',
                 windowTitle="Statistical Performance Indicators Data Explorer",
                 tags$body(HTML('
                                  
                                  <div class="navbar-header">
                                  <a href="https://www.worldbank.org" class="navbar-brand">
                                  <img title="" alt="" class="img-responsive" src="https://www.worldbank.org/content/dam/wbr/logo/logo-wb-header-en.svg"/>
                                  </a>
                                  </div>
                                  </div>
                                  
                                  <div class="container-fluid breadcrumb">
                                  <div class="row"><div class="col-xs-12 page-title">
                                  <a class="breadcrumb" href="//www.worldbank.org/en/programs/statistical-performance-indicators">Statistical Performance Indicators</a>
                                  
                                  </div></div>
                                  </div>
                                  
                                  <div class="container-fluid title">
                                  <div class="row"><div class="col-xs-12">
                                  <h1>Statistical Performance Indicators (SPI) </h1>
                                  </div></div>
                                  </div>
                                  
                                    <div class="subnav-wrapper">
                                    <div class="container subnavigation">
                                    <div class="row"><div class="col-xs-12">
                                        <ul> 
                                            <li><a title="Home" href="//www.worldbank.org/en/programs/statistical-performance-indicators">Home</a></li>
                                            <li><a title="Explore Data" href="//www.worldbank.org/en/programs/statistical-performance-indicators/Explore-Data">Explore Data</a></li>
                                            <li><a title="Framework" href="//www.worldbank.org/en/programs/statistical-performance-indicators/Framework">Framework</a></li>
                                            <li><a title="FAQ" href="//www.worldbank.org/en/programs/statistical-performance-indicators/FAQ">FAQ</a></li>
                                        </ul>
                                    </div></div>
                                    </div>
                                    ')),            
                 
  #                
  # tags$div(
  #   class="navbar-brand",
  #   tags$a(href = "https://www.worldbank.org",
  #          tags$img(
  #            title="",
  #            class="img-responsive",
  #            src="https://www.worldbank.org/content/dam/wbr/logo/logo-wb-header-en.svg"
  #          ))
  # )  ,
  # tags$div(
  #   class="container-fluid breadcrumb",
  #   tags$div(class="row",
  #            tags$div(
  #              class="col-xs-12 page-title",
  #              tags$a(
  #                class="breadcrumb",
  #                title="Statistical Performance Indicators",
  #                href="//worldbank.org/en/programs/statistical-performance-indicators"
  #              ),
  #              
  #            )),
  #   
  #title="Statistical Performance Indicators Data Explorer",
  
                 theme='cerulean.min.css',


                
                 #####################################################
                 # Welcome Section
                 ####################################################
                 # tabPanel(
                          # fluidPage(
                          #           theme = shinytheme("cerulean"),
                          #           shiny::actionButton(inputId='ab1', label="Visit SPI Homepage", 
                          #                               icon = icon("external-link-square-alt"),
                          #                               # style="color: #86C2E6; background-color: #86C2E6; border-color: #86C2E6",
                          #                               onclick ="window.open('http://www.worldbank.org/spi', '_blank')"),
                          #           includeMarkdown("header.md"),
                          #           downloadButton("downloadDataAll", "Download the SPI Data"),
                          #           h3('Indicator Metadata'),
                          #           withSpinner(DT::dataTableOutput("metadata"))
                          #           #h3('Indicator Metadata'),
                          #           #withSpinner(DT::dataTableOutput("metadata_all"))
                          # )
                 # ),
                 #####################################################
                 # Global Map and Statistics Section
                 ####################################################
                 tabPanel("Global Picture",tabName ='tab1',
                          tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "spi-template.css")
                          ),
                          # div(class="outer",
                          #     shiny::actionButton(inputId='ab1', label="Visit SPI Homepage", 
                          #                        icon = icon("external-link-square-alt"),
                          #                        # style="color: #86C2E6; background-color: #86C2E6; border-color: #86C2E6",
                          #                        onclick ="window.open('http://www.worldbank.org/spi', '_blank')"),
                              fluidRow(
                                column(3,
                                      selectizeInput("color_choices_overall", "Choose Indicator", 
                                             choices=metadata$descript,
                                             selected='SPI.INDEX')) ,

                                column(3,offset=1,
                                       selectizeInput("year_overall",
                                             "Reference Year",
                                             choices=c(2004:end_date),
                                             selected=end_date)),
                                column(2,offset=1,
                                       downloadButton("downloadDataMap", "Download"))

                          ),
                          useShinyjs(),
                          #radioButtons("togglemap", label="Show Map or Show Table", choices = c(Map = "Map", Table = "Table")),
                          wellPanel(style="background:white",
                            #withSpinner(leafletOutput("spi_map_overall", width='auto', height='75vh')),
                            withSpinner(highchartOutput("spi_map_overall", width='auto', height='75vh')),
                            p(id='map_p','The boundaries, colors, denominations and any other information shown on this map do not imply, on the part of the World Bank Group, any judgment on the legal status of any territory, or any endorsement or acceptance of such boundaries.')
                            
                            ),
                          wellPanel(
                            fluidRow(
                              column(3,offset=1,
                                     selectizeInput("country_tab_orig", "Select Countries",
                                                    choices=NULL,
                                                    selected=c("All"),
                                                    multiple=T)),
                              column(3,offset=1,
                                     selectizeInput("income_tab_orig", "Select Income Groups",
                                                    choices=NULL,
                                                    selected=c("All"),
                                                    multiple=T)),
                              column(3,offset=1,
                                     selectizeInput("region_tab_orig", "Select Region Groups",
                                                    choices=NULL,
                                                    selected=c("All"),
                                                    multiple=T))                                  
                            ),  
                            withSpinner(DT::dataTableOutput("country_table_orig",
                                                            width='100%'))
                          )
                              
                          
       
                        ),
                 

                 # 
                 # #####################################################
                 # # Pillar Scores
                 # #####################################################
                 # 
                 # tabPanel("Scores by Pillar",
                 #          div(class="outer",style='padding:50px',
                 #              
                 #              h2('Overall Scores by Pillar'),
                 #              selectizeInput("dim_year",
                 #                             "Reference Year",
                 #                             choices=c(2016:end_date),
                 #                             selected=end_date
                 #                             
                 #              ),
                 #              selectizeInput("country", "Select Countries",
                 #                             choices=NULL,
                 #                             selected=c("All"),
                 #                             multiple=T),  
                 #              downloadButton("downloadDataDim", "Download"),
                 #              withSpinner(plotlyOutput('plot_dim', width='auto', height='90vh'))
                 #          )
                 #          
                 # ),
                 # #####################################################
                 # # Summary Statistics
                 # #####################################################
                 # 
                 # tabPanel("Summary Statistics",
                 #          div(class="outer",style='padding:50px',
                 #              
                 #              h2('Summary Statistics of Indicators'),
                 #              withSpinner(DT::dataTableOutput("summary_stats_overall"))
                 #          )
                 #          
                 # ),
                 # #####################################################
                 # # Summary Statistics by Percentile
                 # #####################################################
                 # 
                 # 
                 # tabPanel("Maturity",
                 #          div(class="outer",style='padding:50px',
                 #              sidebarLayout(
                 #                sidebarPanel(
                 #                  sliderInput("pctl", "Deciles of SPI Overall Score", 
                 #                              min=1, 
                 #                              max=10, 
                 #                              step=1, 
                 #                              value=1, 
                 #                              sep="", 
                 #                              width='100%',
                 #                              animate=
                 #                                animationOptions(interval = 2000, loop = FALSE) ),
                 #                  
                 #                  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }"))
                 #                ),
                 #                mainPanel(
                 #                    h2('Compare Means by Level of Maturity'),
                 #                    p('Below, the user can examine how the mean of our statistical performance indicators change by the decile of the SPI overall score.  
                 #                      Move the slider to change the decile threshold used to calculate the means.  Only countries in a given decile are used
                 #                      to calculate the mean.  This is meant to show how the underlying indicators compare at different levels of maturity of the statistical system.'),
                 # 
                 #                    plotlyOutput('maturityplot')
                 #                    
                 #                  )
                 #              )
                 #          )
                 #          
                 # ),
                 #           
                 #####################################################
                 # Country Reports section
                 ####################################################                 
                 tabPanel("Country Reports",tabName ='tab2',
                          # div(class="outer",
                          #     shiny::actionButton(inputId='ab2', label="Visit SPI Homepage", 
                          #                         icon = icon("external-link-square-alt"),
                          #                         # style="color: #86C2E6; background-color: #86C2E6; border-color: #86C2E6",
                          #                         onclick ="window.open('http://www.worldbank.org/spi', '_blank')"),
                              fluidRow(
                                column(2,
                                       selectizeInput("country_choice",
                                                      "Choose Country",
                                                      choices=as.character(country_info$country),
                                                      selected='Afghanistan')),
                                column(2,offset=1,
                                       selectizeInput("year_choice",
                                                      "Choose Year",
                                                      choices=c(2004:end_date),
                                                      selected=end_date)),  
                                column(2,offset=1,
                                       selectizeInput("comparison_choice",
                                                      "Add Countries to Compare",
                                                      choices=as.character(country_info$country),
                                                      multiple=T                              )),
                                column(1, offset=1,
                                       materialSwitch(inputId = "toggle", label = "Single/Multi Column",
                                                      status='success'))
                                ),
                          #country report: https://github.com/worldbank/SPI/blob/master/country_reports/Albania%20Country%20Report.docx?raw=true    
                          uiOutput("rep"),

                          withSpinner(uiOutput(('fullplot' )))
                 ),
                 #####################################################
                 # Time Trends
                 #####################################################
                 
                 tabPanel("Country Time Trends",
                          # div(class="outer",style='padding:50px',
                          #     shiny::actionButton(inputId='ab3', label="Visit SPI Homepage", 
                          #                         icon = icon("external-link-square-alt"),
                          #                         # style="color: #86C2E6; background-color: #86C2E6; border-color: #86C2E6",
                          #                         onclick ="window.open('http://www.worldbank.org/spi', '_blank')"),
                              h2("Time Trends of Statistical Performance Indicators"),
                              fluidRow(
                                column(2,
                                       selectizeInput("country_choice_time",
                                                      "Choose Country",
                                                      choices=as.character(country_info$country),
                                                      selected='Afghanistan')),
                                column(2,offset=1,
                                       selectizeInput("over_time",
                                                      "Choose Indicator to Plot Over Time",
                                                      choices=as.character(var.labels),
                                                      selected='SPI Overall Score')),  
                                column(2,offset=1,
                                       selectizeInput("comparison_choice_time",
                                                      "Add Countries to Compare",
                                                      choices=as.character(country_info$country),
                                                      multiple=T                              ))
                              
                              ),
                              downloadButton("downloadDataTrends", "Download"),
                              
                              withSpinner(highchartOutput('plot_time'))

                          
                 ),
                 #####################################################
                 # Country Table section
                 ####################################################                 
                 tabPanel("Create Your Own SPI",tabName ='tab3',
                          shinyjs::useShinyjs(), #need this for some advanced features like hiding items in UI
                          #css
                          tags$head(tags$style(
                            HTML('
                                 #weights {
                                    background-color: #ffffff;
                                }
                        
                                body, label, input, button, select { 
                                  font-family: "Arial";
                                  font-size: 16px;
                                }
                                 
                                hr {border-top: 1px solid #000000;} ')
                          )),
                          # div(class="outer",  
                          #    sidebarLayout( 
                          #     
                              mainPanel(
                          #       shiny::actionButton(inputId='ab3', label="Visit SPI Homepage", 
                          #                           icon = icon("external-link-square-alt"),
                          #                           # style="color: #86C2E6; background-color: #86C2E6; border-color: #86C2E6",
                          #                           onclick ="window.open('http://www.worldbank.org/spi', '_blank')"),
                                includeMarkdown("weights.md"),
                                fluidRow(
                                  column(2,
                                    selectizeInput("country_year_choice",
                                                 "Choose Year",
                                                 choices=c(2016:end_date),
                                                 selected=end_date)),
                                  column(2,offset=1,
                                    selectizeInput("country_tab", "Select Countries",
                                                   choices=NULL,
                                                   selected=c("All"),
                                                   multiple=T)),
                                  column(2,offset=1,
                                         selectizeInput("income_tab", "Select Income Groups",
                                                        choices=NULL,
                                                        selected=c("All"),
                                                        multiple=T)),
                                  column(2,offset=1,
                                         selectizeInput("region_tab", "Select Region Groups",
                                                        choices=NULL,
                                                        selected=c("All"),
                                                        multiple=T))

                                
                                ),  
                                fluidRow(
                                  column(3,
                                       materialSwitch(inputId = "toggle_changes", label = "Show Changes from Original Scores",
                                                      status='success'))
                                ),
                                withSpinner(DT::dataTableOutput("country_table",
                                                              width='100%')),
                                withSpinner(DT::dataTableOutput("country_changes",
                                                                width='100%'))
                              ),
                              
                              sidebarPanel(id = "weights",
                                            
                                            h2("Customize Weights"),
                                            hr(),
                                            sliderInput("pillar_1", "Pillar 1: Data Use - Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                           a(id = "toggleDimension1", "Show/hide dimension weights", href = "#"),
                                           shinyjs::hidden(
                                             div(id = "dimension1",
                                                 sliderInput("dim_1_5", "Dimension 1.5: Data Use by International Organizations",
                                                             min = 0, max = 1, value = 1
                                                 )
                                             )
                                           ),
                                           hr(),
                                            sliderInput("pillar_2", "Pillar 2: Data Services - Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                           a(id = "toggleDimension2", "Show/hide dimension weights", href = "#"),
                                           shinyjs::hidden(
                                             div(id = "dimension2",

                                                 sliderInput("dim_2_1", "Dimension 2.1: Data releases",
                                                             min = 0, max = 1, value = (1/3)
                                                 ),
                                                 sliderInput("dim_2_2", "Dimension 2.2: Online access",
                                                             min = 0, max = 1, value = (1/3)
                                                 ),
                                                 sliderInput("dim_2_4", "Dimension 2.4: Data access services",
                                                             min = 0, max = 1, value = (1/3)
                                                 )
                                             )
                                           ),
                                           hr(),
                                            sliderInput("pillar_3", "Pillar 3: Data Products - Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                           a(id = "toggleDimension3", "Show/hide dimension weights", href = "#"),
                                           shinyjs::hidden(
                                             div(id = "dimension3",

                                                 sliderInput("dim_3_1", "Dimension 3: SDG 1",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_2", "Dimension 3: SDG 2",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_3", "Dimension 3: SDG 3",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_4", "Dimension 3: SDG 4",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_5", "Dimension 3: SDG 5",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_6", "Dimension 3: SDG 6",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_7", "Dimension 3: SDG 7",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_8", "Dimension 3: SDG 8",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_9", "Dimension 3: SDG 9",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_10", "Dimension 3: SDG 10",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_11", "Dimension 3: SDG 11",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_12", "Dimension 3: SDG 12",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_13", "Dimension 3: SDG 13",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_15", "Dimension 3: SDG 15",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_16", "Dimension 3: SDG 16",
                                                             min = 0, max = 1, value = (1/16)
                                                 ),
                                                 sliderInput("dim_3_17", "Dimension 3: SDG 17",
                                                             min = 0, max = 1, value = (1/16)
                                                 )
                                             )
                                           ),
                                           hr(),
                                            sliderInput("pillar_4", "Pillar 4: Data Sources - Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                           a(id = "toggleDimension4", "Show/hide dimension weights", href = "#"),
                                           shinyjs::hidden(
                                             div(id = "dimension4",
                                                 sliderInput("dim_4_1.CEN", "Dimension 4.1: Censuses",
                                                             min = 0, max = 1, value = (1/4)
                                                 ),
                                                 sliderInput("dim_4_1.SVY", "Dimension 4.1: Surveys",
                                                             min = 0, max = 1, value = (1/4)
                                                 ),                                           
                                                 sliderInput("dim_4_2", "Dimension 4.2: administrative data",
                                                             min = 0, max = 1, value = (1/4)
                                                 ),
                                                 sliderInput("dim_4_3", "Dimension 4.3: geospatial data",
                                                             min = 0, max = 1, value = (1/4)
                                                 )
                                             )
                                           ),
                                           hr(),
                                            sliderInput("pillar_5", "Pillar 5: Data Infrastructure - Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                           a(id = "toggleDimension5", "Show/hide dimension weights", href = "#"),
                                           shinyjs::hidden(
                                             div(id = "dimension5",
                                                 sliderInput("dim_5_1", "Dimension 5.1: Legislation and governance",
                                                             min = 0, max = 1, value = 0
                                                 ),
                                                 sliderInput("dim_5_2", "Dimension 5.2: Standards and Methods",
                                                             min = 0, max = 1, value = 1
                                                 ),
                                                 sliderInput("dim_5_5", "Dimension 5.5: Finance",
                                                             min = 0, max = 1, value = 0
                                                 )  
                                             )
                                           )                                        
                                            
                                            
                              )
                              
                          )
                          
                 )
                 


# Define server logic required to draw a histogram
server <- function(input, output,session) {

    
    
  output$downloadDataAll <- downloadHandler(
    filename = function() {
      'SPI_Data.csv'
    },
    content = function(file) {
      write.csv(SPI, file, row.names = FALSE)
    }
  )
  

  url <- reactive({
    tags$a(paste0("Download ",input$country_choice," Country Report"), href=paste0("https://github.com/worldbank/SPI/blob/master/country_reports/",input$country_choice,"%20Country%20Report.docx?raw=true"))
  }) 
  output$rep <- renderUI({
    tagList(url())
  })
  

    #################################################
    # Overall SPI
    #################################################
    
    ###########
    # SPI Metadata Datatable
    ###########
    
    output$metadata <- DT::renderDataTable({
        
        metadata_tab_overall <- metadata 
            #filter(Series %in% c('SPI.OVRL.SCR', 'SPI.D1.MSC', 'SPI.D2.CS', 'SPI.D3.AKI', 'SPI.D4.DPO'))
        
        DT::datatable(metadata_tab_overall, caption="Table of Chosen SPI Indicator Metadata",
                      rownames=FALSE,
                      colnames = c("Indicator ID", "Indicator Name", "Source", "Scoring"),
                      class='cell-border stripe',
                      escape = FALSE,
                      extensions = c ('Buttons', 'FixedHeader'), options=list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel'),
                          pageLength = 60,
                          scrollX = TRUE, 
                          paging=FALSE,
                          ordering=F)) 
        
    })
    
 
    # ############
    # # Plot by Pillar
    # ############
    # 
    #add country choices
    choice <- unique(as.character(country_info$country))
    choice<-append('All',choice)
    #add region choices
    region_choice <- unique(as.character(country_info$region))
    region_choice<-append('All',region_choice)
    #add income choices
    income_choice <- unique(as.character(country_info$income))
    income_choice<-append('All',income_choice)    
    # 
    updateSelectizeInput(session, 'country', choices = choice, selected=c("All"), server=TRUE)
 
    
    ###########
    # Now pull data using IDs for WDI and calculate AKI
    ###########
    
    # Availability of Key Indicator Country Score equals Weighted Score divided by Maximum Category Score time 100
    
    df_overall<- reactive({
        SPI %>%
            select(iso3c, country, region,  income, date, starts_with('SPI'), population) %>%
            filter(date==input$year_overall) 

        
        
    }) 
    
    ##################
    # Country Table
    ##################
    updateSelectizeInput(session, 'country_tab_orig', choices = choice, selected=c("All"), server=TRUE)
    updateSelectizeInput(session, 'income_tab_orig', choices = income_choice, selected=c("All"), server=TRUE)
    updateSelectizeInput(session, 'region_tab_orig', choices = region_choice, selected=c("All"), server=TRUE)
    
    
    output$country_table_orig <- DT::renderDataTable({
      
      #if (input$togglemap=="Table") {
        #get country file
        index_tab <- df_overall() %>%
          select(country, SPI.INDEX,SPI.INDEX.PIL1,SPI.INDEX.PIL2,SPI.INDEX.PIL3,
                 SPI.INDEX.PIL4,SPI.INDEX.PIL5, region, income)
        
        #colors
        col_palette <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6")
        
        col_palette2 <- c("#FFBE0B",  "#E7BB25",  "#1A9850")
        
        #calculate the breaks for the color coding
        brks <- quantile(index_tab$SPI.INDEX, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties

        
        brks1 <- quantile(index_tab$SPI.INDEX.PIL1, probs=c(1,2,3,4)/5,na.rm=T)-.001

        
        brks2 <- quantile(index_tab$SPI.INDEX.PIL2, probs=c(1,2,3,4)/5,na.rm=T)-.001

        
        brks3 <- quantile(index_tab$SPI.INDEX.PIL3, probs=c(1,2,3,4)/5,na.rm=T)-.001

        
        brks4 <- quantile(index_tab$SPI.INDEX.PIL4, probs=c(1,2,3,4)/5,na.rm=T)-.001

        
        brks5 <- quantile(index_tab$SPI.INDEX.PIL5, probs=c(1,2,3,4)/5,na.rm=T)-.001

        
        # select countries for table
        
        if (!("All" %in% input$country_tab_orig) ) {
          datatab <- index_tab %>%
            filter((country %in% input$country_tab_orig)) 
        }  else {
          datatab <- index_tab 
        }
        
        #select regions for table
        if (!("All" %in% input$region_tab_orig)) {
          datatab <- datatab %>%
            filter((region %in% input$region_tab_orig)) 
        }  else {
          datatab <- datatab
        }
        
        #select income groups for table
        if (!("All" %in% input$income_tab_orig)) {
          datatab <- datatab %>%
            filter((income %in% input$income_tab_orig)) %>%
            select(-income, -region)
        }  else {
          datatab <- datatab %>%
            select(-income, -region)
        }        
        
        
        #make nice looking table
        DT::datatable(datatab, caption=paste('Overall SPI Index in ',input$year_overall,' and Pillar Scores.', sep=""),
                      rownames=FALSE,
                      colnames = c("Country", "SPI Overall Score", "Pillar 1: Data Use", "Pillar 2: Data Services","Pillar 3: Data Products ","Pillar 4: Data Sources","Pillar 5: Data Infrastructure" ),
                      container=sketch,
                      class='cell-border stripe',
                      escape = FALSE,
                      extensions = c ('Buttons', 'FixedHeader'), 
                      options=list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel'),
                        pageLength = 60,
                        scrollX = TRUE, 
                        paging=FALSE)) %>%
          formatRound(columns=c('SPI.INDEX','SPI.INDEX.PIL1','SPI.INDEX.PIL2','SPI.INDEX.PIL3','SPI.INDEX.PIL4','SPI.INDEX.PIL5'), digits=1) %>%
          formatStyle(    'SPI.INDEX', backgroundColor = styleInterval(brks, col_palette)) %>%
          formatStyle(    'SPI.INDEX.PIL1', backgroundColor = styleInterval(brks1, col_palette)) %>%
          formatStyle(    'SPI.INDEX.PIL2', backgroundColor = styleInterval(brks2, col_palette)) %>%
          formatStyle(    'SPI.INDEX.PIL3', backgroundColor = styleInterval(brks3, col_palette)) %>%
          formatStyle(    'SPI.INDEX.PIL4', backgroundColor = styleInterval(brks4, col_palette)) %>%
          formatStyle(    'SPI.INDEX.PIL5', backgroundColor = styleInterval(brks5, col_palette)) 
      
      
    })
    
    
    # updateSelectizeInput(session, 'color_choices_overall', choices = c('SPI.OVRL.SCR', 'SPI.D1.MSC', 'SPI.D2.CS', 'SPI.D3.AKI', 'SPI.D4.DPO'), server = TRUE)
    
    map_var <- reactive({
        
        names(var.labels[match(input$color_choices_overall, var.labels)])
        
    })
    
    # output$spi_map_overall <- renderLeaflet({
    #     
    #     #if (input$togglemap=="Map") {
    #       spi_map_overall<-countries
    #       
    #       spi_map_overall@data <- spi_map_overall@data %>%
    #         mutate(iso3c=ISO_A3_EH,
    #                iso3c=if_else(ISO_A3_EH==-99,WB_A3,ISO_A3_EH)) %>%
    #           left_join(df_overall()) 
    #       
    #       palette_df <- df_overall() %>%
    #         select(map_var())
    # 
    #       brks <- quantile(palette_df[,1], probs=c(1,2,3,4)/5,na.rm=T)
    #       brks <- append(0,brks)
    #       col_p <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7")
    #       
    #       if (max(brks)<100) {
    #         brks <- append(brks,100)
    #         col_p <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6")
    #         
    #       }        #create pallete
    #       pal <- colorBin(col_p, 
    #                         bins=brks,
    #                         domain=c(0,100),
    #                         na.color='grey',
    #                         pretty=FALSE)
    #       
    #       
    #       
    #       ##############
    #       #create labels
    #       ##############
    #       
    #       
    #       labels <- sprintf(
    #           "<strong>%s</strong><br/> <hr size=2>
    #           <strong> %s: %g </strong><br/> <hr size=2>
    #           ",
    #           spi_map_overall@data$WB_NAME,
    #           'Indicator Value', round(select(spi_map_overall@data, map_var())[,1], digits = 1)
    # 
    #       ) %>%
    #           lapply(htmltools::HTML)
    # 
    #       
    #       if (grepl('SPI.INDEX',map_var())) {
    #       
    #       leaflet(spi_map_overall) %>%
    #           addProviderTiles(providers$Esri.WorldStreetMap,
    #                            options = providerTileOptions(minZoom = 2, maxZoom = 5)) %>%
    #           addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
    #                       fillColor = ~pal(select(spi_map_overall@data, map_var())[,1]),
    #                       label=labels) %>%
    #           addLegend(pal=pal, 
    #                     values=~select(spi_map_overall@data, map_var())[,1], opacity=0.7, 
    #                     labFormat = function(type, cuts, p) {  # Here's the trick
    #                       paste0( c("Bottom 20%","2nd Quintile","3rd Quintile","4th Quintile","Top 20%" ))
    #                     },
    #                     title='Indicator value', position="bottomleft")      
    #       } else {
    #         
    #         pal <- colorNumeric("Blues", palette_df[,1])
    #         
    #         
    #         leaflet(spi_map_overall) %>%
    #           addProviderTiles(providers$Esri.WorldStreetMap) %>%
    #           addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
    #                       fillColor = ~pal(select(spi_map_overall@data, map_var())[,1]),
    #                       label=labels) %>%
    #           addLegend(pal=pal, 
    #                     values=~select(spi_map_overall@data, map_var())[,1], opacity=0.7, 
    #                                           title='Indicator value', position="bottomleft")      
    #       }
    #     
    #     
    #       
    # }) 
    
    
    
    output$spi_map_overall <- renderHighchart({
      
      map_df <- df_overall() %>%
        filter(!(country %in% c('Greenland'))) %>% #drop a few countries for which we do not collect data.
        #summarise(across(!! indicator,last)) %>%
        select(map_var(), everything()) %>%
        mutate(data_available= .[[1]]) %>%
        mutate(data_available=if_else(is.na(data_available) | data_available==-99, as.numeric(NA), round(as.numeric(data_available),1)))     %>%
        rename_with(~gsub(".","_",.x, fixed=TRUE)) 
      
      
      

      
      
      
      if (grepl('SPI.INDEX',map_var())) {
        
        spi_groups_quantiles <- quantile(map_df$data_available, probs=c(1,2,3,4)/5,na.rm=T)
        
        
        SPI_map <- map_df %>%
          mutate(spi_groups=case_when(
            between(data_available, spi_groups_quantiles[4],100) ~ "Top 20%",
            between(data_available, spi_groups_quantiles[3],spi_groups_quantiles[4]) ~ "4th Quintile",
            between(data_available, spi_groups_quantiles[2],spi_groups_quantiles[3]) ~ "3rd Quintile",
            between(data_available, spi_groups_quantiles[1],spi_groups_quantiles[2]) ~ "2nd Quintile",
            between(data_available, 0,spi_groups_quantiles[1]) ~ "Bottom 20%"
            
          )) %>%
          mutate(spi_groups=factor(spi_groups, 
                                   levels=c("Top 20%","4th Quintile","3rd Quintile","2nd Quintile","Bottom 20%" )),
                 value = as.numeric(spi_groups))  
          
        
        #set color pallete
        col_pal <- c("#2ec4b6","#acece7","#f1dc76","#ffbf69","#ff9f1c")  
        names(col_pal) <- c("Top 20%","4th Quintile","3rd Quintile","2nd Quintile","Bottom 20%" )
        
        SPI_highchart_map <- highchart(type = "map") %>%
          hc_add_series(mapData = countries,
                        data=SPI_map,
                        joinBy=c('ISO_A3','iso3c'),
                        name=input$color_choices_overall,
                        value='value',
                        borderColor='white',
                        tooltip = list(
                          pointFormat = "{point.country}: {point.data_available:,.1f} <br>
                                         <b>SPI Overall Score:</b> {point.SPI_INDEX:,.1f} <br>
                                         <b>Data User Score: </b> {point.SPI_INDEX_PIL1:,.1f} <br>
                                         <b>Data Services Score: </b> {point.SPI_INDEX_PIL2:,.1f} <br>
                                         <b>Data Products Score: </b> {point.SPI_INDEX_PIL3:,.1f} <br>
                                         <b>Data Sources Score: </b> {point.SPI_INDEX_PIL4:,.1f} <br>
                                         <b>Data Infrastructure Score: </b> {point.SPI_INDEX_PIL5:,.1f}"
                          
                        )
          ) %>%
          hc_colorAxis(dataClassColor="category", 
                       dataClasses = list(list(from=1, to=1, color="#2ec4b6", name="Top 20%"),
                                          list(from=2, to=2, color="#acece7", name="4th Quintile"),
                                          list(from=3, to=3, color="#f1dc76", name="3rd Quintile"),
                                          list(from=4, to=4, color="#ffbf69", name="2nd Quintile"),
                                          list(from=5, to=5, color="#ff9f1c", name="Bottom 20%"))) %>% 
          hc_mapNavigation(
            enabled = TRUE,
            enableMouseWheelZoom = TRUE,
            enableDoubleClickZoom = TRUE
          ) %>%
          hc_legend(
            verticalAlign = "top"
          )
        
        SPI_highchart_map
        
      } else {
        
        SPI_map <- map_df %>%
          mutate(value=data_available)
        

        SPI_highchart_map <- highchart(type = "map") %>%
          hc_add_series(mapData = countries,
                        data=SPI_map,
                        joinBy=c('ISO_A3','iso3c'),
                        name=input$color_choices_overall,
                        value='value',
                        tooltip = list(
                          pointFormat = "{point.country}: {point.data_available:,.1f} <br>
                                         <b>SPI Overall Score:</b> {point.SPI_INDEX:,.1f} <br>
                                         <b>Data User Score: </b> {point.SPI_INDEX_PIL1:,.1f} <br>
                                         <b>Data Services Score: </b> {point.SPI_INDEX_PIL2:,.1f} <br>
                                         <b>Data Products Score: </b> {point.SPI_INDEX_PIL3:,.1f} <br>
                                         <b>Data Sources Score: </b> {point.SPI_INDEX_PIL4:,.1f} <br>
                                         <b>Data Infrastructure Score: </b> {point.SPI_INDEX_PIL5:,.1f}"
                        )
                        ) %>%
          hc_colorAxis(dataClassColor="category") %>%
          hc_mapNavigation(
            enabled = TRUE,
            enableMouseWheelZoom = TRUE,
            enableDoubleClickZoom = TRUE
          )

        SPI_highchart_map
        
      }
      
      
      
    }) 
    
    # Downloadable csv of selected dataset ----
    output$downloadDataMap <- downloadHandler(
      filename = function() {
        'SPI_Global_Map.csv'
      },
      content = function(file) {
        write.csv(df_overall(), file, row.names = FALSE)
      }
    )
    
    # observeEvent(input$togglemap,{
    #   req(input$togglemap)
    #   if (input$togglemap == "Map"){
    #     # hide("country_tab_orig")
    #     # hide("income_tab_orig")
    #     # hide("region_tab_orig")
    #     # hide("country_table_orig")
    # 
    #     show("spi_map_overall")
    #     show('map_p')
    #   } else {
    #     # show("country_tab_orig")
    #     # show("income_tab_orig")
    #     # show("region_tab_orig")
    #     # show("country_table_orig")
    # 
    #     hide("spi_map_overall")
    #     hide('map_p')
    #   }
    # })
    # 
 
    
    # ####
    # # SPI AKI Summary Stats
    # ###
    # 
    # output$summary_stats_overall <- DT::renderDataTable({
    #     
    #     
    #     #add function to produce weighted summary stats
    #     
    #     
    #     #produce by region
    #     sumstats<- df_overall() %>%
    #         left_join(country_info) %>%
    #         group_by(region) %>%
    #         filter(!is.na(region)) %>%
    #         select(region, starts_with('SPI'), population) %>%
    #         summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) 
    #     
    #     #produce global number
    #     sumstats_gl<- df_overall() %>%
    #         mutate(region='Global') %>%
    #         group_by(region) %>%
    #         select(region, starts_with('SPI'), population) %>%
    #         summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) 
    #     
    #     
    #     #transpose data
    #     sumstats_df_long <-sumstats_gl %>%
    #         bind_rows(sumstats) 
    #     
    #     sumstats_df <- as.data.frame(t(sumstats_df_long %>% select(-region)))
    #     colnames(sumstats_df) = sumstats_df_long$region 
    #     
    #     
    #     sumstats_df <- sumstats_df %>%
    #         rownames_to_column() %>%
    #         rename(source_id=rowname)
    #     
    #     
    #     #create labels df
    #     metadata_tab2_overall <- metadata %>% 
    #         janitor::clean_names() %>%
    #         filter(grepl('SPI',source_id)) %>%
    #         select(source_id, descript)
    #     
    #     
    #     #add variable label
    #     sumstats_df <- sumstats_df %>%
    #         left_join(metadata_tab2_overall) %>%
    #         rename(Series=source_id,
    #                Label=descript) %>%
    #         select(Series, Label, everything())
    #     
    #     DT::datatable(sumstats_df, caption=htmltools::tags$caption(
    #         style = 'caption-side: top; text-align: left;',
    #         htmltools::em("Unweighted Mean of SPI Indicators by Region.  Sub-indicators scaled to be 0-1."))    ,
    #         extensions = 'Buttons', options=list(
    #             dom = 'Bfrtip',
    #             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #             pageLength = 70)) 
    #     
    #     
    #     
    # })
    # 
   
    
    #######################
    # Country Reports
    #######################
    
    
    lolli_df <- reactive({
      df_lolli <- SPI %>%
        select(iso3c, country, region,  income, date, starts_with('SPI'), population) %>%
        filter(date==input$year_choice) %>%
        mutate(ISO_A3_EH=iso3c) 
      
      #need to get the aggregates for the income group and region
      country_select_info <- country_info %>%
        filter(country==input$country_choice)
      
      region_name<-country_select_info$region
      income_name<-country_select_info$income
      
      #produce aggregation
      region_agg <- df_lolli %>%
        filter(region==region_name) %>%
        summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
        mutate(country=region_name)
      
      income_agg <- df_lolli %>%
        filter(income==income_name) %>%
        summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
        mutate(country=income_name)
      
      #lollipop chart
      
      #check if there is a country comparison group and if so add
      if (length(input$comparison_choice)==0) {
        
        df_lolli %>%
          filter(country==input$country_choice) %>%
          bind_rows(region_agg) %>%
          bind_rows(income_agg) 
        
      } else if (length(input$comparison_choice)>0) {
        
        df_compare <- SPI %>%
          select(iso3c, country, region,  income, date, starts_with('SPI'), population) %>%
          filter(date==input$year_choice) %>%
          filter(country %in% input$comparison_choice)
        
        
        df_lolli %>%
          filter(country==input$country_choice) %>%
          bind_rows(region_agg) %>%
          bind_rows(income_agg) %>%
          bind_rows(df_compare)
      }
      
      
      
    }) 
    
    #################
    # Functions for Country Report
    #################
    
    country_report_lolli_fn <- function(variables, title, scale) {
      lollip_df_temp <- lolli_df() %>%
        select(country, starts_with('SPI')) %>%
        relocate(SPI.D3.13.CLMT, .after = SPI.D3.12.CNSP) %>%
        pivot_longer(
          cols = starts_with('SPI'),
          names_to = 'source_id',
          values_to = 'values'
        ) %>% 
        left_join(metadata_raw) %>%
        left_join(metadataind) %>%
        mutate(shortname = ifelse(!is.na(shortname2), shortname2, shortname),
               shortname2 = NULL) %>%
        filter(!is.na(shortname)) %>%
        mutate(source_name = ifelse(is.na(source_name), shortname, source_name),
               shortname=factor(shortname, ordered=TRUE),
               country=factor(country, levels=unique(country))) %>%
        mutate(indi2 = trimws(str_remove(SPI_indicator_id, "Dimension"))) %>%
        mutate(dimension = substr(indi2, 1, 1)) %>%
        mutate(dimname = paste("Pillar ", dimension),
               dimname = case_when(
                 dimname=="Pillar  1" ~ "Pillar 1: Data Use",
                 dimname=="Pillar  2" ~ "Pillar 2: Data Services",
                 dimname=="Pillar  3" ~ "Pillar 3: Data Products",
                 dimname=="Pillar  4" ~ "Pillar 4: Data Sources",
                 dimname=="Pillar  5" ~ "Pillar 5: Data Infrastructure",
                 TRUE ~ "Overall Index"
               ),
               shortname=ifelse(dimname == "Pillar  1: Data Use", str_wrap(shortname, 25), str_wrap(shortname, 12))
        ) %>%
        mutate(labtext = paste(paste(country),
                               paste(source_name),
                               paste("Score: ", values),
                               sep = "<br />"))
      
      #order the categories
      lollip_df_temp <- lollip_df_temp %>%
        mutate(shortname=factor(shortname, levels = unique(lollip_df_temp$shortname))) %>%
        filter(grepl(variables,source_id)) %>%
        select(shortname, values, dimname, country, source_name ) %>%
        mutate(values=round(values,2))
    
    
    highchart() %>%
      hc_add_series(data = lollip_df_temp,
                    hcaes(x=shortname, y = values, group = country),
                    type = "column") %>%
      hc_xAxis(type='category') %>%
      hc_yAxis(min=0,max=scale) %>%
      hc_title(text=title)
}
    
    output$fullplot <- renderUI({
      
    p0 <- country_report_lolli_fn('SPI.INDEX','Overall Index', 100)
    #   
    p1 <- country_report_lolli_fn('SPI.D1', "Pillar 1: Data Use",1)
    p2 <- country_report_lolli_fn('SPI.D2', "Pillar 2: Data Services",1)
    p3 <- country_report_lolli_fn('SPI.D3', "Pillar 3: Data Products",1)
    p4 <- country_report_lolli_fn('SPI.D4', "Pillar 4: Data Sources",1)
    p5 <- country_report_lolli_fn('SPI.D5', "Pillar 5: Data Infrastructure",1)
    

    hw_grid(p0,p1,p2,p3,p4,p5, ncol=(as.numeric(input$toggle)+1), rowheight = 300)
      
    })
      
    # #plotly
    # renderPlotly({
    #   
    #   lollip_df_temp <- lolli_df() %>%
    #     select(country, starts_with('SPI')) %>%
    #     relocate(SPI.D3.13.CLMT, .after = SPI.D3.12.CNSP) %>%
    #     pivot_longer(
    #       cols = starts_with('SPI'),
    #       names_to = 'source_id',
    #       values_to = 'values'
    #     ) %>% 
    #     left_join(metadata_raw) %>%
    #     left_join(metadataind) %>%
    #     mutate(shortname = ifelse(!is.na(shortname2), shortname2, shortname),
    #            shortname2 = NULL) %>%
    #     filter(!is.na(shortname)) %>%
    #     mutate(source_name = ifelse(is.na(source_name), shortname, source_name),
    #            shortname=factor(shortname, ordered=TRUE),
    #            country=factor(country, levels=unique(country))) %>%
    #     mutate(indi2 = trimws(str_remove(SPI_indicator_id, "Dimension"))) %>%
    #     mutate(dimension = substr(indi2, 1, 1)) %>%
    #     mutate(dimname = paste("Pillar ", dimension),
    #            dimname = case_when(
    #              dimname=="Pillar  1" ~ "Pillar 1: Data Use",
    #              dimname=="Pillar  2" ~ "Pillar 2: Data Services",
    #              dimname=="Pillar  3" ~ "Pillar 3: Data Products",
    #              dimname=="Pillar  4" ~ "Pillar 4: Data Sources",
    #              dimname=="Pillar  5" ~ "Pillar 5: Data Infrastructure",
    #              TRUE ~ "Overall Index"
    #            ),
    #            shortname=ifelse(dimname == "Pillar  1: Data Use", str_wrap(shortname, 25), str_wrap(shortname, 12))
    #     ) %>%
    #     mutate(labtext = paste(paste(country),
    #                            paste(source_name),
    #                            paste("Score: ", values),
    #                            sep = "<br />"))
    #   
    #   #order the categories
    #   lollip_df_temp <- lollip_df_temp %>%
    #     mutate(shortname=factor(shortname, levels = unique(lollip_df_temp$shortname)))
    #   
    #   p <- ggplot(lollip_df_temp) +
    #     ## For parallel coordinates/Line charts
    #     geom_line(aes(x=shortname, y = values, group = country, color = country),
    #               size = 1) +
    #     geom_point(aes(x=shortname, y = values, group = country, color = country, text = labtext),
    #                size = 1) +
    #     ## For point chart
    #     #geom_point(aes(x=shortname, y = values, group = country, color = country, text = labtext),
    #     #            size = 3) +
    #     labs(x = "", y = "") +
    #     theme_bw() +
    #     facet_wrap(vars(dimname), scales = "free", ncol = (as.numeric(input$toggle)+1)) +
    #     theme(
    #       panel.grid.minor.y = element_blank(),
    #       panel.grid.major.y = element_blank(),
    #       panel.border = element_blank(),
    #       axis.text.y=element_text(size=10),
    #       legend.text = element_text(size=8),
    #       legend.title = element_blank(),
    #       panel.spacing.y = unit(1, "lines"),
    #       strip.background = element_rect(fill = "#43ACE9"),
    #       strip.text = element_text(colour = 'white', size=13))
    #   
    #   ggplotly(p, tooltip = "text") %>%
    #     layout(autosize = TRUE, 
    #            margin = list(
    #              #l = 200, r = 0, b = 70, t = 70, 
    #              #b= 110, pad = 4
    #              )
    #     )
    #   
    # })
    
    #######################
    # Country Time Trends
    #######################
    trend_var <- reactive({
      
      names(var.labels[match(input$over_time, var.labels)])
      
    })
    
    time_trends_df <- reactive({
      df_trends <- SPI %>%
        select(iso3c, country, region,  income, date, trend_var(), population) %>%
        rename(Score=!! trend_var()) %>%
        mutate(ISO_A3_EH=iso3c) 
      
      #need to get the aggregates for the income group and region
      country_select_info <- country_info %>%
        filter(country==input$country_choice_time)
      
      region_name<-country_select_info$region
      income_name<-country_select_info$income
      
      #produce aggregation
      region_agg <- df_trends %>%
        filter(region==region_name) %>%
        group_by(date) %>%
        summarise(across(starts_with('Score'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
        mutate(country=region_name)
      
      income_agg <- df_trends %>%
        filter(income==income_name) %>%
        group_by(date) %>%
        summarise(across(starts_with('Score'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
        mutate(country=income_name)
      
      #lollipop chart
      
      #check if there is a country comparison group and if so add
      if (length(input$comparison_choice_time)==0) {
        
        df_trends %>%
          filter(country==input$country_choice_time) %>%
          bind_rows(region_agg) %>%
          bind_rows(income_agg) %>%
          filter(!is.na(Score))%>%
          mutate(Score=round(Score, 1))
        
      } else if (length(input$comparison_choice_time)>0) {
        
        df_compare <- SPI %>%
          select(iso3c, country, region,  income, date, trend_var(), population) %>%
          rename(Score=!! trend_var()) %>%
          filter(country %in% input$comparison_choice_time)
        
        
        df_trends %>%
          filter(country==input$country_choice_time) %>%
          bind_rows(region_agg) %>%
          bind_rows(income_agg) %>%
          bind_rows(df_compare) %>%
          filter(!is.na(Score)) %>%
          mutate(Score=round(Score, 1))
      }
      
      
      
    }) 
    
    output$plot_time <- renderHighchart({
      
      hchart(
        time_trends_df(),
        type='line',
        hcaes(
          x=date,
          y=Score,
          group=country
        ),
      ) %>%
        hc_xAxis(
          allowDecimals=FALSE
        ) %>%
        hc_yAxis(
          min=0
        )
      
    })
    
    ###########
    # Country Datatable
    ###########

    updateSelectizeInput(session, 'country_tab', choices = choice, selected=c("All"), server=TRUE)
    updateSelectizeInput(session, 'income_tab', choices = income_choice, selected=c("All"), server=TRUE)
    updateSelectizeInput(session, 'region_tab', choices = region_choice, selected=c("All"), server=TRUE)
    
    #show hide dimension weights
    shinyjs::onclick("toggleDimension1",
                     shinyjs::toggle(id = "dimension1", anim = TRUE))
    shinyjs::onclick("toggleDimension2",
                     shinyjs::toggle(id = "dimension2", anim = TRUE))
    shinyjs::onclick("toggleDimension3",
                     shinyjs::toggle(id = "dimension3", anim = TRUE))
    shinyjs::onclick("toggleDimension4",
                     shinyjs::toggle(id = "dimension4", anim = TRUE))
    shinyjs::onclick("toggleDimension5",
                     shinyjs::toggle(id = "dimension5", anim = TRUE))
    
    df_country_table <- reactive({
      

      
      
      #recalculate index based on the weights
      pillar_total <- input$pillar_1 + input$pillar_2 + input$pillar_3 + input$pillar_4 + input$pillar_5
      pillar2_total <- input$dim_2_1 + input$dim_2_2 + input$dim_2_4
      pillar3_total <- 
        input$dim_3_1 + 
        input$dim_3_2 +
        input$dim_3_3 +
        input$dim_3_4 +
        input$dim_3_5 +
        input$dim_3_6 +
        input$dim_3_7 +
        input$dim_3_8 +
        input$dim_3_9 +
        input$dim_3_10 +
        input$dim_3_11 +
        input$dim_3_12 +
        input$dim_3_13 +
        input$dim_3_15 +
        input$dim_3_16 +
        input$dim_3_17
      pillar4_total <- input$dim_4_1.CEN + input$dim_4_1.SVY + input$dim_4_2 + input$dim_4_3
      pillar5_total <- input$dim_5_1 + input$dim_5_2 + input$dim_5_5
      
      #create index dataset
      index_tab <- SPI %>%
        filter(date==input$country_year_choice) %>%
        mutate(SPI.DIM1.5.INDEX=rowMeans(across(starts_with("SPI.D1.5")), na.rm=FALSE),
               SPI.DIM2.1.INDEX=rowMeans(across(starts_with('SPI.D2.1'))),
               SPI.DIM2.2.INDEX=SPI.D2.2.Openness.subscore,
               SPI.DIM2.4.INDEX=SPI.D2.4.NADA,
               SPI.DIM3.1.INDEX=rowMeans(across(c("SPI.D3.1.POV",
                                                  "SPI.D3.2.HNGR",
                                                  "SPI.D3.3.HLTH",
                                                  "SPI.D3.4.EDUC",
                                                  "SPI.D3.5.GEND",
                                                  "SPI.D3.6.WTRS"))),
               SPI.DIM3.2.INDEX=rowMeans(across(c("SPI.D3.7.ENRG",
                                                  "SPI.D3.8.WORK",
                                                  "SPI.D3.9.INDY",
                                                  "SPI.D3.10.NEQL",
                                                  "SPI.D3.11.CITY",
                                                  "SPI.D3.12.CNSP"))),         
               SPI.DIM3.3.INDEX=rowMeans(across(c("SPI.D3.13.CLMT",
                                                  "SPI.D3.15.LAND" ))),
               SPI.DIM3.4.INDEX=rowMeans(across(c("SPI.D3.16.INST",
                                                  "SPI.D3.17.PTNS" ))),
               SPI.DIM4.1.CEN.INDEX=rowMeans(across(c('SPI.D4.1.1.POPU','SPI.D4.1.2.AGRI','SPI.D4.1.3.BIZZ'))), #separate census and surveys 
               SPI.DIM4.1.SVY.INDEX=rowMeans(across(c('SPI.D4.1.4.HOUS','SPI.D4.1.5.AGSVY','SPI.D4.1.6.LABR', 'SPI.D4.1.7.HLTH','SPI.D4.1.8.BZSVY'))), #separate census and surveys 
               SPI.DIM4.2.INDEX=rowMeans(across(starts_with('SPI.D4.2'))),
               SPI.DIM4.3.INDEX=rowMeans(across(starts_with('SPI.D4.3'))),
               
               SPI.DIM5.1.INDEX=rowMeans(across(starts_with('SPI.D5.1'))),
               SPI.DIM5.1.INDEX=if_else(is.na(SPI.DIM5.1.INDEX),-99999,SPI.DIM5.1.INDEX),
               
               SPI.DIM5.2.INDEX=rowMeans(across(starts_with('SPI.D5.2'))),
               
               SPI.DIM5.5.INDEX=rowMeans(across(starts_with('SPI.D5.5'))),
               SPI.DIM5.5.INDEX=if_else(is.na(SPI.DIM5.5.INDEX),-99999,SPI.DIM5.5.INDEX) 
        ) %>%
        mutate(
          SPI.INDEX.PIL1=SPI.DIM1.5.INDEX,
          SPI.INDEX.PIL2=(
            (input$dim_2_1/pillar2_total)*SPI.DIM2.1.INDEX +
              (input$dim_2_2/pillar2_total)*SPI.DIM2.2.INDEX +
              (input$dim_2_4/pillar2_total)*SPI.DIM2.4.INDEX )
          ,
          SPI.INDEX.PIL3=(
            (input$dim_3_1/pillar3_total)*SPI.D3.1.POV +
              (input$dim_3_2/pillar3_total)*SPI.D3.2.HNGR +
              (input$dim_3_3/pillar3_total)*SPI.D3.3.HLTH +
              (input$dim_3_4/pillar3_total)*SPI.D3.4.EDUC +
              (input$dim_3_5/pillar3_total)*SPI.D3.5.GEND +
              (input$dim_3_6/pillar3_total)*SPI.D3.6.WTRS +
              (input$dim_3_7/pillar3_total)*SPI.D3.7.ENRG +
              (input$dim_3_8/pillar3_total)*SPI.D3.8.WORK +
              (input$dim_3_9/pillar3_total)*SPI.D3.9.INDY +
              (input$dim_3_10/pillar3_total)*SPI.D3.10.NEQL +
              (input$dim_3_11/pillar3_total)*SPI.D3.11.CITY +
              (input$dim_3_12/pillar3_total)*SPI.D3.12.CNSP +
              (input$dim_3_13/pillar3_total)*SPI.D3.13.CLMT +
              (input$dim_3_15/pillar3_total)*SPI.D3.15.LAND +
              (input$dim_3_16/pillar3_total)*SPI.D3.16.INST +
              (input$dim_3_17/pillar3_total)*SPI.D3.17.PTNS)
          ,
          SPI.INDEX.PIL4=(
            (input$dim_4_1.CEN/pillar4_total)*SPI.DIM4.1.CEN.INDEX +
              (input$dim_4_1.SVY/pillar4_total)*SPI.DIM4.1.SVY.INDEX +
              (input$dim_4_2/pillar4_total)*SPI.DIM4.2.INDEX +
              (input$dim_4_3/pillar4_total)*SPI.DIM4.3.INDEX )
          ,
          SPI.INDEX.PIL5=(
            (input$dim_5_1/pillar5_total)*SPI.DIM5.1.INDEX +
              (input$dim_5_2/pillar5_total)*SPI.DIM5.2.INDEX +
              (input$dim_5_5/pillar5_total)*SPI.DIM5.5.INDEX ),
          SPI.INDEX=(input$pillar_1/pillar_total)*SPI.INDEX.PIL1 +
            (input$pillar_2/pillar_total)*SPI.INDEX.PIL2 +
            (input$pillar_3/pillar_total)*SPI.INDEX.PIL3 +
            (input$pillar_4/pillar_total)*SPI.INDEX.PIL4 +
            (input$pillar_5/pillar_total)*SPI.INDEX.PIL5 
          #sum up based on individual dimension weights
        ) %>% #
        mutate(across(starts_with('SPI.INDEX'),~100*.)) %>%
        select(country, SPI.INDEX,SPI.INDEX.PIL1,SPI.INDEX.PIL2,SPI.INDEX.PIL3,SPI.INDEX.PIL4,SPI.INDEX.PIL5, region, income) %>%
        arrange(-SPI.INDEX)
      #  filter(date>=2016) #2016 is first year with complete data
      
      

      
      
    })
    
    
    
    output$country_table <- DT::renderDataTable({
      
      if (input$toggle_changes==0) {
      
        index_tab <- df_country_table()
        
        #colors
        col_palette <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6")
        
        col_palette2 <- c("#FFBE0B",  "#E7BB25",  "#1A9850")
        
        #calculate the breaks for the color coding
        brks <- quantile(index_tab$SPI.INDEX, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties

        
        brks1 <- quantile(index_tab$SPI.INDEX.PIL1, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties

        
        brks2 <- quantile(index_tab$SPI.INDEX.PIL2, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties

        
        brks3 <- quantile(index_tab$SPI.INDEX.PIL3, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties

        brks4 <- quantile(index_tab$SPI.INDEX.PIL4, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties
 
        
        brks5 <- quantile(index_tab$SPI.INDEX.PIL5, probs=c(1,2,3,4)/5,na.rm=T)-.001 #add tiny adjustment of 0.001 to break ties

  
        # select countries for table
        
        if (!("All" %in% input$country_tab) ) {
          datatab <- index_tab %>%
          filter((country %in% input$country_tab)) 
        }  else {
          datatab <- index_tab 
        }
        
        #select regions for table
        if (!("All" %in% input$region_tab)) {
          datatab <- datatab %>%
          filter((region %in% input$region_tab)) 
        }  else {
          datatab <- datatab
        }
  
        #select income groups for table
        if (!("All" %in% input$income_tab)) {
          datatab <- datatab %>%
            filter((income %in% input$income_tab)) %>%
            select(-income, -region)
        }  else {
          datatab <- datatab %>%
            select(-income, -region)
        }        
  
        
          #make nice looking table
        DT::datatable(datatab, caption=paste('Overall SPI Index in ',input$country_year_choice,' and Pillar Scores.', sep=""),
                        rownames=FALSE,
                        colnames = c("Country", "SPI Overall Score", "Pillar 1: Data Use", "Pillar 2: Data Services","Pillar 3: Data Products ","Pillar 4: Data Sources","Pillar 5: Data Infrastructure" ),
                        container=sketch,
                        class='cell-border stripe',
                        escape = FALSE,
                        extensions = c ('Buttons', 'FixedHeader'), 
                        options=list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel'),
                          pageLength = 60,
                          scrollX = TRUE, 
                          paging=FALSE)) %>%
            formatRound(columns=c('SPI.INDEX','SPI.INDEX.PIL1','SPI.INDEX.PIL2','SPI.INDEX.PIL3','SPI.INDEX.PIL4','SPI.INDEX.PIL5'), digits=1) %>%
            formatStyle(    'SPI.INDEX', backgroundColor = styleInterval(brks, col_palette)) %>%
            formatStyle(    'SPI.INDEX.PIL1', backgroundColor = styleInterval(brks1, col_palette)) %>%
            formatStyle(    'SPI.INDEX.PIL2', backgroundColor = styleInterval(brks2, col_palette)) %>%
            formatStyle(    'SPI.INDEX.PIL3', backgroundColor = styleInterval(brks3, col_palette)) %>%
            formatStyle(    'SPI.INDEX.PIL4', backgroundColor = styleInterval(brks4, col_palette)) %>%
            formatStyle(    'SPI.INDEX.PIL5', backgroundColor = styleInterval(brks5, col_palette))
      
      }
        
    })
    
    output$country_changes <- DT::renderDataTable({  
      
      if (input$toggle_changes==1) {
        
      
        #get country file
        index_tab <- df_country_table() %>%
          rename(
            SPI.INDEX.NEW=SPI.INDEX,
            SPI.INDEX.PIL1.NEW=SPI.INDEX.PIL1,
            SPI.INDEX.PIL2.NEW=SPI.INDEX.PIL2,
            SPI.INDEX.PIL3.NEW=SPI.INDEX.PIL3,
            SPI.INDEX.PIL4.NEW=SPI.INDEX.PIL4,
            SPI.INDEX.PIL5.NEW=SPI.INDEX.PIL5
          )
        
        #get original 
        index_orig <- SPI %>%
          filter(date==input$country_year_choice) %>%
          select(country, SPI.INDEX,SPI.INDEX.PIL1,SPI.INDEX.PIL2,SPI.INDEX.PIL3,SPI.INDEX.PIL4,SPI.INDEX.PIL5) 
        
        #merge 
        index_tab <- index_tab %>%
          left_join(index_orig) %>%
          mutate(SPI.INDEX=SPI.INDEX.NEW-SPI.INDEX,
                 SPI.INDEX.PIL1=SPI.INDEX.PIL1.NEW-SPI.INDEX.PIL1,
                 SPI.INDEX.PIL2=SPI.INDEX.PIL2.NEW-SPI.INDEX.PIL2,
                 SPI.INDEX.PIL3=SPI.INDEX.PIL3.NEW-SPI.INDEX.PIL3,
                 SPI.INDEX.PIL4=SPI.INDEX.PIL4.NEW-SPI.INDEX.PIL4,
                 SPI.INDEX.PIL5=SPI.INDEX.PIL5.NEW-SPI.INDEX.PIL5) %>%
          select(country, SPI.INDEX,SPI.INDEX.PIL1,SPI.INDEX.PIL2,SPI.INDEX.PIL3,SPI.INDEX.PIL4,SPI.INDEX.PIL5, region, income)
        
  
        
        # select countries for table
        
        if (!("All" %in% input$country_tab) ) {
          datatab <- index_tab %>%
            filter((country %in% input$country_tab)) 
        }  else {
          datatab <- index_tab 
        }
        
        #select regions for table
        if (!("All" %in% input$region_tab)) {
          datatab <- datatab %>%
            filter((region %in% input$region_tab)) 
        }  else {
          datatab <- datatab
        }
        
        #select income groups for table
        if (!("All" %in% input$income_tab)) {
          datatab <- datatab %>%
            filter((income %in% input$income_tab)) %>%
            select(-income, -region)
        }  else {
          datatab <- datatab %>%
            select(-income, -region)
        }        
        
        
        eps <- 1E-5
        #make nice looking table
        DT::datatable(datatab, caption=paste('Changes in Overall SPI Index in ',input$country_year_choice,' and Pillar Scores.', sep=""),
                      rownames=FALSE,
                      colnames = c("Country", "SPI Overall Score", "Pillar 1: Data Use", "Pillar 2: Data Services","Pillar 3: Data Products ","Pillar 4: Data Sources","Pillar 5: Data Infrastructure" ),
                      container=sketch,
                      class='cell-border stripe',
                      escape = FALSE,
                      extensions = c ('Buttons', 'FixedHeader'), 
                      options=list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel'),
                        pageLength = 60,
                        scrollX = TRUE, 
                        paging=FALSE)) %>%
          formatRound(columns=c('SPI.INDEX','SPI.INDEX.PIL1','SPI.INDEX.PIL2','SPI.INDEX.PIL3','SPI.INDEX.PIL4','SPI.INDEX.PIL5'), digits=1) %>%
          formatStyle(
            columns = c('SPI.INDEX','SPI.INDEX.PIL1','SPI.INDEX.PIL2','SPI.INDEX.PIL3','SPI.INDEX.PIL4','SPI.INDEX.PIL5'), 
            color = styleInterval(cuts = c(-eps, eps), values = c("red", "black", "green")),
            fontWeight = "bold"
          )
        # formatStyle(    'SPI.INDEX', backgroundColor = styleInterval(brks, col_palette)) %>%
        # formatStyle(    'SPI.INDEX.PIL1', backgroundColor = styleInterval(brks1, col_palette)) %>%
        # formatStyle(    'SPI.INDEX.PIL2', backgroundColor = styleInterval(brks2, col_palette)) %>%
        # formatStyle(    'SPI.INDEX.PIL3', backgroundColor = styleInterval(brks3, col_palette)) %>%
        # formatStyle(    'SPI.INDEX.PIL4', backgroundColor = styleInterval(brks4, col_palette)) %>%
        # formatStyle(    'SPI.INDEX.PIL5', backgroundColor = styleInterval(brks5, col_palette))
      }
      
      

      
    })

    
    
}

# Run the application 

shinyApp(ui = ui, server = server)


