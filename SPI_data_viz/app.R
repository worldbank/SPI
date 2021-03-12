# Written by Brian Stacy on Aug 14, 2020
# A data visualization of the Statistical Performance Indicators
# The viz will contain an overview page showing a map with the indicator values and aggregate statistics
# The viz will also contain country report page that shows details on the indicators by country


library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(wbstats)
library(shinythemes)
library(httr)
library(jsonlite)
library(leaflet)
library(shinycssloaders)
library(rgdal)
library(geojsonio)
library(DT)
library(skimr)
library(ggcorrplot)
library(Hmisc)
library(flextable)
library(skimr)

#read in data and metatdata
SPI <- read_csv('SPI_index.csv')

metadata_raw <- read_csv('SPI_dimensions_sources.csv')

metadata <- metadata_raw %>%
    mutate(descript=paste(SPI_indicator_id,": ", spi_indicator_name," - ",source_name , sep="")) %>%
    select(source_id,  descript, spi_indicator_description,spi_indicator_scoring)

metadata <- read_csv('SPI_index_sources.csv') %>%
  bind_rows(metadata)


#create named list, which can be useful later
var.labels <- metadata$descript
names(var.labels) <- metadata$source_id

#read in TopoJSON from World Bank
countries <- geojsonio::geojson_read("WB_countries_Admin0_lowres.geojson",
                                     what = "sp")

#country info
country_info <- wb_countries() %>%
    mutate(income=income_level,
           lending=lending_type) %>%
    filter(region!="Aggregates") %>%
    select(iso3c, country, region, income, lending)


# Define UI for application that draws a histogram
ui <- navbarPage("Statistical Performance Indicators",

                 #####################################################
                 # Welcome Section
                 ####################################################
                 tabPanel("Welcome",
                          fluidPage(theme = shinytheme("cerulean"),
                                    includeMarkdown("header.md"),
                                    h3('Indicator Metadata'),
                                    withSpinner(DT::dataTableOutput("metadata"))
                                    #h3('Indicator Metadata'),
                                    #withSpinner(DT::dataTableOutput("metadata_all"))
                          )
                 ),
                 #####################################################
                 # Global Map and Statistics Section
                 ####################################################
                 tabPanel("Global Picture",
                          div(class="outer",
                              
                              selectizeInput("color_choices_overall", "Choose Indicator", 
                                             choices=metadata$descript,
                                             selected='SPI.INDEX') ,
                              
                              
                              selectizeInput("year_overall",
                                             "Reference Year",
                                             choices=c(2004:2019),
                                             selected=2019
                                             
                              ),
                              downloadButton("downloadDataMap", "Download")
                              
                              
                          ),
                              
                              fluidRow(style='padding:50px',

                                  h2("Map of Statistical Performance Indicators"),
                                  # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                  withSpinner(leafletOutput("spi_map_overall", width='auto', height='80vh')
                                                            )

                                  

                                    

                              
                          
                          )
       
                        ),
                 
                 #####################################################
                 # Time Trends
                 #####################################################
                 
                 tabPanel("Time Trends",
                          div(class="outer",style='padding:50px',
                              h2("Time Trends of Statistical Performance Indicators"),
                              selectizeInput("over_time",
                                             "Choose Indicator to Plot Over Time",
                                             choices=as.character(var.labels),
                                             selected='SPI Overall Score'
                              ),
                              downloadButton("downloadDataTrends", "Download"),
                              
                              withSpinner(plotlyOutput('plot_time', width='auto', height='40vh')),
                              h2("Change in Indicator Across Countries from 2016-2019"),
                              withSpinner(plotlyOutput('country_plot_time', width='auto', height='40vh')),
                              
                            )
                          ),
                 
                 #####################################################
                 # Pillar Scores
                 #####################################################
                 
                 tabPanel("Scores by Pillar",
                          div(class="outer",style='padding:50px',
                              
                              h2('Overall Scores by Pillar'),
                              selectizeInput("dim_year",
                                             "Reference Year",
                                             choices=c(2016:2019),
                                             selected=2019
                                             
                              ),
                              selectizeInput("country", "Select Countries",
                                             choices=NULL,
                                             selected=c("All"),
                                             multiple=T),  
                              downloadButton("downloadDataDim", "Download"),
                              withSpinner(plotlyOutput('plot_dim', width='auto', height='90vh'))
                          )
                          
                 ),
                 #####################################################
                 # Summary Statistics
                 #####################################################
                 
                 tabPanel("Summary Statistics",
                          div(class="outer",style='padding:50px',
                              
                              h2('Summary Statistics of Indicators'),
                              withSpinner(DT::dataTableOutput("summary_stats_overall"))
                          )
                          
                 ),
                 #####################################################
                 # Summary Statistics by Percentile
                 #####################################################
                 
                 
                 tabPanel("Maturity",
                          div(class="outer",style='padding:50px',
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput("pctl", "Deciles of SPI Overall Score", 
                                              min=1, 
                                              max=10, 
                                              step=1, 
                                              value=1, 
                                              sep="", 
                                              width='100%',
                                              animate=
                                                animationOptions(interval = 2000, loop = FALSE) ),
                                  
                                  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }"))
                                ),
                                mainPanel(
                                    h2('Compare Means by Level of Maturity'),
                                    p('Below, the user can examine how the mean of our statistical performance indicators change by the decile of the SPI overall score.  
                                      Move the slider to change the decile threshold used to calculate the means.  Only countries in a given decile are used
                                      to calculate the mean.  This is meant to show how the underlying indicators compare at different levels of maturity of the statistical system.'),

                                    plotlyOutput('maturityplot')
                                    
                                  )
                              )
                          )
                          
                 ),
                           
                 #####################################################
                 # Country Reports section
                 ####################################################                 
                 tabPanel("Country Reports",
                          div(class="outer",
                          
                              h3('Country Selection'),
                              selectizeInput("country_choice",
                                             "Choose Country",
                                             choices=as.character(country_info$country),
                                             selected='Afghanistan'),
                              selectizeInput("year_choice",
                                             "Choose Year",
                                             choices=c(2004:2019),
                                             selected=2019),  
                              selectizeInput("comparison_choice",
                                             "Add Countries to Compare",
                                             choices=as.character(country_info$country),
                                             multiple=T                              ),
                                             
                              ),
                          h3('Pillar 1: Data Use'),
                          withSpinner(plotlyOutput('plot_dim1',
                                                  width = '80%',
                                                  height='600px')),
                          h3('Pillar 2: Data Services'),
                          withSpinner(plotlyOutput('plot_dim2',
                                                   width = '80%',
                                                 height='350px')),
                          h3('Pillar 3: Data Products'),
                          withSpinner(plotlyOutput('plot_dim3',
                                                   width = '100%',
                                                   height='1300px')),
                          h3('Pillar 4: Data Sources'),
                          withSpinner(plotlyOutput('plot_dim4',
                                                  width = '80%',
                                                 height='1000px')),
                          h3('Pillar 5: Data Infrastructure'),
                          withSpinner(plotlyOutput('plot_dim5',
                                                   width = '80%',
                                                 height='900px'
                                                 ))
                              ),
                 #####################################################
                 # Country Table section
                 ####################################################                 
                 tabPanel("Alternative weighting",
                          div(class="outer",  
                             sidebarLayout( 
                              
                              mainPanel(
                                includeMarkdown("weights.md"),
                                selectizeInput("country_year_choice",
                                             "Choose Year",
                                             choices=c(2016:2019),
                                             selected=2019),
                                selectizeInput("country_tab", "Select Countries",
                                               choices=NULL,
                                               selected=c("All"),
                                               multiple=T),  
                                withSpinner(DT::dataTableOutput("country_table",
                                                              width='100%'))
                              ),
                              sidebarPanel(id = "weights",
                                            
                                            h2("Customize Weights for Each Pillar (Weights will automatically sum to 1)"),
                                            h3("Pillar 1: Data Use"),
                                            sliderInput("pillar_1", "Pillar 1 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Dimension Weights"),
                                            
                                            sliderInput("dim_1_5", "Dimension 1.5: Data Use by International Organizations",
                                                        min = 0, max = 1, value = 1
                                            ),
                                            h3("Pillar 2: Data Services"),
                                            sliderInput("pillar_2", "Pillar 2 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Dimension Weights"),
                                            
                                            sliderInput("dim_2_1", "Dimension 2.1: Data releases",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            sliderInput("dim_2_2", "Dimension 2.2: Online access",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            sliderInput("dim_2_4", "Dimension 2.4: Data access services",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            h3("Pillar 3: Data Products"),
                                            sliderInput("pillar_3", "Pillar 3 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Dimension Weights"),
                                            
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
                                            ),
                                            h3("Pillar 4: Data Sources"),
                                            sliderInput("pillar_4", "Pillar 4 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Dimension Weights"),
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
                                            ),
                                            h3("Pillar 5: Data Infrastructure"), 
                                            sliderInput("pillar_5", "Pillar 5 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Dimension Weights"),
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
    
    ############
    # Plot Over Time
    ############
    
    time_var <- reactive({
        
        names(var.labels[match(input$over_time, var.labels)])
        
    })
    
    df_time <- reactive({
        
        #produce by region
        time_df<- SPI %>%
            select(iso3c, country, income, date, population, time_var() ) %>%
            rename(y=time_var()) %>%
            left_join(country_info) %>%
            group_by(date, region) %>%
            summarise(y=100*mean(y, na.rm=T))  %>%
            ungroup()  
        
        
        #produce global number
        time_df_gl<- SPI %>%
            select(iso3c, country, date, population, time_var() ) %>%
            rename(y=time_var()) %>%
            mutate(region='Global') %>%
            group_by(date, region) %>%
            summarise(y=100*mean(y, na.rm=T)) %>%
            ungroup()
        
        #join
        time_df <- time_df %>%
            bind_rows(time_df_gl) 
        
        if (time_var() %in% c('SPI.INDEX', 'SPI.INDEX.PIL1', 'SPI.INDEX.PIL2', 'SPI.INDEX.PIL3', 'SPI.INDEX.PIL4', 'SPI.INDEX.PIL5')) {
            #fix an issue where main variables need to be rescaled
            time_df <- time_df %>%
                mutate(y=y/100)
        }
        
        time_df 
        
        
    })
    
    output$plot_time <- renderPlotly({
        
        plot_ly(data = df_time(), x = ~date, y = ~y, 
                color=~region, 
                labels=time_var(),
                type = 'scatter', mode = 'lines+markers',
                width=1000) %>%
            layout(title = paste('Plot of SPI Indicators Over Time by Region: ',input$over_time, sep=""),
                   xaxis = list(autotick = F, dtick = 1),
                   yaxis = list(range = c(0,100)),
                   legend=list(orientation='h'))
        
        
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadDataTrends <- downloadHandler(
      filename = function() {
        'SPI_Time_Trends.csv'
      },
      content = function(file) {
        write.csv(df_time(), file, row.names = FALSE)
      }
    )
    
    ############
    # Country Plot Over Time
    ############
    df_country_time <- reactive({
      
      changes_df <-  SPI %>%
        select(iso3c, country, region,  income, date, time_var(), population) %>%
        filter(date==2016 | date==2019) 
      
      
      # Rearrange for Plot


      #set up data
      changes_df_temp <- changes_df %>%
        select(country,iso3c, date, starts_with('SPI')) %>%
        pivot_longer( #create dataset that is countryXdateXindicator
          cols = starts_with('SPI'),
          names_to = 'source_id',
          values_to = 'values'
        ) %>% 
        unique() %>%
        filter(!is.na(values)) %>%
        pivot_wider( #reshape to be countryxindicator with columns with data
          id_cols=c('country','iso3c','source_id'),
          names_from='date',
          names_prefix='value_',
          values_from='values'
        ) %>%
        mutate(change=value_2019-value_2016,
               color=if_else(change>=0,'improved', 'decreased'))  %>%
        left_join(metadata) %>%
        filter(!is.na(descript)) %>%
        ungroup() 
      
      changes_df_temp %>%
        arrange(-value_2019) %>% 
        mutate(Country=factor(country, levels=unique(changes_df_temp$country))) 
      
      
      
      
    })
    
    output$country_plot_time <- renderPlotly({
      
      
      p <- ggplot(df_country_time(), aes(x=Country)) +
        geom_point(aes(y=value_2016), shape=16) +
        geom_point(aes(y=value_2019), color='blue', shape=17) +
        geom_segment( aes(x=Country ,xend=Country, y=value_2016, yend=value_2019,
                          color=color)) +
        scale_color_manual(values = c('decreased' = "red", 'improved' = "green")) +
        scale_y_continuous(name='SPI Value') +
        coord_flip() +
        theme_bw() +
        labs(
          color = 'Change'
        ) +
        ylab('SPI Value')
      
      l <- list(
        font = list(
          x=0.7,
          y=0.9,
          family = "sans-serif",
          size = 12,
          color = "#000"),
        bgcolor = "#E2E2E2",
        bordercolor = "#FFFFFF",
        borderwidth = 2)
      
      ggplotly(p, height=2000, width=1000, tooltip=c('x','y')) %>%
        layout(legend = l)
      
      
    })

    ############
    # Plot by Pillar
    ############
    
    #add country choices
    choice <- unique(as.character(country_info$country))
    choice<-append('All',choice)
    
    updateSelectizeInput(session, 'country', choices = choice, selected=c("All"), server=TRUE)
    
    hgt <- reactive({
      20*(100*length(input$choice)/(length(unique(country_info$country)+1)))
      print(2000)
      
    })
    
    df_dim <- reactive({
      
      #produce by region
      time_df<- SPI %>%
        select(iso3c, country, income, date, population, starts_with('SPI.INDEX') ) %>%
        filter(date==input$dim_year) 
      
      if (!("All" %in% input$country))
        time_df %>%
        filter(country %in% input$country) 
      else (
        time_df
      )
      
      
    })
    
    output$plot_dim <- renderPlotly({
      
      ##################### Zipper ###############################
      

      
      # create dataframe by income
      df_aggregated <- df_dim() %>%
        ungroup() %>%
        filter(!is.na(SPI.INDEX)) %>%
        mutate(rank=max(SPI.INDEX)) %>%
        arrange(SPI.INDEX)  %>%
        ungroup() %>%
        mutate(rank = rank(rank, ties.method = 'first')) %>%
        arrange(rank) %>%
        mutate(Country=factor(rank,  levels=rank, labels=country)) %>%
        mutate(across(starts_with("SPI.INDEX"), round,1))
      
      
      #plot by Pillar
      colors <- c("SPI Overall Score" = "#390099", "SPI Pillar 1 - Data Use" = "#9b5de5", "SPI Pillar 2 - Data Services" = "#f15bb5", "SPI Pillar 3 - Data Products" = "#fee440", "SPI Pillar 4 - Data Sources" = "#00bbf9", "SPI Pillar 5 - Data Infrastructure" = "#00f5d4" )
      
      zipper_plt<-ggplot(df_aggregated, aes(x=Country, y=SPI.INDEX)) +
        geom_segment( aes(x=Country, xend=Country, y=0, yend=100), color="grey", size=1, alpha=0.1) +
        geom_point(aes(color="SPI Overall Score"),size=2,) +
        geom_point( aes(y=SPI.INDEX.PIL1, color="SPI Pillar 1 - Data Use"),size=2,) +
        geom_point( aes(y=SPI.INDEX.PIL2, color="SPI Pillar 2 - Data Services"), size=2,) +
        geom_point( aes(y=SPI.INDEX.PIL3, color="SPI Pillar 3 - Data Products"), size=2,) +
        geom_point( aes(y=SPI.INDEX.PIL4, color="SPI Pillar 4 - Data Sources"), size=2,) +
        geom_point( aes(y=SPI.INDEX.PIL5, color="SPI Pillar 5 - Data Infrastructure"), size=2,) +
        #geom_text(aes(label=food_insecure_mod_severe), nudge_y = 3) +
        coord_flip()+
        theme_bw() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        expand_limits(y=c(0,100)) +
        labs(
          x="",
          y="Statistical Performance Indicator",
          color="Legend"
        ) +
        ggtitle(str_wrap("Statistical Performance Indicator", 120)) + 
        # scale_x_continuous(
        #   breaks = df_aggregated$rank, # specify tick breaks using rank column
        #   labels = df_aggregated$country # specify tick labels using x column
        # ) +
        scale_color_manual(values = colors,
                           breaks=c("SPI Overall Score", "SPI Pillar 1 - Data Use", "SPI Pillar 2 - Data Services", "SPI Pillar 3 - Data Products", "SPI Pillar 4 - Data Sources", "SPI Pillar 5 - Data Infrastructure"),
                           labels=c("SPI Overall Score", "SPI Pillar 1 - Data Use", "SPI Pillar 2 - Data Services", "SPI Pillar 3 - Data Products", "SPI Pillar 4 - Data Sources", "SPI Pillar 5 - Data Infrastructure"))
      zipper_plt
      

      
      ggplotly(zipper_plt, height=if_else(('All' %in% input$country),as.numeric(2000), 100*as.numeric(length(input$country))))
      
      
    })
    
    output$downloadDataDim <- downloadHandler(
      filename = function() {
        'SPI_Dimensions_Data.csv'
      },
      content = function(file) {
        write.csv(df_dim(), file, row.names = FALSE)
      }
    )
    
    
    
    
    #####################################
    ########## Maturity
    #####################################
    
    mat_df<-SPI  %>%
      filter(date==2019) %>%
      select(one_of(metadata$source_id))
    
    
    
    spi_groups_quantiles <- quantile(mat_df$SPI.INDEX, probs=c(1:10)/10,na.rm=T)
    
    mat_df <- mat_df %>%
      mutate(decile=case_when(
        between(SPI.INDEX, spi_groups_quantiles[9],100) ~ "Top 10%",
        between(SPI.INDEX, spi_groups_quantiles[8],spi_groups_quantiles[9]) ~ "9th Decile",
        between(SPI.INDEX, spi_groups_quantiles[7],spi_groups_quantiles[8]) ~ "8th Decile",
        between(SPI.INDEX, spi_groups_quantiles[6],spi_groups_quantiles[7]) ~ "7th Decile",
        between(SPI.INDEX, spi_groups_quantiles[5],spi_groups_quantiles[6]) ~ "6th Decile",
        between(SPI.INDEX, spi_groups_quantiles[4],spi_groups_quantiles[5]) ~ "5th Decile",
        between(SPI.INDEX, spi_groups_quantiles[3],spi_groups_quantiles[4]) ~ "4th Decile",
        between(SPI.INDEX, spi_groups_quantiles[2],spi_groups_quantiles[3]) ~ "3rd Decile",
        between(SPI.INDEX, spi_groups_quantiles[1],spi_groups_quantiles[2]) ~ "2nd Decile",
        between(SPI.INDEX, 0,spi_groups_quantiles[1]) ~ "Bottom 10%"
        
      )) %>%
      mutate(decile=factor(decile, 
                               levels=c("Bottom 10%","2nd Decile","3rd Decile","4th Decile", "5th Decile","6th Decile","7th Decile","8th Decile","9th Decile","Top 10%" )))  
    
    
    mat_sumstats <- mat_df %>%
      group_by(decile) %>%
      filter(!decile=="NA")
    
    
    
    my_skim<-    skim_with( numeric = sfl( mean = ~ wtd.mean(.,  w=ipw, na.rm=TRUE),
                                           sd = ~ sqrt(wtd.var(.,  weights=ipw, na.rm=TRUE)),
                                           p25 = ~ (wtd.quantile(., probs=c(0.25),  weights=ipw, na.rm=TRUE)),
                                           p50 = ~ (wtd.quantile(., probs=c(0.5), weights=ipw, na.rm=TRUE)),
                                           p75 = ~ (wtd.quantile(., probs=c(0.75), weights=ipw, na.rm=TRUE)),
                                           complete = ~ sum(!is.na(.))))
    
    mat_sumstats$ipw <- 1
    
    mat_sumstats_df<-my_skim(mat_sumstats) %>%
      yank("numeric") %>%
      mutate(source_id=skim_variable) %>%
      filter(grepl('SPI.INDEX', source_id)) %>%
      left_join(metadata) %>%
      select(decile, descript, mean, p50, complete,  hist) %>%
      mutate(descript=str_wrap(descript, 30),
             mean=round(mean,2))
    
    ##########################
    #Produce dataset based on indicator selected
    ##########################
    maturity_df <- reactive({
      
      #need to modify this depending on country
      
      
      
      mat_sumstats_df%>%
        filter(as.numeric(decile)<=as.numeric(input$pctl)) %>%
        filter(!is.na(descript))
      

      
            
    })
    
    
    output$maturityplot <- renderPlotly({
      
     p<-  ggplot(maturity_df(), aes(x=mean, y=descript)) +
        geom_point(aes(alpha=decile),size=4) +
        geom_segment(aes(x=0,xend=mean,y=descript,yend=descript), alpha=0.2) +
        expand_limits(x=c(0,100)) +
        theme_bw() +
        scale_alpha_discrete(name="Decile",
                             range=c(0.4,1)) +
        theme(
          text=element_text(size=16),
          legend.position = 'top',
          legend.title = element_blank()
        )
     
     l <- list(
         y=0.8,
         font = list(
           family = "sans-serif",
           size = 12,
           color = "#000"),
       bgcolor = "#E2E2E2",
       bordercolor = "#FFFFFF",
       borderwidth = 2)
     
     ggplotly(p, height=800) %>%
       layout(legend=l)
     
    })
    # 
    # output$tableset <- DT::renderDataTable({
    #   
    # 
    #   
    #   
    #   DT::datatable(sumstats_df, caption=paste("Summary Statistics of Statistical Performance Indicators for Countries Below", input$pctl, "Percentile in SPI Overall Score", sep=" "),
    #                 colnames=c("Indicator",  "Mean",  "Median",  "# Complete Cases",  "Histogram"),
    #                 extensions = 'Buttons', options=list(
    #                   dom = 'Bfrtip',
    #                   buttons = c('copy', 'csv', 'excel'),
    #                   pageLength = 60)) %>%
    #     formatRound(columns = c('mean',  'p50'),
    #                 digits=2)
    # })
    # 
    
    
    
    
    ###########
    # Now pull data using IDs for WDI and calculate AKI
    ###########
    
    # Availability of Key Indicator Country Score equals Weighted Score divided by Maximum Category Score time 100
    
    df_overall<- reactive({
        SPI %>%
            select(iso3c, country, region,  income, date, starts_with('SPI'), population) %>%
            filter(date==input$year_overall) 

        
        
    })
    
    # updateSelectizeInput(session, 'color_choices_overall', choices = c('SPI.OVRL.SCR', 'SPI.D1.MSC', 'SPI.D2.CS', 'SPI.D3.AKI', 'SPI.D4.DPO'), server = TRUE)
    
    map_var <- reactive({
        
        names(var.labels[match(input$color_choices_overall, var.labels)])
        
    })
    
    output$spi_map_overall <- renderLeaflet({
        
        
        spi_map_overall<-countries
        
        spi_map_overall@data <- spi_map_overall@data %>%
          mutate(iso3c=ISO_A3_EH,
                 iso3c=if_else(ISO_A3_EH==-99,WB_A3,ISO_A3_EH)) %>%
            left_join(df_overall()) 
        
        palette_df <- df_overall() %>%
          select(map_var())

        brks <- quantile(palette_df[,1], probs=c(1,2,3,4)/5,na.rm=T)
        brks <- append(0,brks)
        col_p <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7")
        
        if (max(brks)<100) {
          brks <- append(brks,100)
          col_p <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6")
          
        }        #create pallete
        pal <- colorBin(col_p, 
                          bins=brks,
                          domain=c(0,100),
                          na.color='grey',
                          pretty=FALSE)
        
        
        
        ##############
        #create labels
        ##############
        
        
        labels <- sprintf(
            "<strong>%s</strong><br/> <hr size=2>
            <strong> %s: %g </strong><br/> <hr size=2>
            ",
            spi_map_overall@data$NAME_EN,
            'Indicator Value', round(select(spi_map_overall@data, map_var())[,1], digits = 1)

        ) %>%
            lapply(htmltools::HTML)

        
        if (grepl('SPI.INDEX',map_var())) {
        
        leaflet(spi_map_overall) %>%
            addProviderTiles(providers$Esri.WorldStreetMap) %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                        fillColor = ~pal(select(spi_map_overall@data, map_var())[,1]),
                        label=labels) %>%
            addLegend(pal=pal, 
                      values=~select(spi_map_overall@data, map_var())[,1], opacity=0.7, 
                      labFormat = function(type, cuts, p) {  # Here's the trick
                        paste0( c("Bottom 20%","2nd Quintile","3rd Quintile","4th Quintile","Top 20%" ))
                      },
                      title='Indicator value', position="bottomleft")      
        } else {
          
          pal <- colorNumeric("Blues", palette_df[,1])
          
          
          leaflet(spi_map_overall) %>%
            addProviderTiles(providers$Esri.WorldStreetMap) %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                        fillColor = ~pal(select(spi_map_overall@data, map_var())[,1]),
                        label=labels) %>%
            addLegend(pal=pal, 
                      values=~select(spi_map_overall@data, map_var())[,1], opacity=0.7, 
                                            title='Indicator value', position="bottomleft")      
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
    
    ####
    # SPI AKI Summary Stats
    ###
    
    output$summary_stats_overall <- DT::renderDataTable({
        
        
        #add function to produce weighted summary stats
        
        
        #produce by region
        sumstats<- df_overall() %>%
            left_join(country_info) %>%
            group_by(region) %>%
            filter(!is.na(region)) %>%
            select(region, starts_with('SPI'), population) %>%
            summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) 
        
        #produce global number
        sumstats_gl<- df_overall() %>%
            mutate(region='Global') %>%
            group_by(region) %>%
            select(region, starts_with('SPI'), population) %>%
            summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) 
        
        
        #transpose data
        sumstats_df_long <-sumstats_gl %>%
            bind_rows(sumstats) 
        
        sumstats_df <- as.data.frame(t(sumstats_df_long %>% select(-region)))
        colnames(sumstats_df) = sumstats_df_long$region 
        
        
        sumstats_df <- sumstats_df %>%
            rownames_to_column() %>%
            rename(source_id=rowname)
        
        
        #create labels df
        metadata_tab2_overall <- metadata %>% 
            janitor::clean_names() %>%
            filter(grepl('SPI',source_id)) %>%
            select(source_id, descript)
        
        
        #add variable label
        sumstats_df <- sumstats_df %>%
            left_join(metadata_tab2_overall) %>%
            rename(Series=source_id,
                   Label=descript) %>%
            select(Series, Label, everything())
        
        DT::datatable(sumstats_df, caption=htmltools::tags$caption(
            style = 'caption-side: top; text-align: left;',
            htmltools::em("Unweighted Mean of SPI Indicators by Region.  Sub-indicators scaled to be 0-1."))    ,
            extensions = 'Buttons', options=list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                pageLength = 70)) 
        
        
        
    })
    
   
    
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
    
    country_report_lolli_fn <- function(variables) {
      
      #set up data
      lollip_df_temp <- lolli_df() %>%
        select(country, starts_with(variables)) %>%
        pivot_longer(
          cols = starts_with(variables),
          names_to = 'source_id',
          values_to = 'values'
        ) %>% 
        left_join(metadata_raw) %>%
        filter(!is.na(source_name)) %>%
        mutate(source_name=str_wrap(source_name, 30)) %>%
        mutate(source_name=factor(source_name, levels=unique(source_name)),
               country=factor(country, levels=unique(country))) 
        
      #now use ggplot
        lollip <- ggplot(lollip_df_temp,
               aes(x=source_name, y=values, color=country)) +
        geom_segment( aes(x=source_name ,xend=source_name, y=0, yend=values), color="grey") +
        geom_point(size=3) +
        coord_flip() +
        theme_bw() +
        scale_x_discrete(limits = rev(levels(lollip_df_temp$source_name))) +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y=element_text(size=14),
          legend.text = element_text(size=14),
          legend.title = element_blank(),
          legend.position = 'top'
        ) +
        xlab("") +
        ylab("Indicator")
      
      ggplotly(lollip)
    }
    
    
    output$plot_dim1 <- renderPlotly({
  
      ggplotly(country_report_lolli_fn('SPI.D1')) %>%
        layout(autosize = TRUE, margin = list(l = 300, r = 0, b = 0, t = 0, pad = 4))
      
    })
    
    output$plot_dim2 <- renderPlotly({
      ggplotly(country_report_lolli_fn('SPI.D2')) %>%
        layout(autosize = TRUE, margin = list(l = 300, r = 0, b = 0, t = 0, pad = 4))
    })
    output$plot_dim3 <- renderPlotly({
      ggplotly(country_report_lolli_fn('SPI.D3')) %>% 
        layout(autosize = TRUE, margin = list(l = 300, r = 0, b = 0, t = 0, pad = 4))
    })
    output$plot_dim4 <- renderPlotly({
      ggplotly(country_report_lolli_fn('SPI.D4')) %>%
        layout(autosize = TRUE, margin = list(l = 300, r = 0, b = 0, t = 0, pad = 4))
    })
    output$plot_dim5 <- renderPlotly({
      ggplotly(country_report_lolli_fn('SPI.D5')) %>%
        layout(autosize = TRUE, margin = list(l = 300, r = 0, b = 0, t = 0, pad = 4))
    })    
    
    
    
    ###########
    # Country Datatable
    ###########
    updateSelectizeInput(session, 'country_tab', choices = choice, selected=c("All"), server=TRUE)
    
    
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
        select(country, SPI.INDEX,SPI.INDEX.PIL1,SPI.INDEX.PIL2,SPI.INDEX.PIL3,SPI.INDEX.PIL4,SPI.INDEX.PIL5) 
      #  filter(date>=2016) #2016 is first year with complete data
      
      

      
      
    })
    
    output$country_table <- DT::renderDataTable({
      
      #get country file
      index_tab <- df_country_table()
      
      #colors
      col_palette <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6")
      
      col_palette2 <- c("#FFBE0B",  "#E7BB25",  "#1A9850")
      
      #calculate the breaks for the color coding
      brks <- quantile(index_tab$SPI.INDEX, probs=c(1,2,3,4)/5,na.rm=T)
      
      brks1 <- quantile(index_tab$SPI.INDEX.PIL1, probs=c(1,2,3,4)/5,na.rm=T)
      
      brks2 <- quantile(index_tab$SPI.INDEX.PIL2, probs=c(1,2,3,4)/5,na.rm=T)
      
      brks3 <- quantile(index_tab$SPI.INDEX.PIL3, probs=c(1,2,3,4)/5,na.rm=T)
      
      brks4 <- quantile(index_tab$SPI.INDEX.PIL4, probs=c(1,2,3,4)/5,na.rm=T)
      
      brks5 <- quantile(index_tab$SPI.INDEX.PIL5, probs=c(1,2,3,4)/5,na.rm=T)

      # select countries for table
      
      if (!("All" %in% input$country_tab))
        datatab <- index_tab %>%
        filter(country %in% input$country_tab) 
      else (
        datatab <- index_tab
      )
      
        #make nice looking table
        DT::datatable(datatab, caption=paste('Overall SPI Index in ',input$country_year_choice,' and Pillar Scores.', sep=""),
                      rownames=FALSE,
                      colnames = c("Country", "SPI Index Value", "Pillar 1: Data Use", "Pillar 2: Data Services","Pillar 3: Data Products ","Pillar 4: Data Sources","Pillar 5: Data Infrastructure" ),
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

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


