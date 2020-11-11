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
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              withSpinner(leafletOutput("spi_map_overall"
                                                        , width = "90%", height = "700px")),
                              
                              h3('Summary Statistics of Indicators'),
                              withSpinner(DT::dataTableOutput("summary_stats_overall")),
                              h3('Indicators Over Time'),
                              selectizeInput("over_time",
                                             "Choose Indicator to Plot Over Time",
                                             choices=as.character(metadata$descript)
                                             
                              ),
                              withSpinner(plotlyOutput('plot_time',
                                                       width = '80%')),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls_overall", class = "panel panel-default", fixed = FALSE,
                                            draggable = FALSE, top = 150, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Indicator Year"),
                                            selectizeInput("year_overall",
                                                           "Reference Year",
                                                           choices=c(2004:2019),
                                                           selected=2019
                                                           
                                            ),
                                            selectizeInput("income_groups_overall",
                                                           "Select Income Groups",
                                                           choices=c("Low income","Lower middle income","Upper middle income","High income"),
                                                           selected=c("Low income","Lower middle income","Upper middle income","High income"),
                                                           multiple=T
                                            ),
                                            selectizeInput("color_choices_overall", "Choose Indicator to Color Map", 
                                                           choices=metadata$descript,
                                                           selected='SPI.INDEX') 
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
                                             choices=c(2016:2019),
                                             selected=2019),  
                              selectizeInput("comparison_choice",
                                             "Add Countries to Compare",
                                             choices=as.character(country_info$country),
                                             multiple=T                              ),
                                             
                              ),
                          h3('Dimension 1: Data Use'),
                          withSpinner(plotlyOutput('plot_dim1',
                                                  width = '80%',
                                                  height='500px')),
                          h3('Dimension 2: Data Services'),
                          withSpinner(plotlyOutput('plot_dim2',
                                                   width = '80%',
                                                 height='350px')),
                          h3('Dimension 3: Data Products'),
                          withSpinner(plotlyOutput('plot_dim3',
                                                   width = '100%',
                                                   height='1300px')),
                          h3('Dimension 4: Data Sources'),
                          withSpinner(plotlyOutput('plot_dim4',
                                                  width = '80%',
                                                 height='1000px')),
                          h3('Dimension 5: Data Infrastructure'),
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
                                withSpinner(DT::dataTableOutput("country_table",
                                                              width='70%'))
                              ),
                              sidebarPanel(id = "weights",
                                            
                                            h2("Customize Weights for Each Pillar (Weights will automatically sum to 1)"),
                                            h3("Dimension 1: Data Use"),
                                            sliderInput("dim_1", "Dimension 1 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Pillar Weights"),
                                            
                                            sliderInput("pillar_1_5", "Pillar 1.5: Data Use by International Organizations",
                                                        min = 0, max = 1, value = 1
                                            ),
                                            h3("Dimension 2: Data Services"),
                                            sliderInput("dim_2", "Dimension 2 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Pillar Weights"),
                                            
                                            sliderInput("pillar_2_1", "Pillar 2.1: Data releases",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            sliderInput("pillar_2_2", "Pillar 2.2: Online access",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            sliderInput("pillar_2_4", "Pillar 2.4: Data services",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            h3("Dimension 3: Data Products"),
                                            sliderInput("dim_3", "Dimension 3 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Pillar Weights"),
                                            
                                            sliderInput("pillar_3_1", "Pillar 3: SDG 1",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_2", "Pillar 3: SDG 2",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_3", "Pillar 3: SDG 3",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_4", "Pillar 3: SDG 4",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_5", "Pillar 3: SDG 5",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_6", "Pillar 3: SDG 6",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_7", "Pillar 3: SDG 7",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_8", "Pillar 3: SDG 8",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_9", "Pillar 3: SDG 9",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_10", "Pillar 3: SDG 10",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_11", "Pillar 3: SDG 11",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_12", "Pillar 3: SDG 12",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_13", "Pillar 3: SDG 13",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_15", "Pillar 3: SDG 15",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_16", "Pillar 3: SDG 16",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            sliderInput("pillar_3_17", "Pillar 3: SDG 17",
                                                        min = 0, max = 1, value = (1/16)
                                            ),
                                            h3("Dimension 4: Data Sources"),
                                            sliderInput("dim_4", "Dimension 4 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Pillar Weights"),
                                            sliderInput("pillar_4_1", "Pillar 4.1: censuses and surveys",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            sliderInput("pillar_4_2", "Pillar 4.2: administrative data",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            sliderInput("pillar_4_3", "Pillar 4.3: geospatial data",
                                                        min = 0, max = 1, value = (1/3)
                                            ),
                                            h3("Dimension 5: Data Infrastructure"), 
                                            sliderInput("dim_5", "Dimension 5 - Overall Weight",
                                                        min = 0, max = 1, value = (1/5)
                                            ),
                                            h4("Pillar Weights"),
                                            sliderInput("pillar_5_2", "Pillar 5.2: Standards and Methods",
                                                        min = 0, max = 1, value = 1
                                                        )
                                          
                                            
                                            
                              )
                              
                          )
                          )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    
    
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
            filter(income %in% input$income_groups_overall) %>% #filter out income groups not selected
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
        
        if (time_var() %in% c('SPI.INDEX', 'SPI.INDEX.DIM1', 'SPI.INDEX.DIM2', 'SPI.INDEX.DIM3', 'SPI.INDEX.DIM4', 'SPI.INDEX.DIM5')) {
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
                type = 'scatter', mode = 'lines+markers') %>%
            layout(title = paste('Plot of SPI Indicators Over Time by Region: ',input$over_time, sep=""),
                   xaxis = list(autotick = F, dtick = 1),
                   yaxis = list(range = c(0,100)))
        
        
    })
    
    ###########
    # Now pull data using IDs for WDI and calculate AKI
    ###########
    
    # Availability of Key Indicator Country Score equals Weighted Score divided by Maximum Category Score time 100
    
    df_overall<- reactive({
        SPI %>%
            select(iso3c, country, region,  income, date, starts_with('SPI'), population) %>%
            filter(date==input$year_overall) %>%
            filter(income %in% input$income_groups_overall) #filter out income groups not selected

        
        
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

        brks <- quantile(palette_df[,1], probs=c(0,1,2,3,4,5)/5,na.rm=T)
        #create pallete
        pal <- colorBin(c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6"), 
                          bins=brks,
                          na.color='grey')
        
        
        
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
    })
    
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
    # Functions
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
    
    output$country_table <- DT::renderDataTable({
      

        #colors
        col_palette <- c("#ff9f1c","#ffbf69","#f1dc76","#acece7","#2ec4b6")
        
        col_palette2 <- c("#FFBE0B",  "#E7BB25",  "#1A9850")
        
        #recalculate index based on the weights
        dim_total <- input$dim_1 + input$dim_2 + input$dim_3 + input$dim_4 + input$dim_5
        dim2_total <- input$pillar_2_1 + input$pillar_2_2 + input$pillar_2_4
        dim3_total <- 
          input$pillar_3_1 + 
          input$pillar_3_2 +
          input$pillar_3_3 +
          input$pillar_3_4 +
          input$pillar_3_5 +
          input$pillar_3_6 +
          input$pillar_3_7 +
          input$pillar_3_8 +
          input$pillar_3_9 +
          input$pillar_3_10 +
          input$pillar_3_11 +
          input$pillar_3_12 +
          input$pillar_3_13 +
          input$pillar_3_15 +
          input$pillar_3_16 +
          input$pillar_3_17
        dim4_total <- input$pillar_4_1 + input$pillar_4_2 + input$pillar_4_3
        
        
        index_tab <- SPI %>%
          filter(date==input$country_year_choice) %>%
          mutate(INDEX.SPI.D2.1=rowMeans(across(starts_with('SPI.D2.1'))),
                 INDEX.SPI.D2.2=SPI.D2.2.Openness.subscore,
                 INDEX.SPI.D2.4=SPI.D2.4.NADA,
                 INDEX.SPI.D3.1=rowMeans(across(c("SPI.D3.1.POV",
                                                  "SPI.D3.2.HNGR",
                                                  "SPI.D3.3.HLTH",
                                                  "SPI.D3.4.EDUC",
                                                  "SPI.D3.5.GEND",
                                                  "SPI.D3.6.WTRS"))),
                 INDEX.SPI.D3.2=rowMeans(across(c("SPI.D3.7.ENRG",
                                                  "SPI.D3.8.WORK",
                                                  "SPI.D3.9.INDY",
                                                  "SPI.D3.10.NEQL",
                                                  "SPI.D3.11.CITY",
                                                  "SPI.D3.12.CNSP"))),         
                 INDEX.SPI.D3.3=rowMeans(across(c("SPI.D3.13.CLMT",
                                                  "SPI.D3.15.LAND" ))),
                 INDEX.SPI.D3.4=rowMeans(across(c("SPI.D3.16.INST",
                                                  "SPI.D3.17.PTNS" ))),
                 INDEX.SPI.D4.1=rowMeans(across(starts_with('SPI.D4.1'))),
                 INDEX.SPI.D4.2=rowMeans(across(starts_with('SPI.D4.2'))),
                 INDEX.SPI.D4.3=rowMeans(across(starts_with('SPI.D4.3'))),
                 #INDEX.SPI.D5.1=rowMeans(across(starts_with('SPI.D5.1'))),
                 INDEX.SPI.D5.2=rowMeans(across(starts_with('SPI.D5.2'))),
                 #INDEX.SPI.D5.5=rowMeans(across(starts_with('SPI.D5.5')))
          ) %>%
          mutate(
            SPI.INDEX.DIM1=rowMeans(across(starts_with("SPI.D1.5")), na.rm=FALSE),
            SPI.INDEX.DIM2=(
              (input$pillar_2_1/dim2_total)*INDEX.SPI.D2.1 +
              (input$pillar_2_2/dim2_total)*INDEX.SPI.D2.2 +
              (input$pillar_2_4/dim2_total)*INDEX.SPI.D2.4 )
              ,
            SPI.INDEX.DIM3=(
              (input$pillar_3_1/dim3_total)*SPI.D3.1.POV +
              (input$pillar_3_2/dim3_total)*SPI.D3.2.HNGR +
              (input$pillar_3_3/dim3_total)*SPI.D3.3.HLTH +
              (input$pillar_3_4/dim3_total)*SPI.D3.4.EDUC +
              (input$pillar_3_5/dim3_total)*SPI.D3.5.GEND +
              (input$pillar_3_6/dim3_total)*SPI.D3.6.WTRS +
              (input$pillar_3_7/dim3_total)*SPI.D3.7.ENRG +
              (input$pillar_3_8/dim3_total)*SPI.D3.8.WORK +
              (input$pillar_3_9/dim3_total)*SPI.D3.9.INDY +
              (input$pillar_3_10/dim3_total)*SPI.D3.10.NEQL +
              (input$pillar_3_11/dim3_total)*SPI.D3.11.CITY +
              (input$pillar_3_12/dim3_total)*SPI.D3.12.CNSP +
              (input$pillar_3_13/dim3_total)*SPI.D3.13.CLMT +
              (input$pillar_3_15/dim3_total)*SPI.D3.15.LAND +
              (input$pillar_3_16/dim3_total)*SPI.D3.16.INST +
              (input$pillar_3_17/dim3_total)*SPI.D3.17.PTNS)
            ,
            SPI.INDEX.DIM4=(
              (input$pillar_4_1/dim4_total)*INDEX.SPI.D4.1 +
              (input$pillar_4_2/dim4_total)*INDEX.SPI.D4.2 +
              (input$pillar_4_3/dim4_total)*INDEX.SPI.D4.3 )
            ,
            SPI.INDEX.DIM5=rowMeans(across(starts_with("INDEX.SPI.D5")), na.rm=FALSE),
            SPI.INDEX=(input$dim_1/dim_total)*SPI.INDEX.DIM1 +
                      (input$dim_2/dim_total)*SPI.INDEX.DIM2 +
                      (input$dim_3/dim_total)*SPI.INDEX.DIM3 +
                      (input$dim_4/dim_total)*SPI.INDEX.DIM4 +
                      (input$dim_5/dim_total)*SPI.INDEX.DIM5 
                                         #sum up based on individual dimension weights
          ) %>% #
          mutate(across(starts_with('SPI.INDEX'),~100*.)) %>% #create weighted index
          select(country, SPI.INDEX,SPI.INDEX.DIM1,SPI.INDEX.DIM2,SPI.INDEX.DIM3,SPI.INDEX.DIM4,SPI.INDEX.DIM5) 
        
        
        #calculate the breaks for the color coding
        brks <- quantile(index_tab$SPI.INDEX, probs=c(1,2,3,4)/5,na.rm=T)

        brks1 <- quantile(index_tab$SPI.INDEX.DIM1, probs=c(1,2,3,4)/5,na.rm=T)

        brks2 <- quantile(index_tab$SPI.INDEX.DIM2, probs=c(1,2,3,4)/5,na.rm=T)

        brks3 <- quantile(index_tab$SPI.INDEX.DIM3, probs=c(1,2,3,4)/5,na.rm=T)

        brks4 <- quantile(index_tab$SPI.INDEX.DIM4, probs=c(1,2,3,4)/5,na.rm=T)

        brks5 <- quantile(index_tab$SPI.INDEX.DIM5, probs=c(1,2,3,4)/5,na.rm=T)

        #make nice looking table
        #make nice looking table
        DT::datatable(index_tab, caption=paste('Overall SPI Index in ',input$country_year_choice,' and Dimension Scores.', sep=""),
                      rownames=FALSE,
                      colnames = c("Country", "SPI Index Value", "Dim 1: Data Use", "Dim 2: Data Services","Dim 3: Data Products ","Dim 4: Data Sources","Dim 5: Data Infrastructure" ),
                      class='cell-border stripe',
                      escape = FALSE,
                      extensions = c ('Buttons', 'FixedHeader'), options=list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel'),
                        pageLength = 60,
                        scrollX = TRUE, 
                        paging=FALSE)) %>%
          formatRound(columns=c('SPI.INDEX','SPI.INDEX.DIM1','SPI.INDEX.DIM2','SPI.INDEX.DIM3','SPI.INDEX.DIM4','SPI.INDEX.DIM5'), digits=1) %>%
          formatStyle(    'SPI.INDEX', backgroundColor = styleInterval(brks, col_palette)) %>%
          formatStyle(    'SPI.INDEX.DIM1', backgroundColor = styleInterval(brks1, col_palette)) %>%
          formatStyle(    'SPI.INDEX.DIM2', backgroundColor = styleInterval(brks2, col_palette)) %>%
          formatStyle(    'SPI.INDEX.DIM3', backgroundColor = styleInterval(brks3, col_palette)) %>%
          formatStyle(    'SPI.INDEX.DIM4', backgroundColor = styleInterval(brks4, col_palette)) %>%
          formatStyle(    'SPI.INDEX.DIM5', backgroundColor = styleInterval(brks5, col_palette))
            
        
        
    })

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


