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

#read in data and metatdata
SPI <- read_csv('SPI_data.csv')
metadata <- read_csv('SPI_dimensions_sources.csv') %>%
    mutate(descript=paste(SPI_indicator_id,": ", spi_indicator_name," - ",source_name , sep="")) %>%
    select(source_id,  descript, spi_indicator_description)

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
    select(iso3c, region, income, lending)


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
                 # Data Analysis Section
                 ####################################################
                 tabPanel("SPI Data Explorer",
                          div(class="outer",
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              withSpinner(leafletOutput("spi_map_overall"
                                                        , width = "100%", height = "800px")),
                              
                              h3('Summary Statistics of Indicators'),
                              withSpinner(DT::dataTableOutput("summary_stats_overall")),
                              h3('Indicators Over Time'),
                              selectizeInput("over_time",
                                             "Choose Indicator to Plot Over Time",
                                             choices=as.character(metadata$descript)
                                             
                              ),
                              withSpinner(plotlyOutput('plot_time',
                                                       width = '70%')),
                              
                              h3('Correlations of Indicators'),
                              withSpinner(plotOutput('corrplot_overall', width = "100%", height = "900px")),
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls_overall", class = "panel panel-default", fixed = FALSE,
                                            draggable = FALSE, top = 150, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Indicator Year"),
                                            selectizeInput("year_overall",
                                                           "Reference Year",
                                                           choices=c(2004:2019),
                                                           selected=2018
                                                           
                                            ),
                                            selectizeInput("income_groups_overall",
                                                           "Select Income Groups",
                                                           choices=c("Low income","Lower middle income","Upper middle income","High income"),
                                                           selected=c("Low income","Lower middle income","Upper middle income","High income"),
                                                           multiple=T
                                            ),
                                            selectizeInput("color_choices_overall", "Choose Indicator to Color Map", 
                                                           choices=metadata$descript) 
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
                      colnames = c("Indicator ID", "Indicator Name", "Source"),
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
        
        if (time_var() %in% c('SPI.OVRL.SCR', 'SPI.D1.MSC', 'SPI.D2.CS', 'SPI.D3.AKI', 'SPI.D4.DPO')) {
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
            select(iso3c, country,  income, date, starts_with('SPI'), population) %>%
            filter(date==input$year_overall) %>%
            filter(income %in% input$income_groups_overall) %>% #filter out income groups not selected
            mutate(ISO_A3_EH=iso3c) 
        
        
    })
    
    # updateSelectizeInput(session, 'color_choices_overall', choices = c('SPI.OVRL.SCR', 'SPI.D1.MSC', 'SPI.D2.CS', 'SPI.D3.AKI', 'SPI.D4.DPO'), server = TRUE)
    
    map_var <- reactive({
        
        names(var.labels[match(input$color_choices_overall, var.labels)])
        
    })
    
    output$spi_map_overall <- renderLeaflet({
        
        
        spi_map_overall<-countries
        
        spi_map_overall@data <- spi_map_overall@data %>%
            left_join(df_overall()) 
        
        
        
        
        #create pallete
        pal <- colorBin("RdYlGn", select(spi_map_overall@data, map_var())[,1], 10, pretty = T)
        
        
        
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
            addLegend(pal=pal, values=~select(spi_map_overall@data, map_var())[,1], opacity=0.7, title='Indicator value', position="bottomleft")        
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
            htmltools::em("Unweighted Mean of SPI Indicators by Region.  Sub-indicators scaled to be 0-100."))    ,
            extensions = 'Buttons', options=list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                pageLength = 60)) 
        
        
        
    })
    
    ###
    # Correlation between AKI indicators
    ###
    
    
    corrp_overall <- reactive({
        
        metadata_tab3_overall <- metadata %>% 
            janitor::clean_names() %>%
            filter(grepl('SPI',source_id)) %>%
            select(source_id, descript)
        
        corr_df <- df_overall()
        corr_names<-metadata_tab3_overall$source_id
        names(corr_names) = metadata_tab3_overall$descript
        
        #calculate correlations between teacher practices
        df_corr_plot <-    round(cor(select(corr_df,corr_names), use="complete.obs"), 2)
        
        #plot the correlation in a nicely formatted table
        pcorr<- ggcorrplot(df_corr_plot,
                           outline.color = "white",
                           ggtheme = theme_bw(),
                           colors = c("#F8696B", "#FFEB84", "#63BE7B"),
                           legend.title = "Correlation",
                           title = "Correlation Between SPI Indicators",
                           lab=T) + 
            scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
            scale_y_discrete(labels = function(x) str_wrap(x, width = 16)) +
            theme_bw() +
            theme(
                text = element_text(size = 12),
            ) +
            labs(caption = "Indicator values are converted to be between 0 and 1 based on whether they meet SPI scoring criteria." 
            )
        
        pcorr
        
    })
    
    
    
    
    
    
    output$corrplot_overall <- renderPlot({
        
        corrp_overall()
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
