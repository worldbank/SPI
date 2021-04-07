#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(wbstats)

SPI <- read_csv('SPI_index.csv')


metadata_card <- read_csv('SPI_card_sources.csv') 

country_info <- wb_countries() %>%
    mutate(income=income_level,
           lending=lending_type) %>%
    filter(region!="Aggregates") %>%
    select(iso3c, country, region, income, lending)

#get average for each 



card <- function(dimension_name, Description, value, color) {
    HTML(
        paste0(
      '<div class="card">
        <div style="background: ',color,'" class="container">
        <h4><b> ', dimension_name, '</b></h4>
        <hr>
        <h3> <center> Value: ', value, ' </center> </h3>
        </div>
        </div>
      '
          
        )
    )
}


ui <- fluidPage(
    h1("SPI Dashboard"),
    tags$head(tags$style('.card {
                         width: 250px;
                         height: 150px;
                       clear: both;
                       /* Add shadows to create the "card" effect */
                       box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                       transition: 0.3s;
                       }
                       /* On mouse-over, add a deeper shadow */
                       .card:hover {
                       box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
                       }
                       /* Add some padding inside the card container */
                       .container {
                       width: 250px;
                       height: 150px;
                       padding: 2px 16px;
                       }')),
    fluidRow(
        column(2,
               selectizeInput("country_choice_card",
                              "Choose Country",
                              choices=as.character(country_info$country),
                              selected='Afghanistan')),
        column(2,offset=1,
               selectizeInput("year_choice_card",
                              "Choose Year",
                              choices=c(2004:2019),
                              selected=2019))
    ),
    wellPanel(
        #h3('Pillar 1: Data Use'),
        uiOutput("cards1")
            ),
    wellPanel(
        #h3('Pillar 2: Data Services'),
        uiOutput("cards2")
        ),
    wellPanel(
        #h3('Pillar 3: Data Products'),
    
        uiOutput("cards3")
        ),
    wellPanel(
        #h3('Pillar 4: Data Sources'),
        
        uiOutput("cards4")
        ),
    wellPanel(
        #h3('Pillar 5: Data Infrastructure'),
        
        uiOutput("cards5")
    )
    
)

server <- function(input, output, session) {
    
    card_df<- reactive({
        
        SPI %>%
            filter(date==input$year_choice_card) %>%
            filter(country==input$country_choice_card) %>%
            mutate(SPI.DIM4.1.INDEX=(SPI.DIM4.1.CEN.INDEX + SPI.DIM4.1.SVY.INDEX)/2
            ) %>%
            select(-SPI.DIM4.1.CEN.INDEX,-SPI.DIM4.1.SVY.INDEX) %>%
            select(country, starts_with("SPI.DIM") ) %>%
            pivot_longer(
                cols = starts_with("SPI.DIM"),
                names_to='source_id',
                values_to='value'
            ) %>%
            mutate(value=if_else(value==-99,as.numeric(NA),value)
            ) %>%
            mutate(value=round(value,1)) %>%
            right_join(metadata_card) %>%
            mutate(color=case_when(
                value==0 ~ "#E63946",
                value>0 & value <1 ~ "#FDE74C",
                value==1 ~ "#457B9D",
                TRUE ~ "gray"
            )) %>%
            arrange(descript)
        

        
        
        
    }) 
    
    
   


    
    output$cards1 <- renderUI({
        
        # First make the cards
        args <- lapply(1:5, function(.x) card(dimension_name=card_df()[.x, "descript"],
                                               Description = card_df()[.x, "spi_indicator_description"],
                                               value = card_df()[.x, "value"],
                                               color=card_df()[.x,'color']))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
        
    })
    
    output$cards2 <- renderUI({
        
        # First make the cards
        args <- lapply(6:9, function(.x) card(dimension_name=card_df()[.x, "descript"],
                                               Description = card_df()[.x, "spi_indicator_description"],
                                               value = card_df()[.x, "value"],
                                               color=card_df()[.x,'color']))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
        
    })
    
    output$cards3 <- renderUI({
        
        # First make the cards
        args <- lapply(10:13, function(.x) card(dimension_name=card_df()[.x, "descript"],
                                               Description = card_df()[.x, "spi_indicator_description"],
                                               value = card_df()[.x, "value"],
                                               color=card_df()[.x,'color']))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
        
    })
    
    output$cards4 <- renderUI({
        
        # First make the cards
        args <- lapply(14:17, function(.x) card(dimension_name=card_df()[.x, "descript"],
                                               Description = card_df()[.x, "spi_indicator_description"],
                                               value = card_df()[.x, "value"],
                                               color=card_df()[.x,'color']))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
        
    })
    
    output$cards5 <- renderUI({
        
        # First make the cards
        args <- lapply(18:22, function(.x) card(dimension_name=card_df()[.x, "descript"],
                                                Description = card_df()[.x, "spi_indicator_description"],
                                                value = card_df()[.x, "value"],
                                                color=card_df()[.x,'color']))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
        
    })
}

shinyApp(ui, server)