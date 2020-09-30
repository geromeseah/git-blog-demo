#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyr)
library(tidyverse)
library(lubridate)

#mass_shootings <- read_csv("data/mass_shootings.csv")
#mass_shootings$date = mdy(mass_shootings$date)

ui <- bootstrapPage(
    theme = shinythemes::shinytheme('simplex'),
    leaflet::leafletOutput('map', width = '100%', height = '100%'),
    absolutePanel(top = 10, right = 10, id = 'controls',
                  sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
                  dateRangeInput(
                      'date_range', 'Select Date', "2010-01-01", "2019-12-01"
                  ),
                  # CODE BELOW: Add an action button named show_about
                  actionButton('show_about', 'About'),
                  actionButton('show_text', 'Author')
    ),
    tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  ")
)
server <- function(input, output, session) {
    # CODE BELOW: Use observeEvent to display a modal dialog
    # with the help text stored in text_about.
    
    mass_shootings <- read_csv("data/mass_shootings.csv")
    mass_shootings$date = mdy(mass_shootings$date)
    text_about <- c("This data was compiled by Mother Jones, nonprofit founded in 1976. 
                    Originally covering cases from 1982-2012, this database has since been 
                    expanded numerous times to remain current.")
    text_about2 <- c("Gerome is an experienced business executive. Follow me at .....")
    
    observeEvent(input$show_about, 
                 {
                     showModal(modalDialog(text_about, title = 'About'))
                 }
    )
    
    observeEvent(input$show_text, 
                 {
                     showModal(modalDialog(text_about2, title = 'Author'))
                 }
    )
    
    output$map <- leaflet::renderLeaflet({
        mass_shootings %>% 
            filter(
                date >= input$date_range[1],
                date <= input$date_range[2],
                fatalities >= input$nb_fatalities
            ) %>% 
            leaflet() %>% 
            setView( -98.58, 39.82, zoom = 5) %>% 
            addTiles() %>% 
            addCircleMarkers(
                popup = ~ summary, radius = ~ sqrt(fatalities)*3,
                fillColor = 'red', color = 'red', weight = 1
            )
    })
}

shinyApp(ui, server)