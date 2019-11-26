#PROJECT APP 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wbstats)
library(countrycode)
library(tidyverse)
library(ggplot2)
library(essurvey)
library(sf)
library(gganimate)
library(plotly)
library(rnaturalearth)

ref <- wb(indicator = "SM.POP.REFG")

refugee_go <- ref %>%
  select(iso3c, country, value, date) %>%
  slice(1323:5731)

ref_origin <- wb(indicator = "SM.POP.REFG.OR") %>%
  select(iso3c, country, value, date) %>%
  slice(1335:6493)


ui <- fluidPage(
  
  # add title
  
  navbarPage("Visualizing Migration",
             tabPanel("About",
                      h4("My name is Alexandra Norris and I am currently a junior at Harvard College studying Government with a secondary in Economics.  
                         
                         This project is a visualization of migration data sourced from the World Bank from 1960 to 2018 looking at refugees by both country of origin and countrty of asylum.  
                         The shape data was sourced from github.")),
             tabPanel(("Refugee Populations by Country of Origin"),
                      
                      mainPanel(
                        
                        # output: graph
                        plotOutput("graph")
                      )
             ),
             tabPanel(("Refugee Populations by Country of Asylum"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year", "Choose a Year", min = 1990, max = 2018, value = 2018, sep = ""),
                          selectInput("type", "Choose Origin or Destination", choices = c("Origin", "Asylum"), selected = "Asylum")),
                      mainPanel(
                        
                        # output: graph
                        plotlyOutput("ref_asylum")
                      )
             )),
             
             tabPanel("Refugees and GDP")
             
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$ref_asylum <- renderPlotly({
     if(input$type == "Asylum") {
       place <- refugee_go
     } else {
       place <- ref_origin
     }
    
       place %>%
       filter(date == input$year) %>%
       plot_geo(locationmode = 'country names') %>%
       add_trace(
         z = ~value, locations = ~country, color = ~value, colors = "RdBu"
       )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

