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
library(shinythemes)

ref <- wb(indicator = "SM.POP.REFG")

refugee_go <- ref %>%
  select(iso3c, country, value, date) %>%
  slice(1323:5731)

ref_origin <- wb(indicator = "SM.POP.REFG.OR") %>%
  select(iso3c, country, value, date) %>%
  slice(1335:6493)

gdp_percap_grow <- wb(indicator = "NY.GDP.PCAP.KD.ZG")

gdp_percap <- wb(indicator = "NY.GDP.PCAP.CD")

gdp <- wb(indicator = "NY.GDP.MKTP.CD")

gdp_grow_ref <- merge(gdp_percap_grow, ref, by = c("iso3c", "date", "country", "iso2c")) %>%
  mutate(gdp_percap_growth = value.x,
         refugee_number = value.y) %>%
  select(iso3c, iso2c, country, date, gdp_percap_growth, refugee_number)

with_gdp <- merge(gdp_grow_ref, gdp, by = c("iso3c", "date", "country", "iso2c")) %>%
  mutate(gdp = value) %>%
  select(iso3c, iso2c, country, date, gdp, gdp_percap_growth, refugee_number)

data <- merge(with_gdp, gdp_percap, by = c("iso3c", "date", "country", "iso2c")) %>%
  mutate(gdp_percap = value) %>%
  select(iso3c, iso2c, country, date, gdp, gdp_percap, gdp_percap_growth, refugee_number)


ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # add title
  
  navbarPage("Visualizing Migration",
             tabPanel("About",
                      h4("My name is Alexandra Norris and I am currently a junior at Harvard College studying Government with a secondary in Economics.  
                         
                         This project is a visualization of migration data sourced from the World Bank from 1960 to 2018 looking at refugees by both country of origin and countrty of asylum.  
                         The shape data was sourced from github.")),
          
             tabPanel(("Refugee Population Map"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year", "Choose a Year", min = 1990, max = 2018, value = 2018, sep = ""),
                          selectInput("type", "Choose Country of Asylum or Country of Origin", choices = c("Origin", "Asylum"), selected = "Asylum")),
                      mainPanel(
                        
                        # output: graph
                        plotlyOutput("ref_asylum")
                      )
             )),
             
             tabPanel(("Refugees and GDP"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("gdp", "Refugee Affect On:", choices = c("GDP", "GDP Per Capita", "GDP Per Capita Growth Rate"), selected = "GDP")),
                      mainPanel(
                        
                        plotOutput("gdp_graph")
                      ))),
             
             tabPanel(("Refugees in The Developed World"),
             
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country", "Select Type of Country:", choices = c("High Income", "Euro Area", "North America"), selected = "High Income")),
                      mainPanel(
                 
                 plotOutput("rich_graph")
               ))),
             
             tabPanel("Conclusion")
             
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
   
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
   
  output$gdp_graph <- renderPlot({
    if(input$gdp == "GDP") {
      y_value <- data$gdp
      y_lab <- "GDP"
    } 
    if(input$gdp == "GDP Per Capita") {
      y_value <- data$gdp_percap
      y_lab <- "GDP Per Capita"
    } else {
      y_value <- data$gdp_percap_growth
      y_lab <- "GDP Per Capita Growth Rate"
    }
    
    data %>%
      ggplot(aes(refugee_number, y_value)) +
      geom_point() +
      scale_x_continuous(labels = scales::comma) +
      geom_smooth(method = "lm") +
      labs(title = "Refugee Counts and GDP Per Capita Growth", x = "Number of Refugees", y = y_lab)
  })
  
output$rich_graph <- renderPlot({
  if(input$country == "High Income") {
    rich_type <- "High income"
  } 
  if(input$country == "Euro Area") {
    rich_type <- "Euro area"
  } else {
    rich_type <- "North America"
  }
  
  data %>%
    filter(country == rich_type) %>%
    ggplot(aes(gdp_percap, refugee_number)) +
    geom_point() +
    scale_x_continuous(labels = scales::comma) +
    geom_smooth(method = "lm") +
    labs(title = "title")
  
})
}

# Run the application 
shinyApp(ui = ui, server = server)

