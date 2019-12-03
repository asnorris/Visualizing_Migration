#PROJECT APP 
# This app was created for my Gov 1005 final project.

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

# Download data using World Bank R package. This data isasylum data showing the
# number of refugees entering a country in a given year. I read this data in
# without cleaning it because it will be used in its raw form later when looking
# at the relationship between refugees and GDP.

ref <- wb(indicator = "SM.POP.REFG")
# I filtered the refugee asylum data to selecct only for the variables that I
# needed. The World Bank data contains variables in the "country" column that
# are not countries but regions or the world as a whole. These categories all
# appear before the individual countries in the data set so I manually cleaned
# the data using slice to remove these non-countries.

refugee_go <- ref %>%
  select(iso3c, country, value, date) %>%
  slice(1323:5731)

# In this code chunk I did the same thing as was done above but this time on
# data showing the number of refugees fleeing a given country. I had to use
# different numbers than I used above to slice the data because there was a
# different number of non-country categories within the country column.

ref_origin <- wb(indicator = "SM.POP.REFG.OR") %>%
  select(iso3c, country, value, date) %>%
  slice(1335:6493)

# Downloaded data on GDP Per Capita growth rate from the World Bank

gdp_percap_grow <- wb(indicator = "NY.GDP.PCAP.KD.ZG")

# Downloaded data on GDP Per Capita from the World Bank

gdp_percap <- wb(indicator = "NY.GDP.PCAP.CD")

# Downloaded data on GDP from the World Bank

gdp <- wb(indicator = "NY.GDP.MKTP.CD")

# In order to observe the relationship between refugees and GDP indicators, I
# needed to combine the data into one dataset.  I used merge to do this. I could
# only merge two datasets at a time so I did this in a couple of iterations.
# After each iteration I used mutate to label the columns so that they were not
# simply value.x and used select to remove unnecessary columns.

gdp_grow_ref <- merge(gdp_percap_grow, ref, by = c("iso3c", "date", "country", "iso2c")) %>%
  mutate(gdp_percap_growth = value.x,
         refugee_number = value.y) %>%
  select(iso3c, iso2c, country, date, gdp_percap_growth, refugee_number)

# I added the GDP data to the combined dataset

with_gdp <- merge(gdp_grow_ref, gdp, by = c("iso3c", "date", "country", "iso2c")) %>%
  mutate(gdp = value) %>%
  select(iso3c, iso2c, country, date, gdp, gdp_percap_growth, refugee_number)

# This is the final cleaned and comprehensive data set. It contains data on the
# number of refugees entering a country, GDP, GDP Per Capita, and GDP Per Capita
# growth rate.

data <- merge(with_gdp, gdp_percap, by = c("iso3c", "date", "country", "iso2c")) %>%
  mutate(gdp_percap = value) %>%
  select(iso3c, iso2c, country, date, gdp, gdp_percap, gdp_percap_growth, refugee_number)


# Here I create the ui for my shiny app.

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # Create my nav bar with the names of the tabs I want in my app.
  
  navbarPage("Visualizing Migration",
    
             # The about tab has information about my project, myself, and my
             # data sources. I used tags$div and a() to create hyperlinks that
             # can connect users to my data sources.
             
             tabPanel("About",
                      h1("Visualizing Migration"),
                        h2("About This Project"),
                          tags$div("This project is a visualization of migration data sourced from the World Bank from 1990 to 2018 looking at data on
                                   where refugees are coming from, where refugees are going, and the relationship between refugee numbers and GDP indicators within different contexts. Given the current political context regarding refugees and whether or not they are welcomed into western countries, I decided to do this project to
                                   better understand the correlations between refugees and different GDP indicators."),
                        h2("About Me"),
                          tags$div("My name is Alexandra Norris and I am currently a junior at Harvard College studying Government with a secondary in Economics.
                                   This Shiny App is my final project for GOV 1005 taught by David Kane. Check out my GitHub", a(href =  "https://github.com/asnorris",
                                                                                                                                 "here!")),
                        h2("Data Sources"),
                          tags$div("All of my data was sourced from the World Bank's", 
                                   a(href = "https://databank.worldbank.org/home.aspx", "Data Bank."),
                                   "The Data Bank contains data collected by the World Bank, IMF, and other UN organizations on different political, economic, and humanitarian indicators.
                                   For this project, I used data on",
                                   a(href = "https://data.worldbank.org/indicator/sm.pop.refg", "Refugee Countries of Origin,"), 
                                   a(href = "https://data.worldbank.org/indicator/SM.POP.REFG.OR", "Refugee Countreis of Asylum,"), 
                                   a(href = "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD", "GDP,"), 
                                   a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.KD", "GDP Per Capita,"),
                                   "and",
                                   a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG", "GDP Per Capita Growth Rate.")
                                   )
                        ),
          
              # This tab shows maps of the world indicating the number of
              # refugees entering and leaving a country. I create a slider to
              # adjust the year visible and a select option so that the user can
              # choose whether to look at countries of asylum or countries of
              # origin.
             
              tabPanel(("Refugee Population Map"),
                    
                  h4("The below maps show the number of refugees fleeing or seeking asylum in a given country. Use the slider to view the maps in different
                      years and use the drop-down menu to choose between viewing countries of asylum or countries of origin. The scale on the right shows which
                      colors correspond to the different numbers of refugees."),
                  
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year", "Choose a Year", min = 1990, max = 2018, value = 2018, sep = ""),
                          selectInput("type", "Choose Country of Asylum or Country of Origin", choices = c("Origin", "Asylum"), selected = "Asylum")),
                        
                      mainPanel(
                        plotlyOutput("ref_asylum")
                      )
                      
                      )),
             
             # This tab shows three graphs showing the relationship between the
             # number of refugees entering a country and different GDP
             # indicators. I created a select option so the user can choose
             # which type of GDP indicator they would like to observe.
             
             tabPanel(("Refugees and GDP"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("gdp", "Refugee Affect On:", choices = c("GDP", "GDP Per Capita", "GDP Per Capita Growth Rate"), selected = "GDP")),
                      mainPanel(
                        plotOutput("gdp_graph")
                      )),
                      
                      tags$div("The above graphs show the relationship between the number of refugees seeking asylum in a country and three GDP indicators: GDP, GDP Per Capita,
                                and the GDP Per Capita Growth Rate.",
                               
                               tags$br(),
                               
                               "Looking at the above graphs, we observe a positive relationship between refugees and GDP with 
                               a correlation coeffecient of 1,389,293, meaning that an increase of one refugee is associated with a $1,389,293 USD increase in GDP.",
                               
                               tags$br(),
                               
                               "The graph of GDP Per Capita and refugees shows a slight negative relationship between the two with a correlation coffecient of  -0.0007, meaning
                               that an increase of one refugee is associated with a $0.0007 USD decrease in GDP per capita.",
                               
                               tags$br(),
                               
                               "The graph of refugees and GDP per capita growth rate shows that there is a very slight negative relationship between the two with a correlation coefficient of  -0.00000001, meaning that an
                               increase of one refugee is associated with a $0.00000001 USD decrease in the GDP per capita growth rate of a nation. This correlation is not
                               statistically significant given that the 95% confidence interval for this data encompasses zero, meaning that the correlation could be either
                               positive, negative, or zero.")
                      
                  ), 
             
             # This tab shows three graphs looking at the relationship between
             # GDP per capita in three developed contexts. I created a select
             # option so that users can choose which type of developed context
             # they want to look at.
             
             tabPanel(("Refugees in The Developed World"),
             
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country", "Select Type of Country:", choices = c("High Income", "Euro Area", "North America"), selected = "High Income")),
                      mainPanel(
                 
                 plotOutput("rich_graph")
               )),
               
               tags$div("The graphs above show the relationships between the number of refugees in a country and GDP per capita. The data I downloaded from the World Bank's 
                        Data Bank had regional and economic indicators along with country indicators. This allowed me to look at data explicitly for high income countries,
                        countries in the European Union, and countries in North America without much data cleaning. According to the",
                        a(href = "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups", "World Bank,"),
                        "a nation classifies as high income if it has a GNI per capita of at least $12,376.",
                        tags$br(),
                        "As you can see by looking at each of the graphics, there is a negative correlation between refugees and GDP per capita in all of them. 
                        This indicates that in much of the developed world (as defined by high income countries, the EU, and countries in North America), an increase
                        in the number of refugees is associated with a decrease in GDP per capita.")),
             
             # This tab is where I make conclusions about the data and include
             # my video conclusion.
             
             tabPanel(("Conclusion"),
              h1("Thank you for checking out my Shiny App!"))
             
  )
)

# Define server logic required to draw my maps and graphs

server <- function(input, output){
  
  # Create my maps.  I use if else statements so that the map changes depending
  # on the input selected by the user.
   
   output$ref_asylum <- renderPlotly({
     if(input$type == "Asylum") {
       place <- refugee_go
       title_map <- "Refugee Numbers by Country of Asylum"
     } else {
       place <- ref_origin
       title_map <- "Refugee Numbers by Country of Origin"
     }
    
     # To create my map I use the plotly package, specifically the plot_geo
     # function. Because most of the variation in refugee numbers is between 0
     # and 1 million with a few high outliers, I felt that the best way to
     # display this with a color scale was to do it logarithmicly. To create a
     # color scale in log10 I used the colorscale argument followed by list
     # columns relating specific colors to specific numbers of refugees. The
     # dtick argument allows my color scale on the side of the map to be labeled
     # at each million. This was the only way to make it readable because if I
     # were to label the earlier values due to it being done in log form, the
     # labels would overlap and look bad.
     
       place %>%
       filter(date == input$year) %>%
         plot_geo(locationmode = 'country names') %>%
         add_trace(
           z = ~value, locations = ~country, color = ~value, colorbar = list(tick0 = 0,
                                                                             tickmode = "log",
                                                                             dtick =1000000),
           colorscale = list(
             list(0, 'rgb(255,255,204)'),
             list(10/10000000, 'rgb(229,255,204)'),
             list(100/10000000, 'rgb(204,255,229)'),
             list(1000/10000000, 'rgb(102,255,255)'),
             list(10000/10000000, 'rgb(107,178,255)'),
             list(100000/10000000, 'rgb(0,128,255)'),
             list(1000000/10000000, 'rgb(0,0,255)'),
             list(10000000/10000000, 'rgb(153,0,76)')))
   })
   
   # Here I create the graphs showing the relationship between incoming refugees
   # and GDP indicators. Again, I use if else statements so that the data in the
   # graph changes depending on the input selected by the user. Here I define
   # the input specific variables.
   
  output$gdp_graph <- renderPlot({
    if(input$gdp == "GDP") {
      y_value <- data$gdp
      y_lab <- "GDP"
      gdp_title <- "Number of Refugees and GDP"
    } 
    else if(input$gdp == "GDP Per Capita") {
      y_value <- data$gdp_percap
      y_lab <- "GDP Per Capita"
      gdp_title <- "Number of Refugees and GDP Per Capita"
    } else {
      y_value <- data$gdp_percap_growth
      y_lab <- "GDP Per Capita Growth Rate"
      gdp_title <- "Number of Refugees and GDP Per Capita Growth Rate"
    }
    
    # Use ggplot to create the framework for the graph. I used geom point to
    # make a scatter plot, geom smooth to add a line of best fit, and scale x
    # and y continuous to avoid the axies being in scientific notation because
    # it is not user friendly.
    
    data %>%
      ggplot(aes(refugee_number, y_value)) +
      geom_point() +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      geom_smooth(method = "lm") +
      labs(title = gdp_title, x = "Number of Refugees", y = y_lab, caption = "Data sourced from the World Bank's Data Bank")
  })
  
  # Create graphs for the observations on different types of developed nations
  
output$rich_graph <- renderPlot({
  if(input$country == "High Income") {
    rich_type <- "High income"
    rich_title <- "High Income Countries"
  } 
  else if(input$country == "Euro Area") {
    rich_type <- "Euro area"
    rich_title <- "Countries in the European Union"
  } else {
    rich_type <- "North America"
    rich_title <- "North American Countries"
  }
  
  # Create graphs for different types of rich countries. Use filter to only
  # observe data that the user selects. Use geom point and geom smooth to create
  # scatter plots and lines of best fit.
  
  data %>%
    filter(country == rich_type) %>%
    ggplot(aes(refugee_number, gdp_percap)) +
    geom_point() +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    geom_smooth(method = "lm") +
    labs(title = rich_title, x = "Number of Refugees", y = "GDP Per Capita", caption = "Data sourced from the World Bank's Data Bank")
  
})
}

# Run the application 
shinyApp(ui = ui, server = server)

