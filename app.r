
library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(tidyverse) 
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(dplyr)      # Used for data manipulation and merging
library(htmltools)  # Used for constructing map labels using HTML


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("GHG Emission"),
  sidebarLayout(
    sliderInput("year", "Year:",
                min = min(selectDF $  year), max = max(selectDF $  year),
                value = 2000),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Emission Per Year", leafletOutput("mapPerYear")),
                  tabPanel("Emission Per Capita", leafletOutput("mapPerCap"))
      )
        
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  rawDF <- read_csv("input_data/rawDF.csv") 
  selectDF <- rawDF[, c("country", "year", "co2", "co2_per_capita")]
  shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  WorldCountry <- geojson_read(shapeurl, what = "sp")
  
  yearDF <- reactive({
    return(left_join(data.frame(Name = WorldCountry$name), selectDF %>% 
                       filter(year == input$year), by = c("Name" ="country")))
  })
  pal_PerYear <- reactive({
    colorBin("magma", domain = yearDF()$co2)})
  
  mapPerYear <- renderLeaflet(
    leaflet(WorldCountry) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~(pal_PerYear()(yearDF()$co2)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7, 
        highlight = highlightOptions(
          weight = 3,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE), 
        label = lapply(paste("<strong>", yearDF()$Name, "</strong>", "<br/>", 
                             "CO2:", yearDF()$co2)
                       , HTML)) %>% 
      addLegend(pal = pal_PerYear(), values = yearDF()$co2,
                title = "CO2", position = "bottomright")
  )
  
  output$mapPerYear <- mapPerYear
  
  pal_PerCap <- reactive({
    colorBin("magma", domain = yearDF()$co2_per_capita)})
  
  mapPerCap <- renderLeaflet(
    leaflet(WorldCountry) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~(pal_PerCap()(yearDF()$co2_per_capita)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7, 
        highlight = highlightOptions(
          weight = 3,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE), 
        label = lapply(paste("<strong>", yearDF()$Name, "</strong>", "<br/>", 
                             "CO2:", yearDF()$co2_per_capita)
                       , HTML)) %>% 
      addLegend(pal = pal_PerCap(), values = yearDF()$co2_per_capita,
                title = "CO2", position = "bottomright")
  )
  output$mapPerCap <- mapPerCap
}


shinyApp(ui = ui, server = server)
