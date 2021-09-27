
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
      tabsetPanel(
        tabPanel("tab1", leafletOutput("map")),
        tabPanel("tab2")
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  rawDF <- read_csv("input_data/rawDF.csv") 
  selectDF <- rawDF[, c("country", "year", "co2")]
  shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  WorldCountry <- geojson_read(shapeurl, what = "sp")
  
  yearDF <- reactive({
    return(left_join(data.frame(Name = WorldCountry$name), selectDF %>% 
                       filter(year == input$year), by = c("Name" ="country")))
  })
  
  pal <- reactive({
    colorBin("magma", domain = yearDF()$co2)})
  
  output$map <- renderLeaflet(
    leaflet(WorldCountry) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~(pal()(yearDF()$co2)),
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
      addLegend(pal = pal(), values = yearDF()$co2,
                title = "CO2", position = "bottomright")
  )
  
}


shinyApp(ui = ui, server = server)
