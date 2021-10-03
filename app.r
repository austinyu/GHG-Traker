
library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(tidyverse) 
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(htmltools)  # Used for constructing map labels using HTML

rawDF <- read_csv("input_data/rawDF.csv") 
selectDF <- rawDF[, c("country", "year", "co2", "co2_per_capita", "co2_per_gdp",
                      "total_ghg", "ghg_per_capita", "ghg_per_gdp",
                      "methane", "methane_per_capita", "methane_per_gdp",
                      "nitrous_oxide", 'nitrous_oxide_per_capita', "nitrous_oxide_per_gdp")]
catOfDF <- vector(mode="list", length=4)
names(catOfDF) <- c("total_ghg", "co2", "methane", "nitrous_oxide")
catOfDF[[1]] <- selectDF %>% 
  select("total_ghg", "ghg_per_capita", "ghg_per_gdp") %>% 
  rename( "total" = "total_ghg",
          "per_capita" = "ghg_per_capita",
          "per_gdp" = "ghg_per_gdp")
catOfDF[[2]] <- selectDF %>% 
  select("co2", "co2_per_capita", "co2_per_gdp") %>% 
  rename( "total" = "co2",
          "per_capita" = "co2_per_capita",
          "per_gdp" = "co2_per_gdp")
catOfDF[[3]] <- selectDF %>%  
  select("methane", "methane_per_capita", "methane_per_gdp") %>% 
  rename( "total" = "methane",
          "per_capita" = "methane_per_capita",
          "per_gdp" = "methane_per_gdp")
catOfDF[[4]] <- selectDF %>% 
  select("nitrous_oxide", "nitrous_oxide_per_capita", "nitrous_oxide_per_gdp") %>% 
  rename( "total" = "nitrous_oxide",
          "per_capita" = "nitrous_oxide_per_capita",
          "per_gdp" = "nitrous_oxide_per_gdp")

# Define UI for app that draws a histogram ----
ui <- navbarPage(
  
  title = "Visualizing Greehouse Gas Emission",
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  tabPanel(
    title = "Map",
    sidebarLayout(
      sidebarPanel(
        title = "Explore GHG Emission",
        sliderInput("year", "Select a Year:",
                    min = min(selectDF$year), max = max(selectDF$year),
                    value = 2000),
        selectInput("species", "Select a type of GHG:", 
                    c("Total GHG" = 1,
                      "CO2" = 2,
                      "Methane" = 3,
                      "Nitrous Oxide" = 4))
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Emission Per Year", leafletOutput("mapPerYear")),
                    tabPanel("Emission Per Capita", leafletOutput("mapPerCap")),
                    tabPanel("Emission Per GDP", leafletOutput("mapPerGDP"))
        )
      )
    )
  )
  #tabPanel(
  #  tltle = "Under Construction"
  #)
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  WorldCountry <- geojson_read(shapeurl, what = "sp")
  
  yearDF <- reactive({
    return(left_join(data.frame(Name = WorldCountry$name), catOfDF[[input$species]] %>% 
                       filter(year == input$year), by = c("Name" ="country")))
  })
  
  # plot co2 emission per year
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

  # plot co2 emission per year and per capita
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
                title = "CO2 per Capita", position = "bottomright")
  )
  output$mapPerCap <- mapPerCap
  
  # plot co2 emission per year and per gdp
  pal_PerGDP <- reactive({
    colorBin("magma", domain = yearDF()$co2_per_gdp)})
  mapPerGDP <- renderLeaflet(
    leaflet(WorldCountry) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~(pal_PerGDP()(yearDF()$co2_per_gdp)),
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
                             "CO2:", yearDF()$co2_per_gdp)
                       , HTML)) %>% 
      addLegend(pal = pal_PerGDP(), values = yearDF()$co2_per_gdp,
                title = "CO2 per GDP", position = "bottomright")
  )
  output$mapPerGDP <- mapPerGDP
}


shinyApp(ui = ui, server = server)
