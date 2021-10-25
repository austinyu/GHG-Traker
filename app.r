
# library(shiny)
# library(leaflet)
# library(RColorBrewer)
# library(ggplot2)
# library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
# library(htmltools)  # Used for constructing map labels using HTML
# library(shinyWidgets)

# load required packages
library(tidyverse)
#if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
#if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
#if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
if(!require(modeldata)) install.packages("modeldata", repos = "http://cran.us.r-project.org")
if(!require(networkD3)) install.packages("networkD3", repos = "http://cran.us.r-project.org")

rawDF <- read_csv("input_data/rawDF.csv")
selectDF <- rawDF[, c("country", "year", "co2", "co2_per_capita", "co2_per_gdp",
                      "total_ghg", "ghg_per_capita", "ghg_per_gdp",
                      "methane", "methane_per_capita", "methane_per_gdp",
                      "nitrous_oxide", 'nitrous_oxide_per_capita', "nitrous_oxide_per_gdp")]
catOfDF <- vector(mode="list", length=4)
names(catOfDF) <- c("total_ghg", "co2", "methane", "nitrous_oxide")
catOfDF[[1]] <- selectDF %>%
  select("country", "year", "total_ghg", "ghg_per_capita", "ghg_per_gdp") %>%
  rename( "total" = "total_ghg",
          "per_capita" = "ghg_per_capita",
          "per_gdp" = "ghg_per_gdp")
catOfDF[[2]] <- selectDF %>%
  select("country", "year", "co2", "co2_per_capita", "co2_per_gdp") %>%
  rename( "total" = "co2",
          "per_capita" = "co2_per_capita",
          "per_gdp" = "co2_per_gdp")
catOfDF[[3]] <- selectDF %>%
  select("country", "year", "methane", "methane_per_capita", "methane_per_gdp") %>%
  rename( "total" = "methane",
          "per_capita" = "methane_per_capita",
          "per_gdp" = "methane_per_gdp")
catOfDF[[4]] <- selectDF %>%
  select("country", "year", "nitrous_oxide", "nitrous_oxide_per_capita", "nitrous_oxide_per_gdp") %>%
  rename( "total" = "nitrous_oxide",
          "per_capita" = "nitrous_oxide_per_capita",
          "per_gdp" = "nitrous_oxide_per_gdp")

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(
    title = "Interactive GHG Traker",
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    tabPanel("Total GHG Map",
      div(class="outer", tags$head(includeCSS("styles.css")),
        title = "Map",
        leafletOutput("mymap", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 150, left = 55, width = 250, fixed=TRUE, height = "auto", draggable = TRUE,
                      sliderInput("year", "Select a Year:",
                        min = min(selectDF$year), max = max(selectDF$year),
                        value = 2000),
                      selectInput("species", "Select a type of GHG:",
                        c("Total GHG" = "total_ghg",
                        "CO2" = "co2",
                        "Methane" = 'methane',
                        "Nitrous Oxide" = 'nitrous_oxide'), selected = "co2"),
                      selectInput("kind", "Select a Map:",
                                  c("Total Emission" = "Total Emission",
                                    "Per Capita" = "Per Capita",
                                    "Per GDP" = 'Per GDP'))
          )
        )
    ),
    # tabPanel("Graphs", leafletOutput("mapPerYear")),
    tabPanel("Data",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
              "Adapted from data on CO2 and Greenhouse Gas Emissions by", tags$a(href="https://github.com/owid/co2-data",
                                                                 "Our World in Data."), tags$br(),
              "CSS and HTML style files are from ",  tags$a(href="https://github.com/eparker12/nCoV_tracker","nCoV_tracker")
             )
    )
  )
)
# branch

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  WorldCountry <- geojson_read(shapeurl, what = "sp")

  yearDF <- reactive({
    return(left_join(data.frame(Name = WorldCountry$name), catOfDF[[input$species]] %>%
                       filter(year == input$year), by = c("Name" ="country")))
  })

  # plot total emission per year
  pal_PerYear <- reactive({
    colorBin("Oranges", domain = yearDF()$total, pretty = TRUE)})
  mapPerYear <- renderLeaflet(
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~(pal_PerYear()(yearDF()$total)),
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
                             input$species, yearDF()$total)
                       , HTML)) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend(pal = pal_PerYear(), values = yearDF()$total,
                title = paste(input$species, input$kind, sep=" "), position = "bottomright")
  )

  # plot emission per year and per capita
  pal_PerCap <- reactive({
    colorBin("Oranges", domain = yearDF()$per_capita, pretty = TRUE)})
  mapPerCap <- renderLeaflet(
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~(pal_PerCap()(yearDF()$per_capita)),
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
                             input$species, yearDF()$per_capita)
                       , HTML)) %>%
      addLegend(pal = pal_PerCap(), values = yearDF()$per_capita,
                title = paste(input$species, input$kind, sep=" "), position = "bottomright")
  )

  # plot emission per year and per gdp
  pal_PerGDP <- reactive({
    colorBin("Oranges", domain = yearDF()$per_gdp, pretty = TRUE)})
  mapPerGDP <- renderLeaflet(
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~(pal_PerGDP()(yearDF()$per_gdp)),
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
                             input$species, yearDF()$per_gdp)
                       , HTML)) %>%
      addLegend(pal = pal_PerGDP(), values = yearDF()$per_gdp,
                title = paste(input$species, input$kind, sep=" "), position = "bottomright")
  )
  observeEvent(input$kind, {
  if (input$kind == "Total Emission") {output$mymap <- mapPerYear}
  if (input$kind == "Per Capita") {output$mymap <-  mapPerCap}
  if (input$kind == "Per GDP") {output$mymap <-  mapPerGDP}
  })
}

shinyApp(ui = ui, server = server)
