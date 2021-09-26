library(shiny)
library(leaflet)
library(RColorBrewer)


rawDF <- read_csv("input_data/rawDF.csv") 
selectDF <- rawDF[, c("country", "year", "co2")]
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")


# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  # Sidebar panel for inputs ----
  absolutePanel(top = 10, right = 10,
    # Input: Slider for the number of bins ----
    sliderInput(inputId = "year",
                label = "Year:",
                min = 1850,
                max = 2019,
                value = 2000)
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  #year <- reactive({input$year})
  yearDF = reactive({selectDF %>% filter(year == input$year)})
  joinDF <- left_join(data.frame(Name = WorldCountry$name), selectDF, by = c("Name" ="country"))
  pal <- colorBin("magma", domain = joinDF$co2)
  myLabels <- paste("<strong>", joinDF$Name, "</strong>", "<br/>", 
                    "CO2:", joinDF$co2)
  
  
  output$map <- renderLeaflet({.
    leaflet(WorldCountry) %>% 
    addTiles() 
  })
  
  observe({
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = yearDF,
        fillColor = pal(joinDF$co2),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7, 
        highlight = highlightOptions(
          weight = 3,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE), 
        label = lapply(myLabels, HTML)) %>% 
      addLegend(pal = pal, values = joinDF$co2,
                title = "CO2", position = "bottomright")
  })
  
}


shinyApp(ui = ui, server = server)
