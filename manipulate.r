library(leaflet)    # The map-making package
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(dplyr)      # Used for data manipulation and merging
library(htmltools)  # Used for constructing map labels using HTML
library(ggplot2)
library(tidyverse) 


rawDF <- read_csv("raw_data/owid-co2-data.csv") 

observationDF <- rawDF %>% 
  group_by(year) %>% 
  summarise(total = sum(co2))
observationDF %>%  ggplot(mapping = aes(x = year, y = total)) +
  geom_smooth()

rawDF %>% filter(year > 1900) 

maniDF <- rawDF %>% mutate(country = case_when(
  country == "United States" ~ "United States of America",
  TRUE ~ country
))

selectDF <- maniDF[, c("country", "year", "co2", "population", "gdp")]




year <- 2000
yearDF = selectDF %>% filter(year == 2000)

shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")

joinDF <- left_join(data.frame(Name = WorldCountry$name), yearDF, by = c("Name" ="country"))

## Create a color palette (using the "magma" color scheme)
pal <- colorBin("magma", domain = joinDF$co2)

myLabels <- paste("<strong>", joinDF$Name, "</strong>", "<br/>", 
                  "CO2:", joinDF$co2)

Map <- leaflet(WorldCountry) %>% addTiles() %>% 
  addPolygons(
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
Map





