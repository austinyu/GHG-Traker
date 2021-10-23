library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(tidyverse) 
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(htmltools) 

rawDF <- read_csv("/Users/chenxingliu/Desktop/FAll\ 2021/STA-230/Rshiny/STA230_RShiny/input_data/rawDF.csv")
selectDF <- rawDF[, c("country", "year", "co2", "co2_per_capita", "co2_per_gdp",
                      "total_ghg", "ghg_per_capita", "ghg_per_gdp",
                      "methane", "methane_per_capita", "methane_per_gdp",
                      "nitrous_oxide", 'nitrous_oxide_per_capita', "nitrous_oxide_per_gdp")]
catOfDF <- vector(mode="list", length=4)
line_chart <- function(catagory){
  ggplot(data = selectDF,
         mapping = aes(x = year, y = catagory)) +
    labs(title = "The Emission of ", xlab = "year", ylab = catagory)+
    geom_line() 
}
line_chart(selectDF$co2)
line_chart(selectDF$co2_per_capita)

