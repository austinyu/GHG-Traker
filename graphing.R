library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(tidyverse) 
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(htmltools) 

rawDF <- read_csv("input_data/rawDF.csv")
selectDF <- rawDF[, c("country", "year", "co2", "co2_per_capita", "co2_per_gdp",
                      "total_ghg", "ghg_per_capita", "ghg_per_gdp",
                      "methane", "methane_per_capita", "methane_per_gdp",
                      "nitrous_oxide", 'nitrous_oxide_per_capita', "nitrous_oxide_per_gdp")]
catOfDF <- vector(mode="list", length=4)

### mapping functions ###

# comparing graph
comparison <- function(which_year, checker = list("total_ghg", "methane", "nitrous_oxide")){
  df <- data.frame(type = 0,
                   emission = 0)
  df <- df[-1,]
  
  if("ghg" %in% checker && !("methane" %in% checker) && !("nitrous_oxide" %in% checker)){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
  }
  else if(!("ghg" %in% checker) && "methane" %in% checker && !("nitrous_oxide" %in% checker)){
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
  }
  else if(!("ghg" %in% checker) && !("methane" %in% checker) && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
  }
  
  else if("ghg" %in% checker && "methane" %in% checker && !("nitrous_oxide" %in% checker)){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
  }
  else if(!("ghg" %in% checker) && "methane" %in% checker && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
  }
  else if("ghg" %in% checker && !("methane" %in% checker) && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
  }
  else if("ghg" %in% checker && "methane" %in% checker && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
  }
  graph = ggplot(data = df,
                 mapping = aes(x = df[ , 1], y = df[ , 2])) + 
    geom_col(alpha = 0.8, stat="identity") + 
    labs(title = "The Bar Plot Comparison",
         y = "Amount of Emission",
         x = "Type")
  
  graph
}

ui <- fluidPage(
  navbarPage(
    title = "Visualizing Greehouse Gas Emission",
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    tabPanel("Comparison",
             sliderInput("year", "Select a Year to Compare:",
                         min = 1990, max = max(selectDF$year),
                         value = 1990),
             checkboxGroupInput("checkGroup", 
                                h3("Choose Emission Types to Compare"), 
                                choices = list("GHG" = "ghg", 
                                               "Methane" = "methane", 
                                               "Nitrous Oxide" = "nitrous_oxide"),
                                selected = list("ghg", "methane", "nitrous_oxide")),
             plotOutput("comparison")
    )
  )
)

#111111111

server <- function(input, output) {

  output$comparison <- renderPlot({
    comparison(input$year, input$checkGroup)
  })
}

shinyApp(ui, server)

