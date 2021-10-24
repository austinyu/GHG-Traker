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

### mapping functions ###

# comparing graph
comparison <- function(which_year){
  df <- data.frame(type = c("ghg", "methane", "nitrous_oxide"),
                   emission = c(sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE),
                                sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE),
                                sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
  graph = ggplot(data = df,
                 mapping = aes(x = emission, y = type)) + 
    geom_col(alpha = 0.8, stat="identity") + 
    labs(title = "The Bar Plot Comparison of GHG, Methane, and Nitrous Oxide",
         x = "Amount of Emission",
         y = "Type")
  
  graph
}

#
line_chart <- function(type_of_ghg = c("total_ghg", "co2", "methane", "nitrous_oxide"), type_of_map = c("Total Emission", "Per Capita", "Per GDP")){
  if(type_of_ghg == "total_ghg" & type_of_map == "Total Emission"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = sum(selectDF$total_ghg, na.rm = TRUE))) + 
      labs(title = "The Emission of GHG in Total", x = "year", y = "GHG in Total")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "co2" & type_of_map == "Total Emission"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$co2)) + 
      labs(title = "The Emission of CO2 in Total", x = "year", y = "CO2 in Total")+
      geom_col()
    graph
  }
  if(type_of_ghg == "methane" & type_of_map == "Total Emission"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$methane)) + 
      labs(title = "The Emission of Methane in Total", x = "year", y = "Methane in Total")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "nitrous_oxide" & type_of_map == "Total Emission"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$nitrous_oxide)) + 
      labs(title = "The Emission of Nitrous Oxide in Total", x = "year", y = "Nitrous Oxide in Total")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "total_ghg" & type_of_map == "Per Capita"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$ghg_per_capita)) + 
      labs(title = "The Emission of GHG in Total", x = "year", y = "GHG per Capita")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "co2" & type_of_map == "Per Capita"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$co2_per_capita)) + 
      labs(title = "The Emission of CO2 Per Capita", x = "year", y = "CO2 per Capita")+
      geom_col()
    graph
  }
  if(type_of_ghg == "methane" & type_of_map == "Per Capita"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$methane_per_capita)) + 
      labs(title = "The Emission of Methane Per Capita", x = "year", y = "Methane Per Capita")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "nitrous_oxide" & type_of_map == "Per Capita"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$nitrous_oxide_per_capita)) + 
      labs(title = "The Emission of Nitrous Oxide Per Capita", x = "year", y = "Nitrous Oxide Per Capita")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "total_ghg" & type_of_map == "Per GDP"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$ghg_per_gdp)) + 
      labs(title = "The Emission of GHG Per GDP", x = "year", y = "GHG Per GDP")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "co2" & type_of_map == "Per GDP"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$co2_per_gdp)) + 
      labs(title = "The Emission of CO2 Per GDP", x = "year", y = "CO2 Per GDP")+
      geom_col()
    graph
  }
  if(type_of_ghg == "methane" & type_of_map == "Per GDP"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$methane_per_gdp)) + 
      labs(title = "The Emission of Methane Per GDP", x = "year", y = "Methane Per GDP")+
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
  if(type_of_ghg == "nitrous_oxide" & type_of_map == "Per GDP"){
    graph = ggplot(data = selectDF,
                   mapping = aes(x = year, y = selectDF$nitrous_oxide_per_gdp)) + 
      labs(title = "Emission of Nitrous Oxide Per GDP", x = "year", y = "Nitrous Oxide Per GDP") +
      xlim(1990, 2019) + 
      geom_col()
    graph
  }
}

ui <- fluidPage(
  navbarPage(
    title = "Visualizing Greehouse Gas Emission",
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    tabPanel("The Graph",
             sidebarPanel(position = "left",
                          selectInput("variable", "Select a type of GHG:", 
                                      c("Total GHG" = "total_ghg",
                                        "CO2" = "co2",
                                        "Methane" = "methane",
                                        "Nitrous Oxide" = "nitrous_oxide", selected = "co2")),
                          selectInput("kind", "Select a Map:",
                                      c("Total Emission" = "Total Emission",
                                        "Per Capita" = "Per Capita",
                                        "Per GDP" = 'Per GDP')),
                          plotOutput("line_chart"))
    ),
    tabPanel("Comparison",
             sliderInput("year", "Select a Year to Compare:",
                         min = 1990, max = max(selectDF$year),
                         value = 1990),
             plotOutput("comparison"),
    )
  )
)

server <- function(input, output) {
  
  output$line_chart <- renderPlot({
    line_chart(input$variable, input$kind)
  })
  output$comparison <- renderPlot({
    comparison(input$year)
  })
}

shinyApp(ui, server)

