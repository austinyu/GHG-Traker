library(ggplot2)
library(tidyverse) 

rawDF <- read_csv("raw_data/owid-co2-data.csv") 
countrySelected <- "India"
sourceSelected <- "co2"
startYear <- 1950
endYear <- 2018
selectDF <- rawDF[, c("country", "year", sourceSelected)] %>% 
  filter(country == countrySelected) %>% 
  filter(year > startYear & year < endYear)

selectDF %>% ggplot(mapping = aes(x = year, y = co2)) +
  geom_line()


