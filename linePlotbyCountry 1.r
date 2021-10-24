library(ggplot2)
library(tidyverse) 

co2_year_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & co2!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = co2, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("co2 emission") +  xlab("Year") + theme_bw() + 
    #    scale_colour_manual(values=c(covid_col)) +
    #   scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

co2_per_capita_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & co2_per_capita!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = co2_per_capita, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("co2_per_capita") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

ch4_year_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & methane!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = methane, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("methane emission") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

ch4_per_capita_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & methane_per_capita!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = methane_per_capita, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("methane_per_capita") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

no2_year_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & nitrous_oxide!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = nitrous_oxide, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("no2 emission") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

no2_per_capita_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & nitrous_oxide_per_capita!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = nitrous_oxide_per_capita, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("no2_per_capita") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

total_ghg_plot<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & total_ghg!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = total_ghg, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("total greenhouse gas emission") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

total_ghg_per_capita<-function(input, coun, year1, year2) {
  plot_df = subset(input, year>=year1 & year<=year2 & ghg_per_capita!=0 & country == coun)
  g1 = ggplot(plot_df, aes(x = year, y = ghg_per_capita, color = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("total_greenhouse_gas_per_capita") +  xlab("Year") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

