if(!require(plotly)) 
  install.packages("plotly") & library(plotly)
if(!require(shiny)) 
  install.packages("shiny") & library(shiny)
if(!require(ggplot2)) 
  install.packages("ggplot2") & library(ggplot2)
if(!require(shinyWidgets))
  install.packages("shinyWidgets") & library(shinyWidgets)

# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative cases") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot cumulative sars cases by date
sars_cumulative_plot = function(sars_aggregated, sars_date) {
  plot_df = subset(sars_aggregated, date<=as.Date(sars_date, format="%Y-%m-%d"))
  ggplot(plot_df, aes(x = date, y = cases)) + geom_line(colour = sars_col) + geom_point(size = 1, alpha = 0.8, colour = sars_col) +
    ylab("Cumulative cases") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(sars_col)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    scale_y_continuous(limits=c(0,10000), labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    # geom_bar(position="stack", stat="identity") + 
    ylab("New cases (weekly)") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values = c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "yellow",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}


shinyApp(ui = ui, server = server)