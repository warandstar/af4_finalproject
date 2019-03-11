library("shiny")
library("dplyr")
library("ggplot2")
library("maps")
library("tidyr")
library("mapproj")
library("RColorBrewer")
#install.packages('devtools')

#devtools::install_github("UrbanInstitute/urbnmapr")

source("./our_ui.R")

options(scipen=999)

source("./project.R")

# Define server logic for random distribution app ----
our_server <- function(input, output) {
  
  other_city_house_data <- reactive({
    data <- get_metropolitan_house_data(input$city)
  })
  
  other_city_rent_data <- reactive({
    data <- get_metropolitan_rent_data(input$city)
    data
  })
  
  
  
  output$three_plot <- renderPlot({
    
    if(input$analysis_var == "Both" && input$data_type == "All States") {
    both_plot <-  ggplot(data = state_summary, na.rm = F) +
      geom_jitter(
        mapping = aes(x= House_Price, y= Rental_Price, color = State), # thinly stroked
        size = 3
      ) + 
      # Add title and axis labels
      labs(
        title = "State Specific House Price Vs Rental Rates (2019)", # map title
        x = "House Rates (in dollars)", # x-axis label
        y = "Rental Rates (in dollars)" #  # y-axis label 
      ) 
     both_plot
    } else if (input$analysis_var == "House Price" && input$data_type == "State-specific") {
     
     # A bar chart of the total population of each state
     # The `state` is mapped to the x-axis, and the `poptotal` is mapped
     # to the y-axis
    house_plot <- ggplot(data = house_price_summary) +
       geom_col(
         mapping = aes(x = State, y = House_Price), 
         size = 2
         ) +
                  # Add title and axis labels
                  labs(
                    title = "Compare last 5 years (2015-2019) House Price in U.S. States", # map title
                    y = "House Price (in dollars)", # x-axis label
                    x = "State Abbreviations" #  # y-axis label      
                ) 
    house_plot
    } else {
      rent_plot <- ggplot(data = rent_price_summary, na.rm = T) +
        geom_col(
          mapping = aes(x = State, y = Rental_Price), 
          size = 2
        ) +
        # Add title and axis labels
        labs(
          title = "Compare last 5 years (2015-2019) Rental Price in U.S. States", # map title
          y = "Rental Price (in dollars)", # x-axis label
          x = "State Abbreviations" #  # y-axis label      
        ) 
      rent_plot
    }
  })

  
  output$other_city_plot <- renderPlot({
  })
}
  
