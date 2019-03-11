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

options(scipen = 999)

source("./project.R")

# Define server logic for random distribution app ----
our_server <- function(input, output) {
  
# Creating plots for housing/rental and rate/percentage
output$us_plot <- renderPlot({
  if(input$data_type == "House" && input$var_type == "Rate") {
    housing_rates <- ggplot(data = house_national_data, na.rm = T) +
      geom_line(
        mapping = aes(x = year, y = monthly_average), 
        size = 2
      ) +
      geom_line(data = house_seattle_data, na.rm = T,
                mapping = aes(x = year, y = monthly_average),
                size = 2
      ) +
    labs(title = "Seattle Housing Rates Compared to National Housing Rates",
         x = "Year",
         y = "Housing Rate (Price in Dollars)")
  housing_rates
  } else if (input$data_type == "Rent" && input$var_type == "Rate") {
    rental_rates <- ggplot(data = rent_national_data, na.rm = T) +
      geom_line(
        mapping = aes(x = year, y = monthly_average), 
        size = 2
      ) +
      geom_line(data = rent_seattle_data, na.rm = T,
                mapping = aes(x = year, y = monthly_average),
                size = 2
      ) +
      labs(title = "Seattle Rental Rates Compared to National Housing Rates",
           x = "Year",
           y = "Rental Rate (Price in Dollars)")
    rental_rates
  } else if (input$data_type == "House" && input$var_type == "Percentage") {
    housing_percentage_change <- ggplot(data = house_national_data, na.rm = T) +
      geom_line(
        mapping = aes(x = year, y = percent_change), 
        size = 2
      ) +
      geom_line(data = house_seattle_data, na.rm = T,
                mapping = aes(x = year, y = percent_change),
                size = 2
      ) +
      labs(title = "Percentage Change of Seattle Housing Rate Compared to Percentage Change of National Housing Rates",
           x = "Year",
           y = "Percentage Change")
    housing_percentage_change
  } else {
    rental_percentage_change <- ggplot(data = rent_national_data, na.rm = T) +
      geom_line(
        mapping = aes(x = year, y = percent_change), 
        size = 2
      ) +
      geom_line(data = rent_seattle_data, na.rm = T,
                mapping = aes(x = year, y = percent_change),
                size = 2
      ) +
      labs(title = "Percentage Change of Seattle Rental Rates Compared to Percentage Change of National Rental Rates",
           x = "Year",
           y = "Percentage Change")
    rental_percentage_change
  }

})

  output$us_plot <- renderPlot({
    if(input$data_type == "House" && input$var_type == "Rate") {
      housing_rates <- ggplot(data = house_national_data, na.rm = T) +
        geom_line(
          mapping = aes(x = year, y = monthly_average), 
          size = 2
        ) +
        geom_line(data = house_seattle_data, na.rm = T,
                  mapping = aes(x = year, y = monthly_average),
                  size = 2
        ) +
      labs(title = "Seattle Housing Rates Compared to National Housing Rates",
           x = "Year",
           y = "Housing Rate (Price in Dollars)")
    }
  })
  
  other_city_house_data <- reactive({
    data <- get_metropolitan_house_data(input$city)
  })
  
  other_city_rent_data <- reactive({
    data <- get_metropolitan_rent_data(input$city)
    data
  })
  



  output$us_summary <- renderText({
    if(input$data_type == "House") {
      paste0("This visualization demonstrates the rates of ", input$var_type, " (in dollars) for almost each state in U.S. by deploying the 'House Price (in dollars)' in 
              Y-axis and 'State Abbraviations' in X-axis. Critical Question: How does the one time House Price Rates differs from State-to-State in U.S. in most recent years? 
              Analysis: The one time House Price Rate differs from State-to-State in U.S. in most recent years in an approximate range from $100000 to $600000 as clearly conveyed by our plot.")
      
    } else {
      paste0("This visualization demonstrates the rates of ", input$var_type, " (in dollars) for almost each state in U.S. by deploying the 'Rental Price (in dollars)' in 
              Y-axis and 'State Abbraviations' in X-axis. Critical Question: How does the one time Rental Price Rates differs from State-to-State in U.S. in most recent years? 
              Analysis: The annual Rental Price Rate differs from State-to-State in U.S. in most recent years in an approximate range from $12000 to $48000 as clearly conveyed by our plot.")
    }
    
  })


}
  
