library("shiny")
library("dplyr")
library("ggplot2")
library("maps")
library("tidyr")
#library("mapproj")
library("RColorBrewer")
#install.packages('devtools')

#devtools::install_github("UrbanInstitute/urbnmapr")

source("./our_ui.R")

options(scipen = 999)

source("./project.R")

# Define server logic for random distribution app ----
our_server <- function(input, output) {
  
  house_national_data_reactive <- reactive({
    data <- house_national_data[, c("year", input$var_type)]
    data
  })
  
  rent_national_data_reactive <- reactive({
    data <- rent_national_data[, c("year", input$var_type)]
    data
  })
  
  house_seattle_data_reactive <- reactive({
    data <- house_seattle_data[, c("year", input$var_type)]
    data
  })
  
  rent_seattle_data_reactive <- reactive({
    data <- rent_seattle_data[, c("year", input$var_type)]
    data
  })
  
  other_city_house_data_reactive <- reactive({
    print(input$var_type)
    data <- get_metropolitan_house_data(input$city)
    data <- data[, c("year", input$var_type)]
    data
  })
  
  other_city_rent_data_reactive <- reactive({
    data <- get_metropolitan_rent_data(input$city)
    data <- data[, c("year", input$var_type)]
    data
  })
  

  # Creating plots for housing/rental and rate/percentage
  output$us_plot <- renderPlot({
    if(input$data_type == "House") {
      housing_rates <- ggplot(data = house_national_data_reactive(), na.rm = T) +
        geom_line(
          mapping = aes(x = year, y = input$var_type), 
          size = 2
        ) +
        geom_line(data = house_seattle_data_reactive(), na.rm = T,
                  mapping = aes(x = year, y = input$var_type),
                  size = 2
        ) +
      labs(title = "Seattle Housing Rates Compared to National Housing Rates",
           x = "Year",
           y = "Housing Rate")
      housing_rates
    } else {
      rental_rates <- ggplot(data = rent_national_data_reactive(), na.rm = T) +
        geom_line(
          mapping = aes(x = year, y = input$var_type), 
          size = 2
        ) +
        geom_line(data = rent_seattle_data_reactive(), na.rm = T,
                  mapping = aes(x = year, y = input$var_type),
                  size = 2
        ) +
        labs(title = "Seattle Rental Rates Compared to National Housing Rates",
             x = "Year",
             y = "Rental Rate")
       rental_rates
    } 
  })

  output$other_city_plot <- renderPlot({
    if(input$data_type == "House") {
      housing_rates <- ggplot(data = house_seattle_data_reactive(), na.rm = T) +
        geom_line(
          mapping = aes(x = year, y = input$var_type), 
          size = 2
        ) +
        geom_line(data = other_city_house_data_reactive(), na.rm = T,
                  mapping = aes(x = year, y = input$var_type),
                  size = 2
        ) +
        labs(title = "Seattle Housing Rates Compared to National Housing Rates",
             x = "Year",
             y = "Housing Rate (Price in Dollars)")
      housing_rates
    } else {
      rental_rates <- ggplot(data = house_seattle_data_reactive(), na.rm = T) +
        geom_line(
          mapping = aes(x = year, y = input$var_type), 
          size = 2
        ) +
        geom_line(data = other_city_rent_data_reactive(), na.rm = T,
                  mapping = aes(x = year, y = input$var_type),
                  size = 2
        ) +
        labs(title = "Seattle Rental Rates Compared to National Housing Rates",
             x = "Year",
             y = "Rental Rate (Price in Dollars)")
      rental_rates
    } 
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


  # Tab2 - summaries of data on House Listing & Monthly Rent in Seattle
  output$two_summary <- renderPrint({
    if (input$data_type == "House") {
    summary(house_seattle_data_reactive)
    } else {
      summary(rent_seattle_data_reactive)
    }
  })
  
  # Tab2 - plot
  # returns line plots between which the red plot represents data of house listings
  # in Seattle in terms of either Rate or Percent_Change depending on user's input
  # and the blue plot represents data of rent in Seattle also in in terms of either 
  # Rate or percent_change depending on user's input. 
  output$two_plot <- renderPlot({
    p <- ggplot(data = house_seattle_data_reactive, na.rm = TRUE) +
        geom_line(mapping = aes(x = year, y = input$var_type), 
                  color = "red",
                  size = 2) + 
        # second line in the same plot 
        # represents how rate change over time in Seattle  
        geom_line(data = rent_seattle_data_reactive, na.rm = TRUE,
                  mapping = aes(x = year, y = input$var_type), 
                  color = "blue") + 
        labs(
          title = paste0("Seattle Regional", input$var_type, "Change Over Time for House"),
          x = "month",
          y = input$var_type,
          color = "Changes"
      ) 
    p
  }) #two_plot ends here
  
  # Tab2 - Table 
  # returns two tables 
  output$two_table <- renderTable({
    if (input$data_type == "House") {
      house_seattle_data_reactive
    } else if (input$var_type == "Rent") {
      rent_seattle_data_reactive
    } 
  }) # two_table ends here
}
  
