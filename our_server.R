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
  }
    
})


output$three_plot <- renderPlot({
  
  if(input$analysis_var == "Both" && input$data_type == "All States") {
  both_plot <-  ggplot(data = state_summary, na.rm = ) +
    geom_jitter(
      mapping = aes(x = House_Price, y = Rental_Price, color = State), # thinly stroked
      size = 3
    ) + 
    # Add title and axis labels
    labs(
      title = "State Specific Difference in House & Rental Rates", # map title
      x = "State Abbreviations", # x-axis label
      y = "Difference in House & Rental Rates (in dollars)" #  # y-axis label 
    ) 
   both_plot
  } else if (input$analysis_var == "House Price") {
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

  output$us_summary <- renderText({
    if(input$analysis_var == "House Price") {
      paste0("This visualization demonstrates the rates of ", input$var_type, " (in dollars) for almost each state in U.S. by deploying the 'House Price (in dollars)' in 
              Y-axis and 'State Abbraviations' in X-axis. Critical Question: How does the one time House Price Rates differs from State-to-State in U.S. in most recent years? 
              Analysis: The one time House Price Rate differs from State-to-State in U.S. in most recent years in an approximate range from $100000 to $600000 as clearly conveyed by our plot.")
      
    } else if(input$var_type == "Rental Price") {
      paste0("This visualization demonstrates the rates of ", input$var_type, " (in dollars) for almost each state in U.S. by deploying the 'Rental Price (in dollars)' in 
              Y-axis and 'State Abbraviations' in X-axis. Critical Question: How does the one time Rental Price Rates differs from State-to-State in U.S. in most recent years? 
              Analysis: The annual Rental Price Rate differs from State-to-State in U.S. in most recent years in an approximate range from $12000 to $48000 as clearly conveyed by our plot.")
      
    } else {
      paste0("This visualization demonstrates the difference between the rates of ", input$var_type, " (in dollars) for almost each state in U.S. by deploying the 
          'Difference between House & Rental Rates (in dollars)' in Y-axis and 'State Abbraviations' in X-axis.Critical Question: How does the one time Rental Price Rates differs from annual Rental Price from State-to-State in U.S. in most recent years? 
              Analysis: The difference between the one time House Price and the annual Rental Price Rate differs from State-to-State in U.S. in most recent years in an approximate range from $10000 to $70000 as clearly conveyed by our plot. But a point to be 
              to be noted is that House is an one time invesment. So for example of state CA (California), the one time house price of about $650000 is profitable for life time living than paying rent every year of about $30000.")
      
    }
    
  })

}
  
