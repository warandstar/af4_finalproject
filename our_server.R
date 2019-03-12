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
  
  national_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_national_data[, c("year", input$var_type)]
    } else {
      data <- rent_national_data[, c("year", input$var_type)]
    }
    data
  })
  
  
  seattle_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_seattle_data[, c("year", input$var_type)]
    } else {
      data <- rent_seattle_data[, c("year", input$var_type)]
    }
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
  
  other_city_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- get_metropolitan_house_data(input$city)
      data <- data[, c("year", input$var_type)]
    } else {
      data <- get_metropolitan_rent_data(input$city)
      data <- data[, c("year", input$var_type)]
    }
    data
    
  })
  
  washington_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_washington_data[, c("year", input$var_type)]
    } else {
      data <- rent_washington_data[, c("year", input$var_type)]
    }
    data
  })
  
  
  # Creating plots for housing/rental and rate/percentage
  output$us_plot <- renderPlot({
    rates <- ggplot(data = national_data_reactive(), na.rm = T) +
      geom_line(
        mapping = aes_string(x = "year", y = input$var_type), 
        size = 2
      ) +
      geom_line(data = seattle_data_reactive(), na.rm = T,
                mapping = aes_string(x = "year", y = input$var_type),
                size = 2
      ) +
      labs(title = "Seattle Housing Rates Compared to National Housing Rates",
           x = "Year",
           y = "Housing Rate")
    rates
  })
  
  # Tab2 - summaries of data on House Listing & Monthly Rent in Seattle
  output$seattle_summary <- renderPrint({
    if (input$data_type == "House") {
      summary(seattle_data_reactive())
    } else {
      summary(seattle_data_reactive())
    }
  })
  
  # Tab2 - plot
  # returns line plots between which the red plot represents data of house listings
  # in Seattle in terms of either Rate or Percent_Change depending on user's input
  # and the blue plot represents data of rent in Seattle also in in terms of either 
  # Rate or percent_change depending on user's input. 
  output$seattle_plot <- renderPlot({
    p <- ggplot(data = house_seattle_data_reactive(), na.rm = TRUE) +
      geom_line(mapping = aes_string(x = "year", y = input$var_type), 
                color = "red",
                size = 2) + 
      # second line in the same plot 
      # represents how rate change over time in Seattle  
      geom_line(data = rent_seattle_data_reactive(), na.rm = TRUE,
                mapping = aes_string(x = "year", y = input$var_type), 
                color = "blue") + 
      labs(
        title = paste0("Seattle Regional", input$var_type, "Change Over Time for House and Rent"),
        x = "month",
        y = input$var_type,
        color = "Changes"
      ) 
    p
  }) #two_plot ends here
  
  # Tab2 - Table 
  # returns two tables 
  output$seattle_table <- renderTable({
    if (input$data_type == "House") {
      house_seattle_data_reactive
    } else if (input$var_type == "Rent") {
      rent_seattle_data_reactive
    } 
  }) # two_table ends here
  
  
  # Tab 3 - Creating plots for seattle/wa and rate/percentage
  output$washington_plot <- renderPlot({
    rates <- ggplot(data = seattle_data_reactive(), na.rm = TRUE) + # basic graphical object
      geom_line(aes_string(x = "year", y = input$var_type), colour="black") +  # first layer
      geom_line(data = washington_data_reactive(), na.rm = TRUE, aes_string(x = "year", y = input$var_type), colour="red") +  # second layer
      labs(title = "Seattle Housing Rates Compared to Washington State Housing Rates",
           x = "Year",
           y = "Housing Rate (Price in Dollars)")
    rates
  })
  
  View(seattle_data_reactive)
  
  
  
  # Tab 4
  output$other_city_plot <- renderPlot({
    rates <- ggplot(data = seattle_data_reactive(), na.rm = T) +
      geom_line(
        mapping = aes_string(x = "year", y = input$var_type), 
        size = 2
      ) +
      geom_line(data = other_city_data_reactive(), na.rm = T,
                mapping = aes_string(x = "year", y = input$var_type),
                size = 2
      ) +
      labs(title = "Seattle Rates Compared to Other Cities",
           x = "Year",
           y = "Housing Rate")
    rates
  })
  
  # Construct a function that returns a color based on the data
  # Colors are taken from the ColorBrewer Set3 palette
  if (input$data_type == "House") {
    palette_fn <- colorFactor(palette = "Set3", domain = house_seattle_data_reactive())
  } else {
    palette_fn <- colorFactor(palette = "Set3", domain = rent_seattle_data_reactive()) 
  }
  
  # Map
  output$map <- renderLeaflet ({
    # Create a Leaflet map of new building construction by category
    if (input$data_type == "House") {
      leaflet(data =  house_seattle_data_reactive()) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -122.3321, lat = 47.6062, zoom = 10) %>%
        addCircles(
          lat = -122.3321, # specify the column for `lat` as a formula
          lng = 47.6062, # specify the column for `lng` as a formula
          stroke = FALSE, # remove border from each circle 
          color = ~palette_fn(input$var_type), # a "function of" the palette mapping
          radius = 20,
          fillOpacity = 0.5
        ) %>%
        addLegend(
          position = "bottomright",
          title = paste0(input$var_type, "of House Price in Seattle"),
          pal = palette_fn, # the palette to label
          values = ~input$var_type, # the values to label
          opacity = 1
        ) } else {
          leaflet(data =  rent_seattle_data_reactive()) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = -122.3321, lat = 47.6062, zoom = 10) %>%
            addCircles(
              lat = -122.3321, # specify the column for `lat` as a formula
              lng = 47.6062, # specify the column for `lng` as a formula
              stroke = FALSE, # remove border from each circle
              color = ~palette_fn(input$var_type), # palette mapping
              radius = 20,
              fillOpacity = 0.5
            ) %>%
            addLegend(
              position = "bottomright",
              title = paste0(input$var_type, "of Rental Price in Seattle"),
              pal = palette_fn, # the palette to label
              values = ~input$var_type, # the values to label
              opacity = 1
            )
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
  
  
  
}