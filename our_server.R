library("shiny")
library("dplyr")
library("ggplot2")
library("maps")
library("tidyr")
library("leaflet")
library("RColorBrewer")

source("./our_ui.R")

options(scipen = 999)

source("./project.R")

# Define server logic for random distribution app ----
our_server <- function(input, output, session) {

  # Those observe command will take care of same sidebars in the different tabs
  observe({
    updated_var_type <- input$var_type
    updateRadioButtons(session, "var_type2", selected = updated_var_type)
    updateRadioButtons(session, "var_type3", selected = updated_var_type)
    updateRadioButtons(session, "var_type4", selected = updated_var_type)
    updateRadioButtons(session, "var_type5", selected = updated_var_type)
  })
  
  observe({
    updated_var_type <- input$var_type2
    updateRadioButtons(session, "var_type", selected = updated_var_type)
    updateRadioButtons(session, "var_type3", selected = updated_var_type)
    updateRadioButtons(session, "var_type4", selected = updated_var_type)
    updateRadioButtons(session, "var_type5", selected = updated_var_type)
  })
  
  observe({
    updated_var_type <- input$var_type3
    updateRadioButtons(session, "var_type2", selected = updated_var_type)
    updateRadioButtons(session, "var_type", selected = updated_var_type)
    updateRadioButtons(session, "var_type4", selected = updated_var_type)
    updateRadioButtons(session, "var_type5", selected = updated_var_type)
  })
  
  observe({
    updated_var_type <- input$var_type4
    updateRadioButtons(session, "var_type2", selected = updated_var_type)
    updateRadioButtons(session, "var_type3", selected = updated_var_type)
    updateRadioButtons(session, "var_type", selected = updated_var_type)
    updateRadioButtons(session, "var_type5", selected = updated_var_type)
  })
  observe({
    updated_var_type <- input$var_type5
    updateRadioButtons(session, "var_type2", selected = updated_var_type)
    updateRadioButtons(session, "var_type3", selected = updated_var_type)
    updateRadioButtons(session, "var_type4", selected = updated_var_type)
    updateRadioButtons(session, "var_type", selected = updated_var_type)
  })
  
  observe({
    updated_data_type <- input$data_type
    updateRadioButtons(session, "data_type3", selected = updated_data_type)
    updateRadioButtons(session, "data_type4", selected = updated_data_type)
    updateRadioButtons(session, "data_type5", selected = updated_data_type)
  })
  
  observe({
    updated_data_type <- input$data_type3
    updateRadioButtons(session, "data_type", selected = updated_data_type)
    updateRadioButtons(session, "data_type4", selected = updated_data_type)
    updateRadioButtons(session, "data_type5", selected = updated_data_type)
  })
  
  observe({
    updated_data_type <- input$data_type4
    updateRadioButtons(session, "data_type3", selected = updated_data_type)
    updateRadioButtons(session, "data_type", selected = updated_data_type)
    updateRadioButtons(session, "data_type5", selected = updated_data_type)
  })
  
  observe({
    updated_data_type <- input$data_type5
    updateRadioButtons(session, "data_type3", selected = updated_data_type)
    updateRadioButtons(session, "data_type4", selected = updated_data_type)
    updateRadioButtons(session, "data_type", selected = updated_data_type)
  })
  
  # The reactive data of national average data of either price or percentage of either house or rent
  national_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_national_data[, c("year", input$var_type)]
    } else {
      data <- rent_national_data[, c("year", input$var_type)]
    }
  })
  
  # The reactive data of seattle's average data of either price or percentage of either house or rent
  seattle_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_seattle_data[, c("year", input$var_type)]
    } else {
      data <- rent_seattle_data[, c("year", input$var_type)]
    }
    data
    
  })

  # The reactive data of seattle's average data of either price or percentage of house
  house_seattle_data_reactive <- reactive({
    data <- 0
    data <- house_seattle_data[, c("year", input$var_type)]
    data
  })

  # The reactive data of seattle's average data of either price or percentage of rent
  rent_seattle_data_reactive <- reactive({
    data <- 0
    data <- rent_seattle_data[, c("year", input$var_type)]
    data
  })
  
  # The reactive data of other city's average data of either price or percentage of either house or rent
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
  
  # The reactive data of non-seattle washington's average data of either price or percentage of either house or rent
  washington_data_reactive <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_washington_data[, c("year", input$var_type)]
    } else {
      data <- rent_washington_data[, c("year", input$var_type)]
    }
    data
  })
  
  # The reactive data of seattle's data of either price or percentage of either house or rent for each ZIP codes
  
  seattle_individual <- reactive({
    data <- 0
    if (input$data_type == "House") {
      data <- house_seattle_individual[, c("year", "RegionName", "city", input$var_type)]
    } else {
      data <- rent_seattle_individual[, c("year", "RegionName", "city", input$var_type)]
    }
    
    data <- data %>%
      filter(year == input$year)
    data
  })
  
  
  # Creating plots for housing/rental and rate/percentage
  output$us_plot <- renderPlot({
      rates <- ggplot(data = national_data_reactive(), na.rm = TRUE) +
        geom_line(
          mapping = aes_string(x = "year", y = input$var_type, group = 1), 
          size = 2,
          color = "black"
        ) +
        geom_line(
          data = seattle_data_reactive(), na.rm = TRUE,
          mapping = aes_string(x = "year", y = input$var_type, group = 1),
          size = 2,
          color = "red"
        ) +
      labs(title = paste0("Seattle ", input$data_type, " ", input$var_type, " Compared to National ", input$data_type, " ", input$var_type),
           x = "Year",
           y = paste0(input$data_type, " ", input$var_type))
    rates
  })
  
  
  output$us_summary <- renderText({
    paste0("This visualization demonstrates ", input$var_type, " of prices over the years Seattle and National Level. ", 
           "Thus, the answer to this question, we can clearly see that the rapid increase from 2012 to 2017 in ",
           "Seattle compared to national level.")
      
  })
  
  
  
  # Tab2 - plot
  # returns line plots between which the red plot represents data of house listings
  # in Seattle in terms of either Rate or Percent_Change depending on user's input
  # and the blue plot represents data of rent in Seattle also in in terms of either 
  # Rate or percent_change depending on user's input. 
  output$seattle_plot <- renderPlot({
    p <- ggplot(data = house_seattle_data_reactive(), na.rm = TRUE) +
      geom_line(mapping = aes_string(x = "year", y = input$var_type, group = 1), 
                color = "red",
                size = 2) + 
      # second line in the same plot 
      # represents how rate change over time in Seattle  
      geom_line(data = rent_seattle_data_reactive(), na.rm = TRUE,
                mapping = aes_string(x = "year", y = input$var_type, group = 1), 
                color = "blue", 
                size = 2) + 
      labs(
        title = paste0("Seattle Regional ", input$var_type, " Change Over Time for House and Rent "),
        x = "Year",
        y = paste0(input$data_type, " ", input$var_type)
      ) 
    p
  }) #two_plot ends here
  
  # Tab2 - summaries of data on House Listing & Monthly Rent in Seattle
  output$seattle_summary <- renderText({
    paste0("This visualization portrays the ", input$var_type, " of the prices of both houses and rents.", 
           "The result shows that the Seattle is experiencing sharp increase of house and rents in 2012 to 2017.")
  })
  
  # Tab 3 - Creating plots for seattle/wa and rate/percentage
  output$washington_plot <- renderPlot({
    rates <- ggplot(data = seattle_data_reactive(), na.rm = T) +
      geom_line(
        mapping = aes_string(x = "year", y = input$var_type, group = 1), 
        size = 2,
        color = "black"
      ) +
      geom_line(data = washington_data_reactive(), na.rm = T,
                mapping = aes_string(x = "year", y = input$var_type),
                size = 2,
                color = "red"
      ) +
      labs(title = paste0("Seattle ", input$data_type, " ", input$var_type, " Compared to WA State cities' ", input$data_type, " ", input$var_type),
           x = "Year",
           y = paste0(input$data_type, " ", input$var_type))
    rates
  })
  
  output$washington_summary <- renderText({
    paste0("This visualization portrays the ", input$var_type, "of prices of", input$data_type, 
           "in Seattle area and Washington state outside of Seattle area. ", 
           "Thus, The data shows that the trends of sharp increase from 2012 and 2017 is true but ", 
           "the Seattle area has more rapid increase compared to other Washington State cities")
  })
  
  # Tab 4
  output$other_city_plot <- renderPlot({
    rates <- ggplot(data = seattle_data_reactive(), na.rm = T) +
      geom_line(
        mapping = aes_string(x = "year", y = input$var_type), 
        size = 2,
        color = "black"
      ) +
      geom_line(data = other_city_data_reactive(), na.rm = T,
                mapping = aes_string(x = "year", y = input$var_type),
                size = 2,
                color = "red"
      ) +
      labs(title = paste0("Seattle ", input$data_type, " ", input$var_type, " Compared to some other U.S. cities' ", input$data_type, " ", input$var_type),
           x = "Year",
           y = paste0(input$data_type, " ", input$var_type))
    rates
  })
  
  output$other_city_summary <- renderText({
    paste0("This visualization shows the ", input$var_type, " of the prices of ", input$data_type, " in ",
           "Seattle Area and ", input$city, " Area. ", 
           "Overall, the result shows that Seattle is one of cities with sharp increase in prices!")
  })
  
  # Construct a function that returns a color based on the data
  # Colors are taken from the ColorBrewer Set3 palette

  
  # Map
  output$map <- renderLeaflet ({
    palette_fn <- colorFactor(palette = "Set3", domain = seattle_individual())
    
    # Create a Leaflet map of new building construction by category
      leaflet(data =  seattle_individual()) %>%
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
          title = paste(input$var_type, "of", input$data_type, "Price in Seattle"),
          pal = palette_fn, # the palette to label
          values = ~input$var_type, # the values to label
          opacity = 1
        ) 
  })
  
  
  
  
  
}