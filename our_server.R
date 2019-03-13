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

  ## Observe
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

  # Tab 1
  # Creating plots for housing/rental and rate/percentage of Seattle and National level
  # housing/rental and rate/percentage are chosen by users by interactive sidebars.
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
  
  # summary of the plot on Seattle and National Data
  output$us_summary <- renderText({
    if(input$data_type == "House") {
      data_seattle <- house_seattle_data
      data_nation <- house_national_data
    } else {
      data_seattle <- rent_seattle_data
      data_nation <- rent_national_data
    }
    
    seattle_2012 <- data_seattle %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    seattle_2018 <- data_seattle %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()

    nation_2012 <- data_nation %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    nation_2018 <- data_nation %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
   
    seattle_2012 <- round(seattle_2012, 2)
    seattle_2018 <- round(seattle_2018, 2)
    
    nation_2012 <- round(nation_2012, 2)
    nation_2018 <- round(nation_2018, 2)
    
    change_seattle <- round((log(seattle_2018) - log(seattle_2012)) * 100, 2)
    change_nation <- round((log(nation_2018) - log(nation_2012)) * 100, 2)
    
    paste0("This visualization demonstrates ", input$var_type, " of prices over the years Seattle and National Level. ", 
           "Thus, the answer to this question of seattle increasing more rapidly than national, ",
           "we can clearly see that the rapid increase from 2012 to 2018 Seattle compared to national level. ",
           "For instance, from 2012 to 2018, for price of ", input$data_type, " in Seattle increased from $", seattle_2012, 
           " to $", seattle_2018, ", which is ", change_seattle, "%, meanwhile as nationwide increased from $", 
           nation_2012, " to $", nation_2018, ", which is ", change_nation, "%")
      
  })
  
  
  
  # Tab2 - plot
  # returns line plots between which the red plot represents data of house listings
  # in Seattle in terms of either Rate or Percent_Change depending on user's input
  # and the blue plot represents data of rent in Seattle also in in terms of either 
  # Rate or percent_change depending on user's input. 
  output$seattle_plot <- renderPlot({
    rates <- ggplot(data = house_seattle_data_reactive(), na.rm = TRUE) +
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
    rates
  })
  
  # Tab2 - summaries of data on House Listing & Monthly Rent in Seattle
  output$seattle_summary <- renderText({
    data_rent <- rent_seattle_data
    data_house <- house_seattle_data
    
    house_2012 <- data_house %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    house_2018 <- data_house %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
    house_2012 <- round(house_2012, 2)
    house_2018 <- round(house_2018, 2)
    
    change_house <- round((log(house_2018) - log(house_2012)) * 100, 2)
    
    rent_2012 <- data_rent %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    rent_2018 <- data_rent %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
    rent_2012 <- round(rent_2012, 2)
    rent_2018 <- round(rent_2018, 2)
    
    change_rent <- round((log(rent_2018) - log(rent_2012)) * 100, 2)
    
    paste0("This visualization portrays the ", input$var_type, " of the prices of both houses and rents.", 
           "The result shows that the Seattle is experiencing sharp increase of house and rents in 2012 to 2018.",
           "For instance, from 2012 to 2018, for price of house in Seattle increased from $", house_2012, 
           " to $", house_2018, ", which is ", change_house, "%, meanwhile the price of rent increased from $", 
           rent_2012, " to $", rent_2018, ", which is ", change_rent, "%")
  })
  
  # Tab 3 - Creating plots for rate/percentage of housing/rental for Seattle and Washington outside of Seattle
  # Housing/rental and Rate/Percentage can be switched by users by using interactive sidebars
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
  
  # create the summary of plot of washington and seattle data
  output$washington_summary <- renderText({
    if(input$data_type == "House") {
      data_seattle <- house_seattle_data
      data_washington <- house_washington_data
    } else {
      data_seattle <- rent_seattle_data
      data_washington <- rent_washington_data
    }
    seattle_2012 <- data_seattle %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    seattle_2018 <- data_seattle %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
    seattle_2012 <- round(seattle_2012, 2)
    seattle_2018 <- round(seattle_2018, 2)
    
    change_seattle <- round((log(seattle_2018) - log(seattle_2012)) * 100, 2)
    
    washington_2012 <- data_washington %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    washington_2018 <- data_washington %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
    washington_2012 <- round(washington_2012, 2)
    washington_2018 <- round(washington_2018, 2)
    
    change_washington <- round((log(washington_2018) - log(washington_2012)) * 100, 2)
    
    paste0("This visualization portrays the ", input$var_type, "of prices of", input$data_type, 
           "in Seattle area and Washington state outside of Seattle area. ", 
           "As seen above, the data shows that the trends of sharp increase from 2012 and 2017 is true but ", 
           "the Seattle area has more rapid increase compared to other Washington State cities",
           "For instance, from 2012 to 2018, for price of ", input$data_type, " in Seattle increased from $", seattle_2012, 
           " to $", seattle_2018, ", which is ", change_seattle, "%, meanwhile in washington state outside of Seattle area",
           "increased from $", washington_2012, " to $", washington_2018, ", which is ", change_washington, "%")
  })
  
  # Tab 4
  # create the plots of rates/percentage of house/rents of Seattle and other cities
  # rates/percentage and house/rents can be switched based on users' choice
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
  
  # create the summary of plot of seattle and other city data
  output$other_city_summary <- renderText({
    if(input$data_type == "House") {
      data_seattle <- house_seattle_data
      data_other_city <- get_metropolitan_house_data(input$city)
    } else {
      data_seattle <- rent_seattle_data
      data_other_city <- get_metropolitan_rent_data(input$city)
    }
    
    seattle_2012 <- data_seattle %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    seattle_2018 <- data_seattle %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
    seattle_2012 <- round(seattle_2012, 2)
    seattle_2018 <- round(seattle_2018, 2)
    
    change_seattle <- round((log(seattle_2018) - log(seattle_2012)) * 100, 2)
    
    other_city_2012 <- data_other_city %>% 
      filter(year == 2012) %>% 
      select(Rate) %>%
      pull()
    
    other_city_2018 <- data_other_city %>% 
      filter(year == 2018) %>% 
      select(Rate) %>%
      pull()
    other_city_2012 <- round(other_city_2012, 2)
    other_city_2018 <- round(other_city_2018, 2)
    
    change_other_city <- round((log(other_city_2018) - log(other_city_2012)) * 100, 2)
    
    paste0("This visualization shows the ", input$var_type, " of the prices of ", input$data_type, " in ",
           "Seattle Area and ", input$city, " Area. ", 
           "Overall, the result shows that Seattle is one of cities with sharp increase in prices!",
           "For instance, from 2012 to 2018, for price of ", input$data_type, " in Seattle increased from $", seattle_2012, 
           " to $", seattle_2018, ", which is ", change_seattle, "%, meanwhile in ", input$city, " area increased from $", 
           other_city_2012, " to $", other_city_2018, ", which is ", change_other_city, "%")
  })
  
  # Construct a function that returns a color based on the data
  # Colors are taken from the ColorBrewer Set3 palette


  
}