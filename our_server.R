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

options(scipen=999)

source("./project.R")

# Define server logic for random distribution app ----
our_server <- function(input, output) {

#library("tidyverse")
#library("urbnmapr")
# state map data
#get_data = urbnmapr::states %>%
#select(long, lat, group, state_abbv)


#WA_data <- house_only %>%
# filter(City == "Seattle")
# group_by(City) %>% 
#  summarize(
#    price15 <- mean(X2015.01),
#    price16 <- mean(X2016.01),
#    price17 <- mean(X2017.01),
#    price18 <- mean(X2018.01),
#    price19 <- mean(X2019.01)
#  )

# house data
house_data <- house_price_data %>%
  select(RegionID, RegionName, City, State, Metro, CountyName, X2015.01, X2017.01, X2019.01)

house_only <- house_price_data %>% 
  select(State, X2015.01, X2016.01, X2017.01, X2018.01, X2019.01)

h_only <- gather(house_only, year, cost, -State)

house_price_summary <- h_only%>%
  group_by(State) %>%
  summarize(
    House_Price = mean(cost)
  )


# rent
rent_data <- rent_price_data %>%
select(RegionID, RegionName, City, State, Metro, CountyName, X2015.01, X2017.01, X2019.01)

rent_only <- rent_price_data %>% 
  select(State, X2015.01, X2016.01, X2017.01, X2018.01, X2019.01)

r_only <- gather(rent_only, year, cost, -State)

rent_price_summary <- r_only%>%
  group_by(State) %>%
  summarize(
    Rental_Price = mean(cost)
  )


# state house data
state_house_data <- house_data %>%
  select(State, X2019.01)

# state rent data
state_rent_data <- rent_data %>%
  select(State, X2019.01)

combined_house_rent <- left_join(state_house_data, state_rent_data, by = "State")

# change colnames
colnames(combined_house_rent) <- c("State", "House", "Rental")

state_summary <- combined_house_rent%>%
  group_by(State) %>%
  summarize(
    House_Price = mean(House),
    Rental_Price = mean(Rental)
  )

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

  # Tab2 - summary
  output$two_summary <- renderPrint({
    if (input$data_type == "house") {
    summary(house_seattle_data)
    } else {
      summary(rent_seattle_data)
    }
  })
  
  # Tab2 - plot
  output$two_plot <- renderPlot({
      
    p <- ggplot(data = house_seattle_data, na.rm = TRUE) +
        geom_line(mapping = aes(x = year, y = input$var_type), 
                  color = "red",
                  size = 2) + 
        labs(
          title = paste0("Seattle Regional", input$var_type, "Change Over Time for House"),
          x = "month",
          y = input$var_type,
          color = "Changes"
        ) + 
        # second line in the same plot 
        # represents how rate change over time in Seattle  
        geom_line(data = rent_seattle_data, na.rm = TRUE,
                  mapping = aes(x = year, y = input$var_type), 
                  color = "blue")
    p
  })
  
  # Tab2 - Table 
  output$two_table <- renderTable({
    if (input$var_type == "Rate" & input$data_type == "house" ) {
      user_table <- house_seattle_data %>%
        select(year, monthly_average)
      user_table
    } else if (input$var_type == "Rate" & input$data_type == "rent" ) {
      user_table <- rent_seattle_data %>%
        select(year, monthly_average)
      user_table
    } else if (input$var_type == "Percentage" & input$data_type == "house") {
      user_table <- house_seattle_data %>% 
        select(year, percent_change)
    } else if (input$var_type == "Percentage" & input$data_type == "rent") {
      user_table <- rent_seattle_data %>% 
      select(year, percent_change)
    }
  }) # two_table ends here

}
  
