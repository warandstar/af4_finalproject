library("shiny")
library("dplyr")
library("ggplot2")
library("maps")
library("tidyr")
library("mapproj")
library("RColorBrewer")
#install.packages('devtools')

#devtools::install_github("UrbanInstitute/urbnmapr")

source("our_ui.R")

options(scipen=999)


# Define server logic for random distribution app ----
our_server <- function(input, output) {

#library("tidyverse")
library("urbnmapr")
# state map data
#get_data = urbnmapr::states %>%
#select(long, lat, group, state_abbv)

# data
house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

# house data
house_data <- house_price_data %>%
  select(RegionID, RegionName, City, State, Metro, CountyName, X2015.01, X2017.01, X2019.01)



# rent
rent_data <- rent_price_data %>%
select(RegionID, RegionName, City, State, Metro, CountyName, X2015.01, X2017.01, X2019.01)


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

output$four_plot <- renderPlot({
  
  both_plot <-  ggplot(data = state_summary, na.rm = F) +
    geom_jitter(
      mapping = aes(x= House_Price, y= Rental_Price, color = State), # thinly stroked
      size = 5
    ) + 
    # Add title and axis labels
    labs(
      title = "State Specific House Price Vs Rental Rates (2019)", # map title
      x = "House Rates (in dollars)", # x-axis label
      y = "Rental Rates (in dollars)" #  # y-axis label 
    ) 
   both_plot
 
  
})

}
  
