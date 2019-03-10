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
get_data = urbnmapr::states %>%
  select(long, lat, group, state_abbv)

# data
house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

# house data
house_data <- house_price_data %>%
  select(RegionID, RegionName, City, State, Metro, CountyName, X2019.01)

# rent
rent_data <- rent_price_data %>%
select(RegionID, RegionName, City, State, Metro, CountyName, X2019.01)


# state house data
state_house_data <- house_data %>%
  select(State, X2019.01)

# state rent data
state_rent_data <- rent_data %>%
  select(State, X2019.01)

# change colnames
colnames(get_data) <- c("long", "lat", "group", "State")

# joining data with left join
combined_house_data <- left_join(get_data, state_house_data, by = "State")
colnames(combined_house_data) <- c("long", "lat", "group", "State", "Price")

combined_rent_data <- left_join(get_data, state_rent_data, by = "State")
colnames(combined_rent_data) <- c("long", "lat", "group", "State", "Price")

# get house plot
house_plot <- ggplot(data = combined_house_data, na.rm = F) +
  geom_polygon(
    mapping = aes(x = long, y = lat, fill = Price, group = group),
    color = "white", # thinly stroked
    size = .7
  ) + 
  # Add title and axis labels
  labs(
    title = "State specific House Rates in 2019 (most recent)", # map title
    x = "Longitude", # x-axis label
    y = "Latitude" #  # y-axis label 
  ) +
  coord_map()

# get rent plot
rent_plot <- ggplot(data = combined_rent_data, na.rm = F) +
  geom_polygon(
    mapping = aes(x = long, y = lat, fill = Price, group = group),
    color = "white", # thinly stroked
    size = .7
  ) + 
  # Add title and axis labels
  labs(
    title = "State specific House Rates in 2019 (most recent)", # map title
    x = "Longitude", # x-axis label
    y = "Latitude" #  # y-axis label 
  ) +
  coord_map()

}
  
