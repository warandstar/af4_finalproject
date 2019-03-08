library("shiny")
library("dplyr")
library("ggplot2")
library("maps")
library("tidyr")
library("mapproj")
library("RColorBrewer")


source("our_ui.R")

options(scipen=999)


# Define server logic for random distribution app ----
our_server <- function(input, output) {
  
  # Load a shapefile of U.S. states using ggplot's `map_data()` function
  state_shape <- map_data("state")
  


  
}
  
