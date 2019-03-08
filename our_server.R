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
  View(state_shape)
  state_shape$region <- c(state.abb, state_shape$region, na.rm = T)
  state_data <- mutate(state_shape, IS02 = state_codes)
  View(state_shape)
  
  # Create a blank map of U.S. states
  ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group),
      color = "white", # show state outlines
      size = .1        # thinly stroked
    ) +
    coord_map() # use a map-based coordinate system
}
  