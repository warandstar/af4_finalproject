library("ggplot2")
library("dplyr")
library("tidyr")
library("maps")  
library("readr")
library("knitr")
library("httr") 
library("jsonlite")
#install.packages("ggrepel")
library("ggrepel")
library("leaflet")
library("maps")

options(scipen = 999)

house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

#This database produces a map of the states of the United States 
# mainland generated from US Department of the Census data 
# (see the reference).
states <- c("Washington", "New York")
coors <- data.frame(
  long = c(),
  lat = c(),
  stringsAsFactors = FALSE
)
global <- map_data("world")
map.axes(cex.axis=0.8)
state <- map('state', fill = TRUE, col = palette())
  

data("stateMapEnv")

####
states_maps <- map_data("state")
dim(states_maps)

plot <- ggplot(data = states_maps) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) + 
  guides(fill=FALSE) # to leave off the color legend