library("ggplot2")
library("dplyr")
library("tidyr")
library("maps")  
library("readr")
library("knitr")
library("httr") 
library("jsonlite")
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


#####
map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add graticules from a NOAA webserver
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -65, lat = 43, zoom = 7) %>%
  
  # add layers control
  addLayersControl(overlayGroups = c('Place names',
                                     'Graticules',
                                     'Points',
                                     'Lines',
                                     'Polygons'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  
  # list groups to hide on startup
  hideGroup(c('Place names'))

# show map
map

####
pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addCircles(col = ~pal(nbikes), opacity = 0.9) %>% 
  addPolygons(data = lnd, fill = FALSE) %>% 
  addLegend(pal = pal, values = ~nbikes) %>% 
  setView(lng = -0.1, 51.5, zoom = 12) %>% 
  addMiniMap()
