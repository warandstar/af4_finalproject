library("shiny")
library("dplyr")
library("ggplot2")

# getting data from .csv files
source("our_ui.R")
source("our_server.R")

# To start run ning app, passing the variables defined in previous
# code snippets into the `shinyApp()` function
shinyApp(ui = our_ui, server = our_server)
