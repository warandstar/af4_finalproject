library("shiny")
library("dplyr")
library("ggplot2")

#rsconnect::setAccountInfo(name='raidakarim', token='95D61D387F155E73F9964C3B44905971', secret='qc/zsilbklp5FtqsER/Yb6cAql4ZGutJRbHRpjBT')

library(rsconnect)

# getting data from .csv files
source("our_ui.R")
source("our_server.R")

# To start running app, passing the variables defined in previous
# code snippets into the `shinyApp()` function
shinyApp(ui = our_ui, server = our_server)
