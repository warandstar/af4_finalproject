library("shiny")
library("dplyr")






# Define UI for random distribution app ----
our_ui <- fluidPage(
  
  # App title ----
  titlePanel("Explore Housing & Rental in U.S. comapring to Seattle"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: textInput ----
      textInput(inputId = "data_type", label = "Enter 'State-specific' or 'All States'", value = "State-specifc"),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: select input with some choices----
      selectInput(
        inputId = "analysis_var",
        label =  "Choose A Type of Analysis",
        choices = c("House Price", "Rental Price", "Both")
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot w/ summary, and table w/ summary ----
      tabsetPanel(type = "tabs",
                  tabPanel("Seattle & US National Rates", textOutput("one_summary"), tableOutput("one_table"), plotOutput("one_plot")),
                  tabPanel("House & Rental Rates in Seattle", textOutput("two_summary"), plotOutput("two_plot"), tableOutput("two_table")),
                  tabPanel("House & Rental Rates in U.S.", plotOutput("three_plot"), textOutput("three_summary")),
                  tabPanel("Resources", includeHTML("resource.html")),
                  tabPanel("Developers", includeHTML("author.html")))
      )
    )
)

