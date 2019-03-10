library("shiny")
library("dplyr")






# Define UI for random distribution app ----
our_ui <- fluidPage(
  
  # App title ----
  titlePanel("Explore Gentrification in U.S."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: textInput ----
      textInput(inputId = "data_type", label = "Enter Table or Plot", value = "Plot"),
      
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
                  tabPanel("Compare Seattle & US National Rates", textOutput("one_summary"), plotOutput("one_plot")),
                  tabPanel("Compare House & Rent Rates", tableOutput("four_table"), textOutput("four_summary"), plotOutput("four_plot")),
                  tabPanel("Resources", includeHTML("resource.html")),
                  tabPanel("Developers", includeHTML("author.html")))
      )
    )
)
