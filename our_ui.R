library("shiny")
library("dplyr")


# Define UI for random distribution app ----
our_ui <- fluidPage(
  
  # App title ----
  titlePanel("Should you buy a house or rent one to live in any of U.S. States or especially in Seattle?
             Find out with our Technocrats App!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: select input with some choices----
      selectInput(
        inputId = "analysis_var",
        label =  "Choose A Type of Analysis",
        choices = c("House Price", "Rental Price", "Difference")
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot w/ summary, and table w/ summary ----
      tabsetPanel(type = "tabs",
                  tabPanel("Overview", includeHTML("overview.html")),
                  tabPanel("Rates In U.S.", plotOutput("us_plot"), textOutput("us_summary")),
                  tabPanel("Rates In Washington", textOutput("wa_summary"), plotOutput("wa_plot")),
                  tabPanel("Rates In Seattle", textOutput("sea_summary"), plotOutput("sea_plot")),
                  tabPanel("Results", includeHTML("results.html")),
                  tabPanel("Resources", includeHTML("resource.html")),
                  tabPanel("Developers", includeHTML("author.html")))
      )
    )
)

