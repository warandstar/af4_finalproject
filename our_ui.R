library("shiny")
library("dplyr")






# Define UI for random distribution app ----
our_ui <- fluidPage(
  
  # App title ----
  titlePanel("Compare Seattle Housing Market to that of U.S."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: textInput ----
      textInput(inputId = "data_type", label = "", value = ""),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: select input with some choices----
      selectInput(
        inputId = "analysis_var",
        label =  "Choose A Type of Analysis",
        choices = c()
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot w/ summary, and table w/ summary ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", textOutput("table_summary"), tableOutput("table")),
                  tabPanel("Visualization", textOutput("plot_summary"), plotOutput("visualization"),
                  tabPanel("Authors", textOutput("author_details")))
      )
    )
  )
)