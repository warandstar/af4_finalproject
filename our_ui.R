library("shiny")
library("dplyr")

source("./project.R")

# Define UI for random distribution app ----
our_ui <- fluidPage(
  
  # App title ----
 # this title is unnecessary: titlePanel("Technocrats: Should you buy a house or rather rent one to live in any of U.S. States, or in any of WA cities, or specifically in Seattle?
 #            Find out with our Technocrats App!"),
  titlePanel("Explore Housing & Rental Prices in the in U.S. compared to Seattle"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: radioButtons ----
      radioButtons(
        inputId = "var_type",
        label = "Choose Rate or Percentage Change",
        choices = c("Rate", "Percentage"),
        selected = "Rate"
      ),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: select input with some choices----
      conditionalPanel(condition = "input$tabs == '1st' | input$tabs == '3rd' | input$tabs == '4th' | input$tabs == '5th' | input$tabs == '6th'",
                       radioButtons(
                          inputId = "data_type",
                          label =  "Choose A Type of Data",
                          choices = c("House", "Rent"),
                          selected = "House"
                        )
      ),
      
      br(),
      
      conditionalPanel(condition = "input$tabs == 4th", 
                       selectInput(
                         inputId = "city",
                         label = "Choose A City",
                         choices = c("San Francisco", "New York", "Chicago", "Houston", "Washington", "Charlotte"), 
                         selected = "San Francisco"
                         
                       )
        
      ),
      
      br(),
      
      
      conditionalPanel(condition = "input$tabs == '5th' | input$tabs == '6th'",
                       selectInput(
                         inputId = "year",
                         label = "Choose Year and Month",
                         choices = years,
                         selected = years[1]
                       )
      )
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot w/ summary, and table w/ summary ----
      tabsetPanel(type = "tabs",
                  tabPanel("Comparison between Seattle & US Data", value = "1st", plotOutput("us_plot")),
                  tabPanel("Comparison between House & Rent In Seattle", value = "2nd", textOutput("two_summary"), plotOutput("two_plot"), tableOutput("two_table")),
                  tabPanel("Comparison between Seattle & Washington state", value = "3rd", plotOutput("three_plot"), textOutput("three_summary")),
                  tabPanel("Comparison between Seattle & Other Cities", value = "4th", plotOutput("other_city_plot")),
                  tabPanel("Map 1", value = "5th"),
                  tabPanel("Map 2", value = "6th"),
                  tabPanel("Resources", value = "7th", includeHTML("resource.html")),
                  tabPanel("Developers", value = "8th", includeHTML("author.html")))
      )
    )
)

