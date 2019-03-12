library("shiny")
library("dplyr")

# tab 1 will have visualization of house and rent data of 
# seattle and national level (compare) and user can choose between 
# house price and rent price as well as between price in dollars and percentage change

# tab 2 will have the seattle region's change over time for rent and house (?)

# tab 3 will have data and map of the seattle price level on each year 
# based on user's selection
# tab 4 will compare seattle with other region such as SF

source("./project.R")

our_ui <- navbarPage("Seattle Gentrification",
  tabPanel(title = "Intro",
           titlePanel("Gentrification: Explore Housing & Rental Prices in the in U.S. compared to Seattle"),
           
           p("We are going to explore the simple case of gentrification in the example of Seattle"),
           
           p("Questions we will answers are: "),
           
           p("We will address those questions by showing visualization of each case by: "),
           
           p("First we will compare the Seattle's house and rental price with national average level."),
           
           p("Then, We will explore the trend of Seattle house and rental price."),
           
           p("Then, we will see if it is general case of Washington state or just Seattle."),
           
           p("Lastly we will compare with the Seattle area and other cities area."),
           
           p("We will present the map as well.")
  ),

# Define UI for random distribution app ----
our_ui <- fluidPage(
  
  # App title ----
  titlePanel("Explore Housing & Rental Prices in the U.S. compared to Seattle"),

  tabPanel(title = "vs National",
           fluidRow(
             sidebarPanel(
               radioButtons(
                 inputId = "var_type",
                 label = "Choose Rate or Percentage Change",
                 choices = c("Rate", "Percentage"),
                 selected = "Rate"
               ),
               
               radioButtons(
                 inputId = "data_type",
                 label =  "Choose A Type of Data",
                 choices = c("House", "Rent"),
                 selected = "House"
               ),
               
               mainPanel(
                 plotOutput("us_plot")
               )
             )
           )
  ),
  
  
  tabPanel(title = "trend",
           fluidRow(
             sidebarPanel(
               radioButtons(
                 inputId = "var_type",
                 label = "Choose Rate or Percentage Change",
                 choices = c("Rate", "Percentage"),
                 selected = "Rate"
               ),
               
               mainPanel(
                 plotOutput("us_plot2")
               )
             )
           )
           
  ),
  
  tabPanel(title = "vs other Washington",
           fluidRow(
             sidebarPanel(
               radioButtons(
                 inputId = "var_type",
                 label = "Choose Rate or Percentage Change",
                 choices = c("Rate", "Percentage"),
                 selected = "Rate"
               ),
               
               radioButtons(
                 inputId = "data_type",
                 label =  "Choose A Type of Data",
                 choices = c("House", "Rent"),
                 selected = "House"
               ),
               
               mainPanel(
                 plotOutput("washington_plot")
               )
             )
           )
  ),
  
  
  tabPanel(title = "vs other cities",
           fluidRow(
             sidebarPanel(
               radioButtons(
                 inputId = "var_type",
                 label = "Choose Rate or Percentage Change",
                 choices = c("Rate", "Percentage"),
                 selected = "Rate"
               ),
               
               radioButtons(
                 inputId = "data_type",
                 label =  "Choose A Type of Data",
                 choices = c("House", "Rent"),
                 selected = "House"
               ),
               
               selectInput(
                 inputId = "city",
                 label = "Choose A City",
                 choices = c("San Francisco", "New York", "Chicago", "Houston", "Washington", "Charlotte"), 
                 selected = "San Francisco"
                 
               ), 
               
               mainPanel(
                 plotOutput("other_city_plot")
               )
             )
           )
           
  ),
  
  tabPanel(title = "Map 1",
           fluidRow(
             sidebarPanel(
               radioButtons(
                 inputId = "var_type",
                 label = "Choose Rate or Percentage Change",
                 choices = c("Rate", "Percentage"),
                 selected = "Rate"
               ),
               
               radioButtons(
                 inputId = "data_type",
                 label =  "Choose A Type of Data",
                 choices = c("House", "Rent"),
                 selected = "House"
               ),
               
               selectInput(
                 inputId = "year",
                 label = "Choose Year and Month",
                 choices = years,
                 selected = years[1]
               ), 
               
               mainPanel(
                 plotOutput("map1")
               )
             )
           )
  ),
  
  tabPanel(title = "Map 2",
           fluidRow(
             sidebarPanel(
               radioButtons(
                 inputId = "var_type",
                 label = "Choose Rate or Percentage Change",
                 choices = c("Rate", "Percentage"),
                 selected = "Rate"
               ),
               
               radioButtons(
                 inputId = "data_type",
                 label =  "Choose A Type of Data",
                 choices = c("House", "Rent"),
                 selected = "House"
               ),
               
               selectInput(
                 inputId = "year",
                 label = "Choose Year and Month",
                 choices = years,
                 selected = years[1]
               ), 
               
               mainPanel(
                 plotOutput("map2")
               )
             )
           )
           
  ),
  
  
  tabPanel(title = "Resources", 
           includeHTML("resource.html")
           ),
  
  tabPanel(title = "Authors", 
           includeHTML("author.html")
           )

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
                  tabPanel("Seattle & US National Rates", textOutput("one_summary"), tableOutput("one_table"), plotOutput("one_plot")),
                  tabPanel("House & Rental Rates in Seattle", verbatimTextOutput("two_summary"), plotOutput("two_plot"), tableOutput("two_table")),
                  tabPanel("House & Rental Rates in U.S.", plotOutput("three_plot"), textOutput("three_summary")),
                  tabPanel("Resources", includeHTML("resource.html")),
                  tabPanel("Developers", includeHTML("author.html")),
                  tabPanel("Comparison between Seattle & US Data", value = "1st", plotOutput("us_plot")),
                  tabPanel("Comparison between House & Rent In Seattle", value = "2nd", textOutput("two_summary"), plotOutput("two_plot"), tableOutput("two_table")),
                  tabPanel("Comparison between Seattle & Washington state", value = "3rd", plotOutput("three_plot"), textOutput("three_summary")),
                  tabPanel("Comparison between Seattle & Other Cities", value = "4th", plotOutput("other_city_plot")),
                  tabPanel("Map 1", value = "5th"),
                  tabPanel("Map 2", value = "6th"),
                  tabPanel("Resources", value = "7th", includeHTML("resource.html")),
                  tabPanel("Developers", value = "8th", includeHTML("author.html"))
                  )
      )
)

)
