library("shiny")
library("dplyr")

# tab 1 will have visualization of house and rent data of 
# seattle and national level (compare) and user can choose between 
# house price and rent price as well as between price in dollars and percentage change

# tab 2 will have the seattle region's change over time for rent and house (?)

# tab 3 will have data and map of the seattle price level on each year 
# based on user's selection
# tab 4 will compare seattle with other region such as SF

our_ui <- navbarPage("Seattle Gentrification",
                     tabPanel(title = "Introduction", 
                              includeHTML("introduction.html")
                     ),
                     
                     
                     # Define UI for random distribution app ----
                     tabPanel(title = "vs National",
                              fluidPage(
                                sidebarLayout(
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
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput("us_plot"),
                                    textOutput("us_summary")
                                  )
                                )
                              )
                     ),
                     
                     
                     tabPanel(title = "trend",
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    radioButtons(
                                      inputId = "var_type2",
                                      label = "Choose Rate or Percentage Change",
                                      choices = c("Rate", "Percentage"),
                                      selected = "Rate"
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput("seattle_plot"),
                                    textOutput("seattle_summary")
                                  )
                                )
                              )
                     ),
                     
                     tabPanel(title = "vs other Washington",
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    radioButtons(
                                      inputId = "var_type3",
                                      label = "Choose Rate or Percentage Change",
                                      choices = c("Rate", "Percentage"),
                                      selected = "Rate"
                                    ),
                                    
                                    radioButtons(
                                      inputId = "data_type3",
                                      label =  "Choose A Type of Data",
                                      choices = c("House", "Rent"),
                                      selected = "House"
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput("washington_plot"),
                                    textOutput("washington_summary")
                                  )
                                )
                              )
                     ),
                     
                     
                     tabPanel(title = "vs other cities",
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    radioButtons(
                                      inputId = "var_type4",
                                      label = "Choose Rate or Percentage Change",
                                      choices = c("Rate", "Percentage"),
                                      selected = "Rate"
                                    ),
                                    
                                    radioButtons(
                                      inputId = "data_type4",
                                      label =  "Choose A Type of Data",
                                      choices = c("House", "Rent"),
                                      selected = "House"
                                    ),
                                    
                                    selectInput(
                                      inputId = "city",
                                      label = "Choose A City",
                                      choices = c("San Francisco", "New York", "Chicago", "Houston", "Washington", "Charlotte"), 
                                      selected = "San Francisco"
                                      
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput("other_city_plot"),
                                    textOutput("other_city_summary")
                                  )
                                )
                              )
                              
                     ),
                     
                     tabPanel(title = "Map",
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    radioButtons(
                                      inputId = "var_type5",
                                      label = "Choose Rate or Percentage Change",
                                      choices = c("Rate", "Percentage"),
                                      selected = "Rate"
                                    ),
                                    
                                    radioButtons(
                                      inputId = "data_type5",
                                      label =  "Choose A Type of Data",
                                      choices = c("House", "Rent"),
                                      selected = "House"
                                    ),
                                    
                                    selectInput(
                                      inputId = "year",
                                      label = "Choose Year and Month",
                                      choices = 2010:2019,
                                      selected = 2010
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput("map") # to do: change that to leaflet!
                                  )
                                )
                              )
                     ),
                     
                     tabPanel(title = "Conclusion", 
                              includeHTML("conclusion.html")
                     ),
                     
                     tabPanel(title = "Resources", 
                              includeHTML("resource.html")
                     ),
                     
                     tabPanel(title = "Authors", 
                              includeHTML("author.html")
                     )

)