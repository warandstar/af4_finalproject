library("shiny")
library("dplyr")

# user interface that the user can filter data that they want to see
our_ui <- navbarPage("Seattle Gentrification",
                     
                     # Tab 0: gives an overview in text of 
                     # 1) the questions this project is trying to find answers for
                     # 2) sets of data that this project deals with
                     # 3) how this project is going to use to the data sets to address 
                     #   the questions
                     tabPanel(title = "Introduction", 
                              includeHTML("introduction.html")
                     ),
                     
                     # Tab 1: Comparing Seattle Data with corresponding National Data
                     # the user can select two sets of inputs: Rate/Percentage, House/Rent
                     # the App will then plot the data accordingly of the Seattle 
                     # Housing Listing/ Rent in terms of Rate/Percentage,
                     # depending on the user's input
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
                     
                     # Tab 2: showing two line representations of trends in 
                     # Seattle house listing AND rent in terms of 
                     # either Rate or Percentage increase, depending on user's input
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
                     
                     # Tab 3: Comparing Seattle Data with corresponding data set of 
                     # the rest of Washington state
                     # the user can select two sets of inputs: Rate/Percentage, House/Rent
                     # the App will then plot the data accordingly of the Seattle 
                     # Housing Listing/ Rent in terms of Rate/Percentage,
                     # depending on the user's input
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
                     
                     # Tab 4: Comparing Seattle Data with corresponding data set of 
                     # the other representative cities in the U.S - the user can choose among 
                     # New York, San Francisco, Chicago, Huston, Washington, Charlotte
                     # the user can select two sets of inputs: Rate/Percentage, House/Rent
                     # the App will then plot the data accordingly of the Seattle 
                     # Housing Listing/ Rent in terms of Rate/Percentage,
                     # depending on the user's input
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
                     
                     # Tab 5: Conclusion
                     # includes a conclusion of answers to the questions
                     # evident by data shown in previous tabs
                     tabPanel(title = "Conclusion", 
                              includeHTML("conclusion.html")
                     ),
                     
                     # Tab 6: Resources
                     # cite the resources where the data was read from 
                     # constrcuted in a R markdown file 
                     tabPanel(title = "Resources", 
                              includeHTML("resource.html")
                     ),
                     
                     # Tab 7: Authors
                     # brief introduction of our team memebers
                     # constrcuted in a R markdown file 
                     tabPanel(title = "Authors", 
                              includeHTML("author.html")
                     )

)