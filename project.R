library("knitr")
library("httr")
library("jsonlite")
library("dplyr")

house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

# National Data on house and rent price from 2010.11 to 2019.01

house_national_data

rent_national_data


house_state_data

rent_state_data




# Seattle Metro (Specifically King County) Data on House and Rent









# section 2.3 about sample data set
# using seattle as example:

house_sample_data <- house_price_data %>%
  select(RegionID, RegionName, City, State, Metro, CountyName, X2010.11, X2015.11, X2019.01)
View(house_sample_data)

rent_sample_data <- rent_price_data %>%
  select(RegionID, RegionName, City, State, Metro, CountyName, X2010.11, X2015.11, X2019.01)
View(rent_sample_data)

# section 3

# sort the cities by the county to find the main city and its satellite cities
# it returns column of data about cities and its price change 
# for seattle the table will have price change and percentage change of seattle, bellevue, kirkland, renton, etc.

house_national_price_change <- house_sample_data %>%
  summarize(diff = round(mean(X2019.01, na.rm = TRUE) - mean(X2010.11, na.rm = TRUE),2), diff_percentage = round(diff / mean(X2010.11, na.rm = TRUE) * 100, 2))

rent_national_price_change <- rent_sample_data %>%
  summarize(diff = round(mean(X2019.01, na.rm = TRUE) - mean(X2010.11, na.rm = TRUE),2), diff_percentage = round(diff / mean(X2010.11, na.rm = TRUE) * 100, 2))

# get the difference of prices and percent change in house in specific county 
get_metro_avg_house_change <- function(county, state) {
  report_house_data <- house_sample_data %>%
    filter(CountyName == county, State == state) %>%
    summarize(county_name = county, diff = round(mean(X2019.01, na.rm = TRUE) - mean(X2010.11, na.rm = TRUE),2), diff_percentage = round(diff / mean(X2010.11, na.rm = TRUE) * 100, 2))
  report_house_data
}


# get the difference of prices and percent change in rent in specific county 
get_state_avg_rent_change <- function(county, state) {
  report_rent_data <- rent_sample_data %>%
    filter(CountyName == county, State == state) %>%
    summarize(county_name = county, diff = round(mean(X2019.01, na.rm = TRUE) - mean(X2010.11, na.rm = TRUE),2), diff_percentage = round(diff / mean(X2010.11, na.rm = TRUE) * 100, 2))
  report_rent_data
}


# get the joint table of price change in specific county
get_state_avg_change <- function(county, state) {
  joint_data <- house_sample_data %>%
    filter(CountyName == county, State == state) %>%
    left_join(filter(rent_sample_data, CountyName == county, State == state), by = "CountyName", suffix = c(".house", ".rent")) %>%
    summarize(county = unique(CountyName), 
              house_2010 = mean(X2019.01.house, na.rm = TRUE), house_2019 = mean(X2010.11.house, na.rm = TRUE), 
              rent_2010 = mean(X2019.01.rent, na.rm = TRUE), rent_2019 = mean(X2010.11.rent, na.rm = TRUE))
}




