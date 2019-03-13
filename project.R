library("knitr")
library("httr")
library("jsonlite")
library("dplyr")
library("tidyr") 

# read house listings data in the U.S from repo for more data wrangling below 
# read rent data in the U.S
house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

# we only need data from 2010.11 - 2019.1
house_price_data <- house_price_data[ , c(1:7, 183:281)]

# Data wrangling for house listing data in the U.S
# select the first 7 columns
# adds columns containing information for month and year separately
house_price_data <- house_price_data %>%
  gather(key = year, value = year_value, -c(colnames(house_price_data)[1:7])) %>%
  mutate(year = substring(year, 2, 8)) %>%
  separate(year, c("year", "month"), sep = "\\.") %>%
  mutate(year = as.integer(year), month = as.integer(month))

# Data wrangling for rent data in the U.S
# select the first 7 columns
# Again, adds columns containing information for month and year separately
rent_price_data <- rent_price_data %>%
  gather(key = year, value = year_value, -c(colnames(house_price_data)[1:7])) %>%
  mutate(year = substring(year, 2, 8)) %>%
  separate(year, c("year", "month"), sep = "\\.") %>%
  mutate(year = as.integer(year), month = as.integer(month))

# For simplicity and accessbility, 
# we only look at National Data on house and rent price from 2010.11 to 2019.01
# stores a years variable to have a cleaner code later on
years <- 2010:2019

# Create a data frame from house_price_data that contains a summary table
# of National Mean of house price 
house_national_data <- house_price_data %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

house_national_data[, "Percentage"] = c(0, 100 * (log(house_national_data$Rate[2:10]) - log(house_national_data$Rate[1:9])))

rent_national_data <- rent_price_data %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

rent_national_data[, "Percentage"] = c(0, 100 * (log(rent_national_data$Rate[2:10]) - log(rent_national_data$Rate[1:9])))


# Seattle Metro (Specifically King County) Data on House and Rent

# function to get other cities' metropolitan data

get_metropolitan_house_data <- function(city) {
  metro <- house_price_data %>%
    filter(City == city) %>%
    select(Metro) %>%
    pull()
  metro <- metro[1]
  
  result <- house_price_data %>%
    filter(Metro == metro) %>%
    group_by(year, month) %>%
    summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
    group_by(year) %>%
    summarize(Rate = mean(Rate, na.rm = TRUE))
  
  result[, "Percentage"] = c(0, 100 * (log(result$Rate[2:10]) - log(result$Rate[1:9])))
  
  result
}

get_metropolitan_rent_data <- function(city) {
  metro <- rent_price_data %>%
    filter(City == city) %>%
    select(Metro) %>%
    pull()
  metro <- metro[1]
  
  result <- rent_price_data %>%
    filter(Metro == metro) %>% 
    group_by(year, month) %>%
    summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
    group_by(year) %>%
    summarize(Rate = mean(Rate, na.rm = TRUE))
  
  result[, "Percentage"] = c(0, 100 * (log(result$Rate[2:10]) - log(result$Rate[1:9])))
  
  result
}

house_seattle_data <- get_metropolitan_house_data("Seattle")
rent_seattle_data <- get_metropolitan_rent_data("Seattle")

# this is for map

house_seattle_individual <- house_price_data %>%
  filter(Metro == "Seattle-Tacoma-Bellevue") %>%
  group_by(RegionName, year, month) %>%
  summarize(city = City, Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(RegionName, year) %>%
  summarize(city = city[1], Rate = mean(Rate, na.rm = TRUE))

house_seattle_individual[, "Percentage"] = c(0, 100 * (log(house_seattle_individual$Rate[2:10]) - log(house_seattle_individual$Rate[1:9])))

rent_seattle_individual <- rent_price_data %>%
  filter(Metro == "Seattle-Tacoma-Bellevue") %>%
  group_by(RegionName, year, month) %>%
  summarize(city = City, Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(RegionName, year) %>%
  summarize(city = city[1], Rate = mean(Rate, na.rm = TRUE))
rent_seattle_individual[, "Percentage"] = c(0, 100 * (log(rent_seattle_individual$Rate[2:10]) - log(rent_seattle_individual$Rate[1:9])))

# seattle vs washington comparison data
# this data is non-seattle-metropolitan data
house_washington_data <- house_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

house_washington_data[, "Percentage"] = c(0, 100 * (log(house_washington_data$Rate[2:10]) - log(house_washington_data$Rate[1:9])))

rent_washington_data <- rent_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

rent_washington_data[, "Percentage"] = c(0, 100 * (log(rent_washington_data$Rate[2:10]) - log(rent_washington_data$Rate[1:9])))

