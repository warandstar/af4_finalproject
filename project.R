library("knitr")
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
# of national mean of house prices in the U.S
house_national_data <- house_price_data %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

# some math to get the percent change of house_national_data, which is the national mean of house listings, in year and the year before
house_national_data[, "Percentage"] = c(0, 100 * (log(house_national_data$Rate[2:10]) - log(house_national_data$Rate[1:9])))

# Create a data frame called rent_national_data from rent_price_data that 
# contains a summary table of national mean of house prices in the U.S
rent_national_data <- rent_price_data %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

# some math to get the percent change of rent_national_data, which is the national mean of rent, in year and the year before
rent_national_data[, "Percentage"] = c(0, 100 * (log(rent_national_data$Rate[2:10]) - log(rent_national_data$Rate[1:9])))


## Seattle Metro (Specifically King County) Data on House and Rent

# this function takes in an input city and filter 
# house_price_data to get all relevant data of the the metropolitan
# area in which the input city locates and then it 
# returns a mean summary of house listing data of that metropolitan 
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

# this function takes in an input city and filter 
# rent_price_data to get all relevant data of the the metropolitan
# area in which the input city locates and then it 
# returns a mean summary of rent data of that metropolitan 
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

# calls the metropolitan_house_data and get house listing data of 
# Greater Seattle area and stores the data frame as house_seattle_data 
house_seattle_data <- get_metropolitan_house_data("Seattle")

# calls the metropolitan_rent_data and get rent data of 
# Greater Seattle area and stores the data frame as rent_seattle_data 
rent_seattle_data <- get_metropolitan_rent_data("Seattle")


# makes an house_washington_data by filtering house listing data in 
# Washington state but outside the Seattle metropolitan for comparisons
# and ploting
house_washington_data <- house_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

# math to get the percent change of house_washington_data, which is the state mean of house listings, in year and the year before
house_washington_data[, "Percentage"] = c(0, 100 * (log(house_washington_data$Rate[2:10]) - log(house_washington_data$Rate[1:9])))

# makes an rent_washington_data by filtering rent data in 
# Washington state but outside the Seattle metropolitan for comparisons
# and ploting
rent_washington_data <- rent_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

# math to get the percent change of rent_washington_data, which is the state mean of rent, in year and the year before
rent_washington_data[, "Percentage"] = c(0, 100 * (log(rent_washington_data$Rate[2:10]) - log(rent_washington_data$Rate[1:9])))

