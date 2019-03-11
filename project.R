library("knitr")
library("httr")
library("jsonlite")
library("dplyr")
library("tidyr")

house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

# we only need data from 2010.11

house_price_data <- house_price_data[ , c(1:7, 183:281)]

# National Data on house and rent price from 2010.11 to 2019.01

house_national_data <- house_price_data %>%
  gather(key = year, value = year_value, -c(colnames(house_price_data)[1:7])) %>%
  group_by(year) %>%
  summarize(monthly_average = mean(year_value, na.rm = TRUE)) %>%
  mutate(percent_change = c(0, 100 * (log(monthly_average[2:99]) - log(monthly_average[1:98]))))

rent_national_data <- rent_price_data %>%
  gather(key = year, value = year_value, -c(colnames(rent_price_data)[1:7])) %>%
  group_by(year) %>%
  summarize(monthly_average = mean(year_value, na.rm = TRUE)) %>%
  mutate(percent_change = c(0, 100 * (log(monthly_average[2:99]) - log(monthly_average[1:98]))))



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
    gather(key = year, value = year_value, -c(colnames(house_price_data)[1:7])) %>%
    group_by(year) %>%
    summarize(monthly_average = mean(year_value, na.rm = TRUE)) %>%
    mutate(percent_change = c(0, 100 * (log(monthly_average[2:99]) - log(monthly_average[1:98]))))
  
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
    gather(key = year, value = year_value, -c(colnames(rent_price_data)[1:7])) %>%
    group_by(year) %>%
    summarize(monthly_average = mean(year_value, na.rm = TRUE)) %>%
    mutate(percent_change = c(0, 100 * (log(monthly_average[2:99]) - log(monthly_average[1:98]))))
  
  result
}

house_seattle_data <- get_metropolitan_house_data("Seattle")

rent_seattle_data <- get_metropolitan_rent_data("Seattle")


# seattle vs washington comparison data
# this data is non-seattle-metropolitan data
house_washington_data <- house_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  gather(key = year, value = year_value, -c(colnames(rent_price_data)[1:7])) %>%
  group_by(year) %>%
  summarize(monthly_average = mean(year_value, na.rm = TRUE)) %>%
  mutate(percent_change = c(0, 100 * (log(monthly_average[2:99]) - log(monthly_average[1:98]))))

rent_washington_data <- rent_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  gather(key = year, value = year_value, -c(colnames(rent_price_data)[1:7])) %>%
  group_by(year) %>%
  summarize(monthly_average = mean(year_value, na.rm = TRUE)) %>%
  mutate(percent_change = c(0, 100 * (log(monthly_average[2:99]) - log(monthly_average[1:98]))))

