library("knitr")
library("httr")
library("jsonlite")
library("dplyr")
library("tidyr")

house_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zhvi_AllHomes.csv", stringsAsFactors = FALSE))
rent_price_data <- as.data.frame(read.csv(file = "./data/Zip_Zri_AllHomesPlusMultifamily.csv", stringsAsFactors = FALSE))

# we only need data from 2010.11

house_price_data <- house_price_data[ , c(1:7, 183:281)]

<<<<<<< HEAD
=======
house_price_data <- house_price_data %>%
  gather(key = year, value = year_value, -c(colnames(house_price_data)[1:7])) %>%
  mutate(year = substring(year, 2, 8)) %>%
  separate(year, c("year", "month"), sep = "\\.") %>%
  mutate(year = as.integer(year), month = as.integer(month))

rent_price_data <- rent_price_data %>%
  gather(key = year, value = year_value, -c(colnames(house_price_data)[1:7])) %>%
  mutate(year = substring(year, 2, 8)) %>%
  separate(year, c("year", "month"), sep = "\\.") %>%
  mutate(year = year, month = as.integer(month))
>>>>>>> d2136cec743520e46f4e17e9b85a23a9a03f0666

years <- 2010:2019
# National Data on house and rent price from 2010.11 to 2019.01

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

<<<<<<< HEAD
house_seattle_data <- get_metropolitan_house_data("Seattle")
get_seattle_house_rates <- select(house_seattle_data, year, Rate)
get_seattle_house_percentage <- select(house_seattle_data, year, Percentage)

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
rent_seattle_data <- get_metropolitan_rent_data("Seattle")
get_seattle_rent_rates <- select(rent_seattle_data, year, Rate)
get_seattle_rent_percentage <- select(rent_seattle_data, year, Percentage)


# this is for map

house_seattle_individual <- house_price_data %>%
  filter(Metro == "Seattle-Tacoma-Bellevue")

rent_seattle_individual <- rent_price_data %>%
  filter(Metro == "Seattle-Tacoma-Bellevue")

# seattle vs washington comparison data
# this data is non-seattle-metropolitan data
house_washington_data <- house_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%

  gather(key = year, value = year_value, -c(colnames(rent_price_data)[1:7])) %>%
  group_by(year) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  mutate(Percentage = c(0, 100 * (log(Rate[2:99]) - log(Rate[1:98]))))
get_washington_house_rates <- select(house_washington_data, year, Rate)
get_washington_house_percentage <- select(house_washington_data, year, Percentage)


  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

house_washington_data[, "Percentage"] = c(0, 100 * (log(house_washington_data$Rate[2:10]) - log(house_washington_data$Rate[1:9])))


rent_washington_data <- rent_price_data %>%
  filter(State == "WA") %>%
  filter(Metro != "Seattle-Tacoma-Bellevue") %>%
  gather(key = year, value = year_value, -c(colnames(rent_price_data)[1:7])) %>%
  group_by(year) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  mutate(Percentage = c(0, 100 * (log(Rate[2:99]) - log(Rate[1:98]))))
get_washington_rent_rates <- select(rent_washington_data, year, Rate)
get_washington_rent_percentage <- select(rent_washington_data, year, Percentage)

get_house_percentage <- left_join(get_seattle_house_percentage, get_washington_house_percentage, by = "year")
colnames(get_house_percentage) <- c("Year", "Seattle_Percentage", "WA_Percentage")

get_house_rates <- left_join(get_seattle_house_rates, get_washington_house_rates, by = "year")
colnames(get_house_rates) <- c("Year", "Seattle_Rate", "WA_Rate")

get_rent_percentage <- left_join(get_seattle_rent_percentage, get_washington_rent_percentage, by = "year")
colnames(get_rent_percentage) <- c("Year", "Seattle_Percentage", "WA_Percentage")

  group_by(year, month) %>%
  summarize(Rate = mean(year_value, na.rm = TRUE)) %>%
  group_by(year) %>%
  summarize(Rate = mean(Rate, na.rm = TRUE))

rent_washington_data[, "Percentage"] = c(0, 100 * (log(rent_washington_data$Rate[2:10]) - log(rent_washington_data$Rate[1:9])))

get_rent_rates <- left_join(get_seattle_rent_rates, get_washington_rent_rates, by = "year")
colnames(get_rent_rates) <- c("Year", "Seattle_Rate", "WA_Rate")
