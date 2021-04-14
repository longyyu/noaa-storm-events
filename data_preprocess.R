merge_weather_windstorm_data = function(MATCH_WEATHER_PREV_MONTH = FALSE) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate) # date addition/subtraction
  })
  
  # find the <state, county> for the 27 cities where weather data are vailable 
  
  # the US cities dataset - helps match cities to states and counties
  us_cities = read.csv("data/uscities.csv", stringsAsFactors = FALSE) %>%
    select(state_id, state = state_name, 
           county_fips, county = county_name, city, lat, lng) %>% 
    mutate(city = ifelse(city == "St. Louis", "Saint Louis", city))
  
  
  city = read.csv(sprintf("data/weather/%s", "city_attributes.csv"), 
                  stringsAsFactors = FALSE) %>% 
    filter(Country == "United States") %>% 
    # join the 27 cities weather dataset and the US cities dataset by city names
    left_join(us_cities, by = c("City" = "city")) %>%
    # when there are multiple cities with the same names, keep the record with the minimum distance
    mutate(distance = sqrt((Latitude - lat)^2 + (Longitude - lng)^2)) %>% 
    group_by(City) %>% filter(distance == min(distance)) %>% ungroup() %>%
    select(city = City, state, county)
  
  
  # prepare the windstorm data for merging ------------------------------------
  source("reshape_windstorm_data.R") # load the windstorm data
  windstorm = windstorm %>% 
    # change the `ym` column in windstorm to "%Y-%m" format
    mutate(year = round(ym / 100), month = ym %% 100,
           ym = sprintf("%4i-%02i", year, month)) %>%
    select(-year, -month) %>% 
    # join the `county` and `state` to produce a key used for merging
    mutate(key = toupper(paste0(county, ',', state))) %>% 
    select(-state, -county)
  
  
  # prepare the weather data for merging --------------------------------------
  source("reshape_weather_data.R") # load the weather data
  # match weather dataset's cities to <state, county>
  weather = weather %>% 
    left_join(
      city %>% mutate(key = toupper(paste0(county, ',', state))) %>% select(city, key), 
      by = "city"
    )
  
  if (MATCH_WEATHER_PREV_MONTH) {
    weather = weather %>%
      mutate(ym = as.Date(paste0(ym, "-01")) %m+% months(1) %>% format("%Y-%m"))
  }
  
  # merge weather and windstorm datasets --------------------------------------
  # left join, if num_episodes is NA then replace with 0 - no windstorm that month
  weather %>%
    left_join(windstorm, by = c("ym", "key")) %>% select(-key) %>%
    left_join(city, by = "city") %>%
    select(ym, city, county, state, num_episodes, everything()) %>%
    replace_na(list(num_episodes = 0))
  
} # merge_weather_windstorm_data()

# match windstorm to weather in the same month
data_merged = merge_weather_windstorm_data(MATCH_WEATHER_PREV_MONTH = FALSE)
save(data_merged, file = "data/windstorm_weather_same_month.RData")
# match windstorm to weather in the previous month
data_merged = merge_weather_windstorm_data(MATCH_WEATHER_PREV_MONTH = TRUE)
save(data_merged, file = "data/windstorm_weather_prev_month.RData")
