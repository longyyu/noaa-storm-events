library(tidyverse)

if (!file.exists("data/weather_aggre_ym_city.RData")) {
  
  us_cities_27 = read.csv("data/weather/city_attributes.csv", stringsAsFactors = FALSE) %>% 
    filter(Country == "United States") %>% .$City
  
  read_rename_pivot_long = function(varname) {
    read.csv(sprintf("data/weather/%s.csv", varname), stringsAsFactors = FALSE) %>%
      rename_with(.cols = -"datetime", .fn = ~str_replace(.x, "\\.", " ")) %>%
      mutate(datetime = strptime(datetime, format = "%Y-%m-%d %H:%M:%S"),
             date = format(datetime, "%Y-%m-%d")) %>%
      pivot_longer(-c("datetime", "date"), names_to = "city", values_to = varname) %>%
      # keep records of the U.S. cities only
      filter(city %in% us_cities_27)
  } # read_rename_pivot_long()
  
  read_vars = function(varname) {
    read_rename_pivot_long(varname) %>%
      # convert hourly data to daily mean values
      group_by(date, city) %>%
      summarise_at(varname, ~mean(.x, na.rm = TRUE)) %>% ungroup() %>%
      mutate_at(varname, .funs = ~ifelse(is.finite(.x), .x, NA)) %>%
      # compute monthly avg and sd of daily mean values
      mutate(ym = substr(date, 1, 7)) %>%
      group_by(ym, city) %>%
      summarise_at(varname, .funs = list(
        avg = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )) %>% ungroup() %>%
      rename_with(.cols = c("avg", "sd"), .fn = ~sprintf("%s_%s", varname, .x))
  } # read_vars()
  
  # read in all numeric variables except for temperature
  df_humidity = read_vars("humidity")
  df_pressure = read_vars("pressure")
  df_wind_direction = read_vars("wind_direction")
  df_wind_speed = read_vars("wind_speed")
  
  # read in temperature -- method 1
  df_temp = read_rename_pivot_long("temperature") %>%
    # convert hourly data to daily min/max values
    group_by(date, city) %>%
    summarise_at("temperature", .funs = list(
      mintemp = ~ min(.x, na.rm = TRUE), 
      maxtemp = ~ max(.x, na.rm = TRUE)
    )) %>% ungroup() %>% 
    mutate_at(c("mintemp", "maxtemp"), .funs = ~ifelse(is.finite(.x), .x, NA)) %>%
    # compute monthly avg and sd of the daily min/max values
    mutate(ym = substr(date, 1, 7)) %>%
    group_by(ym, city) %>%
    summarise_at(c("mintemp", "maxtemp"), .funs = list(
      avg = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE)
    )) %>% ungroup()
  # there may still be NAs at this point, e.g., Miami did not have any records in Nov. 2017
  # this methods results in highly correlated variables - see the pairwise scatterplot
      # pairs(~ mintemp_avg + maxtemp_avg + mintemp_sd + maxtemp_sd, data = df_temp)
  
  # merge all variables' data frames by ym and date
  join_by_vars = c("ym", "city")
  weather = df_temp %>%
    left_join(df_humidity, by = join_by_vars) %>% 
    left_join(df_pressure, by = join_by_vars) %>% 
    left_join(df_wind_direction, by = join_by_vars) %>% 
    left_join(df_wind_speed, by = join_by_vars)
  
  save(weather, file = "data/weather_aggre_ym_city.RData")
  
} else {
  
  load("data/weather_aggre_ym_city.RData")
  
}
