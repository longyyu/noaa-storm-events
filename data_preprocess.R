library(tidyverse)

# get the list of the 27 counties we have weather data for --------------------

# the US cities dataset
us_cities = read.csv("data/uscities.csv", stringsAsFactors = FALSE) %>%
  select(state_id, state = state_name, 
         county_fips, county = county_name, city, lat, lng) %>% 
  mutate(city = ifelse(city == "St. Louis", "Saint Louis", city))

# the 27 cities weather dataset
folder = "data/historical-hourly-weather-data_2012-2017"

# join the 27 cities weather dataset and the US cities dataset
# by city name, longitude and latitude 
city = read.csv(sprintf("%s/%s", folder, "city_attributes.csv"), 
                stringsAsFactors = FALSE) %>% 
  filter(Country == "United States") %>% 
  left_join(us_cities, by = c("City" = "city")) %>%
  # when there are multiple cities with the same names, keep the record with the minimum distance
  mutate(distance = sqrt((Latitude - lat)^2 + (Longitude - lng)^2)) %>% 
  group_by(City) %>% filter(distance == min(distance)) %>% ungroup() %>%
  select(city = City, state, county, county_fips, lat = Latitude, long = Longitude) #state_id, county_fips

# cat(paste0(sprintf("%s, %s", city$county, city$state), collapse = '\n'))

library(albersusa)
mapdata  = usa_composite() %>% fortify(us, region = "name")
ggplot() +
  theme_bw() + 
  geom_polygon(data = mapdata, aes(long, lat, group = group),
               colour = "grey",fill = NA) +
  scale_x_continuous(limits = c(-125, -70)) +
  coord_map("polyconic") +
  geom_point(data = city, aes(long, lat), color = "darkred", size = 4, alpha = .4) +
  labs(x = "", y = "") +
  ggtitle("27 U.S. Cities") + theme(plot.title = element_text(hjust = .5))



# read, transform, and rbind the storm event datasets -------------------------
ws_file = "windstorm_num_episodes_by_county_ym_2012_2017.RData"
if (!file.exists(ws_file)) {
  read_windstorm_data = function(path) {
    read.csv(path, stringsAsFactors = FALSE) %>% 
      filter(CZ_TYPE == 'C') %>%
      select(ym = BEGIN_YEARMONTH, state = STATE, county = CZ_NAME, 
             event_type = EVENT_TYPE, episode_id = EPISODE_ID) %>% 
      group_by(state, county, ym) %>%
      summarise(num_episodes = n_distinct(episode_id), .groups = "drop")
  } # read_windstorm_data()
  
  file_names = paste0("data/", c(
    "StormEvents_details-ftp_v1.0_d2012_c20200317.csv",
    "StormEvents_details-ftp_v1.0_d2013_c20170519.csv",
    "StormEvents_details-ftp_v1.0_d2014_c20210120.csv",
    "StormEvents_details-ftp_v1.0_d2015_c20191116.csv",
    "StormEvents_details-ftp_v1.0_d2016_c20190817.csv",
    "StormEvents_details-ftp_v1.0_d2017_c20210120.csv"
  ))
  windstorm = lapply(file_names, read_windstorm_data) %>% bind_rows()
  save(windstorm, file = ws_file)
}
load(ws_file)
windstorm = windstorm %>% filter(county %in% toupper(city$county))


# -----------------------------------------------------------------------------
windstorm_merged = city %>% 
  mutate(state = toupper(state), county = toupper(county)) %>%
  left_join(windstorm, by = c("state", "county"))

windstorm_merged %>%
  group_by(county_fips, state, county) %>% summarise(count = n()) %>% 
  arrange(-count)
