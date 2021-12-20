# read, transform, and rbind the storm event datasets
read_windstorm_data = function(path) {
  read.csv(path, stringsAsFactors = FALSE) %>% 
    # look at the events reported by counties (C) rather than forecast zones (Z)
    filter(CZ_TYPE == 'C') %>%
    select(ym = BEGIN_YEARMONTH, state = STATE, county = CZ_NAME, 
           event_type = EVENT_TYPE, episode_id = EPISODE_ID) %>% 
    group_by(state, county, ym) %>%
    summarise(num_episodes = n_distinct(episode_id), .groups = "drop")
} # read_windstorm_data()

read_windstorm_type_data = function(path, type) {
  read.csv(path, stringsAsFactors = FALSE) %>% 
    filter(CZ_TYPE == 'C', EVENT_TYPE == type) %>%
    select(ym = BEGIN_YEARMONTH, state = STATE, county = CZ_NAME, 
           event_type = EVENT_TYPE, episode_id = EPISODE_ID) %>% 
    group_by(state, county, ym) %>%
    summarise(num_episodes = n_distinct(episode_id), .groups = "drop")
} # read_windstorm_type_data()


file_names = paste0("data/", c(
  "StormEvents_details-ftp_v1.0_d2012_c20200317.csv",
  "StormEvents_details-ftp_v1.0_d2013_c20170519.csv",
  "StormEvents_details-ftp_v1.0_d2014_c20210120.csv",
  "StormEvents_details-ftp_v1.0_d2015_c20191116.csv",
  "StormEvents_details-ftp_v1.0_d2016_c20190817.csv",
  "StormEvents_details-ftp_v1.0_d2017_c20210120.csv"
))


ws_file = "data/windstorm_num_episodes_by_county_ym_2012_2017.RData"
if (!file.exists(ws_file)) {
  windstorm = lapply(file_names, read_windstorm_data) %>% bind_rows()
  save(windstorm, file = ws_file)
}

ws_type_file = "data/windstorm_major_types.RData"
if (!file.exists(ws_file)) {
  system.time({ # 150 sec
    ws_thunderstorm = lapply(file_names, read_windstorm_type_data, type = "Thunderstorm Wind") %>% bind_rows()
    ws_hail = lapply(file_names, read_windstorm_type_data, type = "Hail") %>% bind_rows()
    ws_flashflood = lapply(file_names, read_windstorm_type_data, type = "Flash Flood") %>% bind_rows()
    ws_flood = lapply(file_names, read_windstorm_type_data, type = "Flood") %>% bind_rows()
    ws_tornado = lapply(file_names, read_windstorm_type_data, type = "Tornado") %>% bind_rows()
  })
  save(ws_thunderstorm, ws_hail, ws_flashflood, ws_flood, ws_tornado, file = ws_type_file)
}

load(ws_file)