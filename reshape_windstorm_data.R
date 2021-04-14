# read, transform, and rbind the storm event datasets
ws_file = "data/windstorm_num_episodes_by_county_ym_2012_2017.RData"
if (!file.exists(ws_file)) {
  read_windstorm_data = function(path) {
    read.csv(path, stringsAsFactors = FALSE) %>% 
      # look at the events reported by counties (C) rather than forecast zones (Z)
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