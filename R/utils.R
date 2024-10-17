library(dplyr)
library(lubridate)

update_activities <- function() {
     
     id <- get_credential()
     my_token <- get_token(id)
     load_activities(token = my_token)
     
}

get_credential <- function() {
     app_name <- Sys.getenv("APP_NAME")
     app_client_id <- Sys.getenv("APP_CLIENT_ID")
     app_secret <- Sys.getenv("APP_SECRET")
     
     credential <- c(app_name, app_client_id, app_secret)
     
     return(credential)
}

get_token <- function(credential) {
     token <- httr::config(token = rStrava::strava_oauth(
          app_name, app_client_id, app_secret,
          app_scope = "activity:read_all"))
     
     return(token)
}

load_activities <- function(token) {
     
     activities <- rStrava::get_activity_list(stoken = token)
     my_acts <- rStrava::compile_activities(activities) %>% 
          
          # convert start_date_local to Date object
          mutate(start_date_time_local = as_datetime(x = start_date_local),
                 start_date_local = as_date(x = start_date_local)) %>% 
          
          # separate date/time components into different columns
          mutate(year = year(start_date_time_local), 
                 month = month(start_date_time_local), 
                 day = day(start_date_time_local), 
                 hour = hour(start_date_time_local),
                 minute = minute(start_date_time_local),
                 second = second(start_date_time_local)) %>% 
          
          # rename sports
          mutate(sport_type = case_when(
               sport_type == "MountainBikeRide" ~ "vtt",
               sport_type == "Ride" ~ "road",
               sport_type == "GravelRide" ~ "gravel",
               sport_type == "Run" ~ "run",
               sport_type == "Hike" ~ "hike")) %>% 
          
          # select, rename & reorder columns
          select(
               # activity & athlete info
               upload_id, name, sport_type, start_date_time_local, start_date_local,
               year, month, day, hour, minute, second,
               # activity stats
               distance, total_elevation_gain, elev_high, 
               average_speed, max_speed, 
               moving_time, elapsed_time,
               manual,
               # map info
               map.id, map.resource_state, map.summary_polyline, 
               start_latlng1, start_latlng2, end_latlng1, end_latlng2)
     
     # Save the inventory
     usethis::use_data(my_acts, overwrite = TRUE)
     
}
