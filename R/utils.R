update_activities <- function() {
     
     my_token <- get_token()
     load_activities(token = my_token)
     
}

get_token <- function() {
     
     app_name <- Sys.getenv("APP_NAME")
     app_client_id <- Sys.getenv("APP_CLIENT_ID")
     app_secret <- Sys.getenv("APP_SECRET")
     
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

get_distance <- function(sport = NULL, years = NULL) {
     dist <- my_acts %>% 
          filter(if (!is.null(sport)) sport_type %in% sport else TRUE) %>% 
          filter(if (!is.null(years)) year %in% years else TRUE) %>% 
          pull(distance)
     
     dist <- format(round(sum(dist), digits = 0), big.mark = "'")
          
     return(dist)
}

get_elevation <- function(sport = NULL, years = NULL) {
     elev <- my_acts %>% 
          filter(if (!is.null(sport)) sport_type %in% sport else TRUE) %>% 
          filter(if (!is.null(years)) year %in% years else TRUE) %>% 
          pull(total_elevation_gain)
     
     elev <- format(round(sum(elev), digits = 0), big.mark = "'")
     
     return(elev)
}

get_wdays_freq <- function(years = NULL) {
     
     if (length(years) > 1) {
          sort_years <- sort(unique(years))
          if (all(diff(sort_years) != 1)) stop("years must be consecutive")
     }
     
     date <- my_acts %>% 
          filter(if (!is.null(years)) year %in% years else TRUE) %>%
          pull(start_date_local)
     wdays <- lubridate::wday(as.Date(date), label = TRUE, abbr = FALSE, 
                  locale = "en_US.UTF-8", week_start = 1)
     
     wdays <- as.data.frame(table(wdays)) %>% 
          mutate(share = Freq / sum(Freq))
     names(wdays) <- c("weekday", "freq", "share")
     
     return(wdays)
     
}