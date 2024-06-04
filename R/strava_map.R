
strava_map <- function() {
     
     map <- leaflet() %>%
          setView(lng = 7.3705, lat = 46.7341, zoom = 8) %>% 
          addProviderTiles(
               provider = "Jawg.Streets",
               options = providerTileOptions(accessToken = Sys.getenv("JAWG_TOKEN"))
          )
     
     for (i in na.omit(unique(my_acts$upload_id))) {
          activity <- my_acts %>% filter(upload_id == i)
          coords   <- googleway::decode_pl(activity$map.summary_polyline)
          map      <- addPolylines(map, lng = coords$lon, lat = coords$lat,
                                   color = 'blue', opacity = 1.5, weight = 2)
     }
     
     map

}