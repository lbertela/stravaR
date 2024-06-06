#' Produce leaflet map with Strava data
#'
#' @param long Initial longitude position
#' @param lat Initial latitude position
#' @param zoom Initial zoom level
#'
#' @return a leaflet map
#' @export
#'
#' @examples
strava_map <- function(long, lat, zoom) {
     
     map <- leaflet() %>%
          setView(lng = long, lat = lat, zoom = zoom) %>% 
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