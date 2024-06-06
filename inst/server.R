server <- function(input, output, session) { 
     
     output$mymap <- renderLeaflet({
          strava_map(long = 7.3705, lat = 46.7341, zoom = 8)
     })

}
