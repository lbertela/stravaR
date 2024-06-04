server <- function(input, output, session) { 
     
     output$mymap <- renderLeaflet({
          strava_map()
     })
     
}
