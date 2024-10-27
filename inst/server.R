server <- function(input, output, session) { 
     
     output$mymap <- renderLeaflet({
          strava_map(long = 7.3705, lat = 46.7341, zoom = 8)
     })
     
     output$calendar <- renderGirafe({
          calendar_heat(dates = my_acts %>% 
                             select(start_date_local, distance) %>% 
                             group_by(start_date_local) %>% 
                             summarise(distance = sum(distance)) %>% 
                             ungroup() %>% 
                             pull(start_date_local),
                        values = my_acts %>% 
                             select(start_date_local, distance) %>% 
                             group_by(start_date_local) %>% 
                             summarise(distance = sum(distance)) %>% 
                             ungroup() %>% 
                             pull(distance),
                        breaks = c(0, 30, 60, 100),
                        labels = c("0-30", "30-60", "60-100", "100+"),
                        colors = c("#0571B0", "#92C5DE", "#F4A582", "#CA0020"))
     })
     
     output$plot_month <- renderPlotly({distance_per_month(data = my_acts)})
     output$plot_week <- renderPlotly({distance_over_week(data = my_acts)})
     output$eddington <- renderPlotly({eddington_plot(data = my_acts)})
     
     t_theme <- reactablefmtr::default()
     t_theme[["tableStyle"]]$fontSize <- 18
     t_theme[["headerStyle"]]$fontSize <- 20
     t_theme[["headerStyle"]]$background <- "#C2B8B8"
     t_theme[["highlightColor"]] <- "#D0D0D0"
     t_theme[["stripedColor"]] <- "#DCDCDC"
     t_theme[["backgroundColor"]] <- "#ECF0F5"
     t_theme[["tableStyle"]]$borderBottom <- "1px solid #000000"

     output$strava_table <- renderReactable(
          
          reactable(my_acts %>% 
                         select(sport_type, name, start_date_local, distance,
                                total_elevation_gain, moving_time, average_speed) %>% 
                         mutate(sport_type = case_when(
                              sport_type == "vtt" ~ "vtt_icon",
                              sport_type == "road" ~ "road_icon",
                              sport_type == "gravel" ~ "gravel_icon",
                              sport_type == "run" ~ "run_icon",
                              sport_type == "hike" ~ "hike_icon")) %>% 
                         mutate(distance = round(distance, digits = 1)) %>% 
                         mutate(average_speed = round(average_speed, digits = 1)) %>% 
                         mutate(total_elevation_gain = format(round(total_elevation_gain, digits = 0), big.mark = "'")) %>% 
                         mutate(moving_time = tolower(as.character(lubridate::seconds_to_period(moving_time)))),
                    defaultColDef = colDef(
                         vAlign = "center",
                         align = "center"),
                    columns = list(
                         sport_type = colDef(name = "Sport", minWidth = 40,
                                             cell = function(value) {
                                                  image <- img(src = sprintf("%s.png", value), 
                                                               style = "height: 40px;", 
                                                               alt = value)
                                             }
                         ),
                         name = colDef(name = "Name", minWidth = 160, align = "left"),
                         start_date_local = colDef(name = "Date", minWidth = 100),
                         distance = colDef(name = "Distance", minWidth = 100),
                         total_elevation_gain = colDef(name = "Elevation gain", minWidth = 130),
                         moving_time = colDef(name = "Duration", minWidth = 100),
                         average_speed = colDef(name = "Km/h", minWidth = 100)
                    ),
                    striped = TRUE,
                    compact = TRUE,
                    pageSizeOptions = c(5, 10, 15, 20, nrow(my_acts)),
                    showPageSizeOptions = TRUE,
                    height = 605,
                    width = 1200,
                    resizable = TRUE,
                    highlight = TRUE,
                    searchable = TRUE,
                    theme = t_theme
          )
     )
}
