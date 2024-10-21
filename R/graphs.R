distance_per_year <- function(data, min_year = NULL, max_year = NULL) {

     if (is.null(min_year)) min_year <- min(data$year)
     if (is.null(max_year)) max_year <- max(data$year)
     
     max_month <- max(data %>% filter(year == max_year) %>% pull(month))
     colors <- paletteer::paletteer_d("ggthemes::Classic_Color_Blind")
     
     data <- data %>%
          filter(year >= min_year & year <= max_year) %>% 
          group_by(year, month) %>%
          summarise(distance = sum(distance), .groups = "drop") %>%
          complete(year, month = 1:12, fill = list(distance = 0)) %>%
          mutate(year = as.character(year)) %>% 
          filter(!(year == max(year) & month > max_month))
     
     # Define query points for months (1:12) with finer resolution
     query_points <- tibble(month = seq(1, 12, by = 0.01))
     
     # Interpolate distance values for each year using PCHIP
     interpolated_data <- data %>%
          group_by(year) %>%
          do({tibble(month = query_points$month,
                     distance = pracma::pchip(.$month, .$distance, query_points$month)
          )}) %>%
          ungroup() %>% 
          filter(!(year == max(year) & month > max_month))
     
     p <- plot_ly()
     
     # Add interpolated lines for each year
     for (i in unique(data$year)) {
          curve_data <- interpolated_data %>% filter(year == i)
          point_data <- data %>% filter(year == i)
          color_index <- match(i, unique(data$year))  # Find the index of the year in unique_years
          
          p <- p %>%
               add_trace(
                    x = curve_data$month,
                    y = curve_data$distance,
                    name = i,
                    type = 'scatter',
                    mode = 'lines',
                    line = list(shape = 'spline', color = colors[color_index], width = 3),
                    hoverinfo = "none",
                    legendgroup = i
               ) %>% 
               add_trace(
                    x = point_data$month,
                    y = point_data$distance,
                    type = 'scatter',
                    mode = 'markers',
                    marker = list(size = 7, color = colors[color_index]),
                    hoverinfo = 'text',
                    text = paste(i, " - ", round(point_data$distance, digits = 1)),
                    showlegend = FALSE,
                    legendgroup = i
               )
     }

     # Customize layout
     p <- p %>% 
               layout(
                    title = list(text = "<b>Distance per Year</b>", font = list(size = 20)),
               xaxis = list(tickvals = 1:12, ticktext = month.abb, 
                            tickfont  = list(size = 15), gridcolor = "lightgrey"),
               yaxis = list(title = list(text = "Distance (km)", font = list(size = 20)),
                            tickfont  = list(size = 15), gridcolor = "lightgrey",
                            range = c(0, max(data$distance)+10)),
               legend = list(x = 1.05,
                             y = 0.5,
                             xanchor = "left", 
                             yanchor = "middle",
                             font = list(size = 20)),
               plot_bgcolor = "rgba(0,0,0,0)",
               paper_bgcolor = "rgba(0,0,0,0)",
               showlegend = TRUE,
               margin = list(t = 50),
               hovermode = "x",
               xaxis = list(fixedrange = TRUE), 
               yaxis = list(fixedrange = TRUE),
               hoverlabel = list(font = list(size = 15))
          ) %>%
          config(displayModeBar = FALSE, showAxisDragHandles = FALSE,
                 scrollZoom = FALSE, doubleClick = FALSE,
                 editable = FALSE)

     return(p)
}
     