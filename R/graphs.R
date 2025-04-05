distance_per_month <- function(data, min_year = NULL, max_year = NULL) {
     
     if (is.null(min_year)) min_year <- min(data$year)
     if (is.null(max_year)) max_year <- max(data$year)
     
     max_month <- max(data %>% filter(year == max_year) %>% pull(month))
     years <- seq(min_year, max_year)
     colors <- paletteer::paletteer_d("ggthemes::Classic_Color_Blind", n = length(years))
     
     data <- my_acts %>%
          filter(year %in% years) %>% 
          group_by(year, month) %>%
          summarise(distance = sum(distance), .groups = "drop") %>%
          complete(year, month = 1:12, fill = list(distance = 0)) %>%
          mutate(year = as.character(year)) %>% 
          filter(!(year == max(year) & month > max_month))
     
     # Define query points for months (1:12) with finer resolution
     query_points <- seq(1, 12, by = 0.01)
     
     # Interpolate distance values for each year using PCHIP
     interpolated_data <- data %>%
          group_by(year) %>%
          do(tibble(month = query_points,
                    distance = pracma::pchip(.$month, .$distance, query_points))) %>%
          ungroup() %>%
          filter(!(year == max(year) & month > max_month))
     
     # Create plot
     p <- plot_ly()
     for (i in years) {
          curve_data <- interpolated_data %>% filter(year == i)
          point_data <- data %>% filter(year == i)
          color_index <- match(i, years)
          
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
               xaxis = list(tickvals = 1:12, ticktext = month.abb, fixedrange = TRUE,
                            tickfont  = list(size = 15), gridcolor = "lightgrey"),
               yaxis = list(title = list(text = "Distance (km)", font = list(size = 20)),
                            tickfont  = list(size = 15), gridcolor = "lightgrey",
                            fixedrange = TRUE, range = c(0, max(data$distance)+10)),
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
               hoverlabel = list(font = list(size = 15))
          ) %>%
          config(displayModeBar = FALSE, showAxisDragHandles = FALSE,
                 scrollZoom = FALSE, doubleClick = FALSE, editable = FALSE)
     
     return(p)
}

distance_over_week <- function(data, min_year = NULL, max_year = NULL) {
     
     if (is.null(min_year)) min_year <- min(data$year)
     if (is.null(max_year)) max_year <- max(data$year)
     
     max_week <- max(data %>% filter(year == max_year) %>% pull(week))
     years <- seq(min_year, max_year)
     colors <- paletteer::paletteer_d("ggthemes::Classic_Color_Blind", n = length(years))
     
     data <- data %>%
          filter(year %in% years) %>% 
          group_by(year, week) %>%
          summarise(distance = sum(distance), .groups = "drop") %>%
          group_by(year) %>%
          mutate(distance = cumsum(distance)) %>%
          ungroup() %>% 
          complete(year, week = 1:53, fill = list(distance = NA)) %>% 
          group_by(year) %>%
          mutate(distance = ifelse(week == 1 & is.na(distance), 0, distance)) %>% 
          fill(distance, .direction = "down") %>%
          ungroup() %>% 
          filter(!(year == max(year) & week > max_week))
     
     # Define query points for weeks (1:53) with finer resolution
     query_points <- seq(1, 53, by = 0.01)
     
     # Interpolate distance values for each year using PCHIP
     interpolated_data <- data %>%
          group_by(year) %>%
          do(tibble(week = query_points,
                    distance = pracma::pchip(.$week, .$distance, query_points))) %>%
          ungroup() %>% 
          filter(!(year == max(year) & week > max_week))
     
     # Create plot
     p <- plot_ly()
     for (i in years) {
          curve_data <- interpolated_data %>% filter(year == i)
          point_data <- data %>% filter(year == i)
          color_index <- match(i, years)
          
          p <- p %>%
               add_trace(
                    x = curve_data$week,
                    y = curve_data$distance,
                    name = i,
                    type = 'scatter',
                    mode = 'lines',
                    line = list(shape = 'spline', color = colors[color_index], width = 3),
                    hoverinfo = "none",
                    legendgroup = i
               ) %>% 
               add_trace(
                    x = point_data$week,
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
               xaxis = list(tickvals = 1:53, 
                            ticktext = ifelse(seq(1, 53) %% 5 == 0, seq(1, 53), " "), 
                            tickangle = 0,
                            fixedrange = TRUE,
                            tickfont  = list(size = 15), 
                            gridcolor = "lightgrey"),
               yaxis = list(title = list(text = "Distance (km)", font = list(size = 20)),
                            tickfont  = list(size = 15), 
                            fixedrange = TRUE, 
                            range = c(0, max(data$distance)+100),
                            gridcolor = "lightgrey"),
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
               hoverlabel = list(font = list(size = 15))
          ) %>%
          config(displayModeBar = FALSE, showAxisDragHandles = FALSE,
                 scrollZoom = FALSE, doubleClick = FALSE, editable = FALSE)
     
     p
     
     return(p)
}

eddington_plot <- function(data) {
     
     dt <- data %>% 
          group_by(start_date_local) %>%
          summarize(distance = sum(distance), .groups = "drop") %>% 
          arrange(distance) %>% 
          pull(distance) %>% 
          floor()
     
     actual <- eddington::E_num(dt)
     
     done_activities <- data.frame(
          km = 50:actual,
          act = sapply(50:actual, function(x) (sum(dt >= x) - x) * (-1))
     )
     
     next_activities <- purrr::map_dfr((actual+1):120, ~ data.frame(
          km = .x,
          act = eddington::E_req(dt, .x)
     ))

     edd <- bind_rows(done_activities, next_activities) %>% 
          mutate(text = ifelse(act < 0, NA, paste0(km, "km - ", act, " activities left")))
     
     # Define color gradients for values below and above zero
     colors_below_zero <- colorRampPalette(c("darkgreen", "lightgreen"))(length(which(edd$act < 0)))
     colors_above_zero <- colorRampPalette(c("lightcoral", "darkred"))(length(which(edd$act >= 0)))
     edd$color <- c(colors_below_zero, colors_above_zero)

     # Create the plot with custom colors
     p <- plot_ly(edd, x = ~km, y = ~act, 
                  type = 'bar', 
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  text = ~text,
                  textposition = 'none') %>% 
          layout(
               xaxis = list(
                    title = list(text = "Distance (km)", font = list(size = 20)),
                    tickvals = 50:120, 
                    ticktext = ifelse(seq(50, 120) %% 5 == 0, seq(50, 120), " "), 
                    tickangle = 0,
                    fixedrange = TRUE,
                    tickfont  = list(size = 15), 
                    gridcolor = "lightgrey"
               ),
               yaxis = list(
                    title = list(text = "Activities", font = list(size = 20)),
                    tickfont  = list(size = 15), 
                    fixedrange = TRUE, 
                    range = c(min(edd$act)-5, max(edd$act)+5),
                    gridcolor = "lightgrey"
               ),
               showlegend = FALSE,
               plot_bgcolor = "rgba(0,0,0,0)",
               paper_bgcolor = "rgba(0,0,0,0)",
               margin = list(t = 50),
               hoverlabel = list(font = list(size = 15))
          ) %>%
          config(displayModeBar = FALSE, showAxisDragHandles = FALSE, 
                 scrollZoom = FALSE, doubleClick = FALSE, editable = FALSE)

     return(p)
     
}







week_plot <- function(data) {
     
     max_weeks_per_year <- my_acts %>%
          distinct(year) %>%
          rowwise() %>%
          mutate(max_week = get_last_week_in_year(year)) %>%
          ungroup() %>%
          rowwise() %>%
          do(tibble(year = .$year, week = 1:.$max_week)) %>%
          ungroup()
     
     dt <- my_acts %>%
          group_by(year, week) %>%
          summarize(distance = sum(distance), .groups = "drop") %>%
          right_join(all_year_weeks, by = c("year", "week")) %>%
          mutate(distance = replace_na(distance, 0)) %>% 
          arrange(year, week)

     
}
