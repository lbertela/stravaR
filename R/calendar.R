calendar_heat <- function(dates, values, min_year = NULL, max_year = NULL,
                          breaks, labels, colors) {
     
     if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", dates))) {
          stop("the dates should have the format 'YYYY-MM-DD'.")
     }
     
     if (!(length(breaks) == length(labels) & length(labels) == length(colors))) {
          stop("'breaks', 'labels' and 'colors' must have the same length")
     }
     
     data <- data.frame(value = values, dates = dates)
     
     if (is.null(min_year)) min_year <- substr(min(data$dates), 1, 4)
     if (is.null(max_year)) max_year <- substr(max(data$dates), 1, 4)
     
     min.date <- as.Date(paste0(min_year, "-1-1"))
     max.date <- as.Date(paste0(max_year, "-12-31"))
     all_days <- tibble(dates = seq(min.date, max.date, by = "days"))
     
     # Merge values to days
     caldat <- left_join(all_days, data, by = "dates") %>%
          mutate(dotw = lubridate::wday(dates, week_start = 1) - 1,     # Monday as start of the week
                 woty = as.numeric(format(dates, "%W")) + 1, # Week of the year
                 yr = as.factor(format(dates, "%Y")),
                 month = as.numeric(format(dates, "%m"))) %>%
          group_by(yr) %>%
          mutate(seq = row_number()) %>%
          ungroup()
     
     month_boundaries <- caldat %>%
          group_by(yr, month) %>%
          mutate(first_wotm = woty[which.min(seq)],     # Week of the year for the first day of the month
                 last_wotm = woty[which.max(seq)],      # Week of the year for the last day of the month
                 first_dotm = dotw[which.min(seq)],     # Day of the week of the first day of the month
                 last_dotm = dotw[which.max(seq)]) %>%  # Day of the week of the last day of the month
          ungroup() %>% 
          mutate(x_m_start = first_wotm - 0.5,       # x position of month start
                 x_m_end = last_wotm + 0.5,          # x position of month end
                 y_m_start_top = first_dotm - 0.5,   # y top position of start of the month
                 y_m_end_down = last_dotm + 0.5) %>% # y down position of end of the month
          select(yr, month, x_m_start, x_m_end, y_m_start_top, y_m_end_down) %>% 
          unique()
     
     fill_borders <- month_boundaries %>%
          filter(month %in% c(1, 12)) %>%
          group_by(yr) %>% 
          mutate(x_v_border = ifelse(month == 12, x_m_end - 1, 1.5),
                 y_v_top = ifelse(month == 1, -0.5, y_m_end_down),
                 y_v_down = ifelse(month == 1, y_m_start_top, 6.5),
                 x_h_down_left = first(x_m_start),  # Takes x_m_start from month 1
                 x_h_down_right = ifelse(month == 12 & y_m_end_down != 6.5, x_m_end - 1, x_m_end),
                 x_right_left = ifelse(month == 12 & y_m_end_down == 6.5, NA, x_m_end - 1))
     
     caldat <- caldat %>%
          mutate(category = cut(value, breaks = c(0, 30, 60, 100, Inf),
                                labels = labels, include.lowest = TRUE))
     
     # Create ggplot
     plot <- ggplot(caldat, aes(x = woty, y = dotw, fill = category)) +
          geom_tile(color = "lightgrey", size = 0.25) +
          scale_y_reverse(breaks = 0:6,
                          labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
          scale_x_continuous(breaks = seq(2.9, 52, by = 4.42),
                             labels = month.abb,
                             expand = c(0, 0)) +
          scale_fill_manual(values = colors,
                            labels = labels, 
                            na.value = "white",
                            na.translate = FALSE) +
          geom_segment(data = month_boundaries, na.rm = TRUE, # vertical month start
                       aes(x = x_m_start, xend = x_m_start,
                           y = 6.5, yend = y_m_start_top),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          geom_segment(data = month_boundaries, na.rm = TRUE, # vertical month end
                       aes(x = x_m_end, xend = x_m_end,
                           y = -0.5, yend = y_m_end_down),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          geom_segment(data = month_boundaries, na.rm = TRUE, # horizontal between months
                       aes(x = x_m_start, xend = x_m_start + 1,
                           y = y_m_start_top, yend = y_m_start_top),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          geom_segment(data = fill_borders, na.rm = TRUE, # vertical left and right borders
                       aes(x = x_v_border, xend = x_v_border,
                           y = y_v_top, yend = y_v_down),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          geom_segment(data = fill_borders, na.rm = TRUE, # horizontal long top border
                       aes(x = 1.5, xend = x_m_end,
                           y = -0.5, yend = -0.5),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          geom_segment(data = fill_borders, na.rm = TRUE, # horizontal long bottom border
                       aes(x = x_h_down_left, xend = x_h_down_right,
                           y = 6.5, yend = 6.5),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          geom_segment(data = fill_borders, na.rm = TRUE, # horizontal small right border
                       aes(x = x_right_left, xend = x_right_left + 1,
                           y = y_m_end_down, yend = y_m_end_down),
                       color = "black", linewidth = 0.6, inherit.aes = FALSE) +
          
          facet_wrap(~yr, ncol = 1, strip.position = "top") +
          coord_fixed(ratio = 1, clip = "off") + 
          theme_minimal(base_size = 10) +
          labs(fill=NULL) +
          theme(axis.title = element_blank(),
                axis.text.x = element_text(size = 10, vjust = 2, face = "bold"),
                axis.text.y = element_text(size = 9, margin = margin(r = 5)),
                strip.text = element_text(size = 15, face = "bold"),
                panel.grid = element_blank(),
                legend.position = "right",
                legend.text = element_text(size = 15),
                legend.key.size = unit(0.5, 'cm'),
                legend.key.spacing.y = unit(0.5, 'cm'))
     
     return(plot)
}
