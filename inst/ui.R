library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(leaflet.providers)
devtools::load_all()

ui <- dashboardPage(
     skin = "blue",
     dashboardHeader(title = "My Strava Dashboard"),
     dashboardSidebar(
          sidebarMenu(
               menuItem("Heat Map", tabName = "heatmap",  icon = icon("location-dot")),
               menuItem("Stats Overview", tabName = "stats", icon = icon("chart-line")),
               menuItem("Data", tabName = "data", icon = icon("database"))
          )
     ),
     dashboardBody(
          tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "custom_valueBox.css")
          ),
          tabItems(
               tabItem(
                    tabName = "heatmap",
                    leafletOutput("mymap")
               ),
               tabItem(
                    tabName = 'stats',
                    fluidRow(
                         valueBox(nrow(my_acts), "Activities", icon = icon("strava"), color = "blue"),
                         valueBox(format(round(sum(my_acts$distance), digits = 0), big.mark = "'"), "Kms", icon = icon("globe"), color = "green"),
                         valueBox(format(round(sum(my_acts$total_elevation_gain), digits = 0), big.mark = "'"), "Meters of elevation", icon = icon("arrow-trend-up"), color = "purple")
                    )
               )
          )
     )
)