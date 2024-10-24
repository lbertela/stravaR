library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(leaflet.providers)
library(reactable)
library(lattice)
library(grid)
library(ggplot2)
library(ggiraph)
library(plotly)
library(tidyr)

devtools::load_all()

ui <- dashboardPage(
     skin = "blue",
     dashboardHeader(title = "My Strava Dashboard"),
     dashboardSidebar(
          sidebarMenu(
               menuItem("Heat Map", tabName = "heatmap",  icon = icon("location-dot")),
               menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
               menuItem("Stats Overview", tabName = "stats", icon = icon("chart-line")),
               menuItem("Data", tabName = "data", icon = icon("database"))
          )
     ),
     dashboardBody(
          tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "custom_valueBox.css"),
               tags$style(HTML(".content-wrapper {overflow-y: auto; overflow-x: hidden;}"))
          ),
          fluidRow(
               valueBox(nrow(my_acts), "Activities", icon = icon("strava"), color = "blue"),
               valueBox(get_distance(), "Kms", icon = icon("globe"), color = "green"),
               valueBox(get_elevation(), "Meters of elevation", icon = icon("arrow-trend-up"), color = "purple")
          ),
          tabItems(
               tabItem(
                    align = "center",
                    tabName = "heatmap",
                    leafletOutput("mymap", width="70%", height="700px")
               ),
               tabItem(
                    align = "center",
                    tabName = "calendar",
                    girafeOutput("calendar")
               ),
               tabItem(
                    align = "center",
                    tabName = "stats",
                    plotlyOutput("plot_test", height = "400px", width = "90%"),
               ),
               tabItem(
                    align = "center",
                    tabName = 'data',
                    br(),
                    reactableOutput(outputId = "strava_table")
               )
          )
     )
)