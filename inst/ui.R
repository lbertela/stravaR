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
library(lubridate)
library(eddington)

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
               tags$link(rel = "stylesheet", type = "text/css", href = "custom_valueBox.css")),
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
                    fluidRow(box(title = tags$div("Distance per month", style = "font-size: 25px;"),
                                 status = "primary",
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 plotlyOutput("plot_month", height = "400px", width = "90%"))),
                    fluidRow(box(title = tags$div("Distance over the weeks", style = "font-size: 25px;"),
                                 width = 12,
                                 collapsible = TRUE,
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotlyOutput("plot_week", height = "400px", width = "90%"))),
                    fluidRow(box(title = tags$div("Eddington number", style = "font-size: 25px;"),
                                 width = 12,
                                 collapsible = TRUE,
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotlyOutput("eddington", height = "400px", width = "90%")))
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