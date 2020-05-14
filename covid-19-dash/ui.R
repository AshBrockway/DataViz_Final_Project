library(shiny)
library(shinydashboard)
library(maps)
library(tidyverse)
library(DT)
library(plotly)
library(gganimate)
library(data.table)
library(readr)

filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
dff10 <- read_csv(filePath) %>% filter(state == "Florida") %>% arrange(county)
dashboardPage(
  dashboardHeader(title = "FL Covid-19"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", 
      menuItem("Florida Cases by County", tabName = "TAB1", icon = icon("TAB 1")), 
      conditionalPanel("input.tabs=='TAB1'",
                       sliderInput("day", "Days Since First Reported Case", min = 1, max = 40, value =40 , step = 1, animate=animationOptions(500))
      ),
      conditionalPanel("input.tabs=='TAB1'",
                       selectInput("County1212", multiple = TRUE, selected = "Palm Beach","Select County:", choices=c(unique(dff10$county)))
      ),
      menuItem("World Map", tabName = "WorldMap1", icon = icon("World Map1")),
      conditionalPanel("input.tabs=='WorldMap1'",
                       radioButtons("worldmap", "Options: ", choices = c("Total Cases"="Total", "Cases per Million"="Million"), selected = "Total"),
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "TAB1", 
        fluidRow(
          valueBoxOutput("vbox", width = 6), 
          valueBoxOutput("vbox2", width = 6)
        ),
        fluidRow(
          box(width=6,  height = 550,
              status="info", 
              title="FL County-Level Covid-19 Cases over Time",
              solidHeader = TRUE,
              plotOutput("myplot")
          ),
          box(width=6, height = 550,
              status="info",
              title = "Data from New York Times",  
              solidHeader = TRUE,
              DTOutput("mydata")
          ),
          box(width=6, height = 550,
              status="info",
              imageOutput("annomate")),
          solidHeader = TRUE,
          box(width = 6, height = 550,
              status = "info",
              plotlyOutput("bar")),
          solidHeader = TRUE
        )
        # Boxes need to be put in a row (or column)
      ),
      tabItem(
        tabName = "TAB2",
        fluidRow(
          box(width=4, status="info",
              title="",
              solidHeader = TRUE, 
              plotOutput("plot2")
          )
        )      
      ),
      tabItem(
        tabName = "WorldMap1",
        plotlyOutput("world_plot", width = "1800px", height = "800px")
      )
    )
  )
)
