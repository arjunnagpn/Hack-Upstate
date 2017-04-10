# Arjun Nag Puttaiah Nagaraja,
# MS in Information Management,
# CAS in Data Science,
# Syracuse University - Spring 2017
# April 9, 2017

# Load required packages to library

library(RColorBrewer)
library(ggmap) 
library(htmltools)
library(rgdal)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(readr)
library(dplyr)
library(magrittr)
library("slam")
library(devtools)
library("tm")
library(sp)
library(caret)
library(e1071)

shinyUI(
  
  dashboardPage(
    
    skin = "red",
    title = "Hack Upstate IX",
    
    dashboardHeader(
      title = "Nest"
      ),
    
    dashboardSidebar(
      
      sidebarMenu(
        
        # Display Sidebar Labels
        
        menuItem("Recommendation", tabName = "overallsummary", icon = icon("bar-chart")),
        menuItem("Crime Information", tabName = "crimeMap", icon = icon("map")),
        menuItem("Street Map", tabName = "streetMap", icon = icon("road"))
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        # Display overall rating interactive map
        
        tabItem(tabName = "overallsummary",

                fluidRow(
                  
                  tabName = "streetMap",
                  
                  tabBox(
                    title = "Overall Recommendation",
                    id = "tabset3",
                    height = "250px",
                    
                    tabPanel(
                      "Interactive Map",
                      leafletOutput("map3")
                    ),
                    
                    width = 12
                  )

                )
        ),
        
        # Display crime maps and widgets
        
        tabItem(tabName = "crimeMap",
                
                fluidRow(
                  box(status = "warning",
                      
                      br(),
                      
                      radioButtons(
                        "crime.button",
                        "Select Year",
                        choices = c(
                          "2017 (Predicted)" = 1,
                          "2016" = 2,
                          "2015" = 3
                        ),
                        selected = 1
                      ),
                      br(),
                      
                      width = 3
                      
                  ),
                  tabBox(
                    title = "Syracuse Crime Information",
                    id = "tabset", 
                    height = "250px",
                    
                    tabPanel(
                      "Interactive Map", 
                      leafletOutput("map")
                    ),
                    
                    width = 9
                  )
                )
        ),
        
        # Display street map for healthcare proximity, number of retail stores and road ratings
        
        tabItem(tabName = "streetMap",
                fluidRow(
                  box(status = "warning",

                      selectInput(
                        "road.attribute",
                        "Road Information",
                        choices = c(
                          "Healthcare" = "healthcare",
                          "Retail Stores" = "retail",
                          "Road Rating" = "road"
                        ),
                        selected = "healthcare"
                      ),

                      width = 3

                  ),

                  tabBox(
                    title = "Syracuse Street Information",
                    id = "tabset2",
                    height = "250px",

                    tabPanel(
                      "Interactive Map",
                      leafletOutput("map2")
                    ),

                    width = 9
                  )
                )
        )
        
      )
    )
  )
)
