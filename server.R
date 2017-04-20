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

# Read required files into R

predictions2017.df <- read.csv("predictions2017.df.csv")

predictions2017.df <- predictions2017.df[predictions2017.df$Major >=0.5,]
uniqpredictions2017.df <- predictions2017.df[!duplicated(predictions2017.df$address),]

my_crime_Data_final2016 <- read.csv("second_crimeData_final2016.csv")

my_crime_Data_final2015 <- read.csv("second_crimeData_final2015.csv")

HealthCarePr <- read.csv("HealthCarePr.csv")

Retail_Food_Stores_final <- read.csv("Retail_Food_Stores_final.csv")

RoadRatingsOnly2015 <- read.csv("RoadRatingsOnly2015.csv")

FinalRatings <- read.csv("FinalRatings.csv")

street.shpfile <- readRDS("street.shpfile")

shinyServer(
  
  function(input, output, session) {
    
    # Display Interactive Map for Crime Data
    
    output$map <- 
      
      renderLeaflet({
        
        if (input$crime.button == 1) {
          leaflet(uniqpredictions2017.df) %>% 
            # setView() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%
            addMarkers(
              clusterOptions = markerClusterOptions(),
              icon = ~makeIcon("C:/Arjun/SU/Other/Projects/Upstate NY Hackathon/Processed Datasets/download.png", "C:/Arjun/SU/Other/Projects/Upstate NY Hackathon/Processed Datasets/download.png", 24, 24),
              popup = paste0(uniqpredictions2017.df$address, ", ",  "\nChance of Major Crime: "  , floor(uniqpredictions2017.df$Major*100), "%")
            )
        }
        
        else if (input$crime.button == 2) {
          leaflet(my_crime_Data_final2016) %>% 
            # setView() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%
            addCircleMarkers(
              radius = 5,
              color = "darkorange",
              fillColor = "darkorange", 
              stroke = FALSE, 
              fillOpacity = 0.5,
              clusterOptions = markerClusterOptions(),
              popup = paste0(my_crime_Data_final2016$address, ", Type of Crime: ", my_crime_Data_final2016$police_call_crime)
            )
        }
        
        else if (input$crime.button == 3) {
          
          leaflet(my_crime_Data_final2015) %>% 
            # setView() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%
            addCircleMarkers(
              radius = 5,
              color = "darkorange",
              fillColor = "darkorange", 
              stroke = FALSE, 
              fillOpacity = 0.5,
              clusterOptions = markerClusterOptions(),
              popup = paste0(my_crime_Data_final2015$address, ", Type of Crime: ", my_crime_Data_final2015$police_call_crime)
            )
        }
        
      })
    
    
    # Display street map for healthcare proximity, number of retail stores and road ratings

    output$map2 <-

      renderLeaflet({

        if (input$road.attribute == "healthcare") {
          street.shpfile@data <- cbind(street.shpfile@data, HealthCarePr$HCRating)
          
          atest <- length(unique(street.shpfile@data$`HealthCarePr$HCRating`))
          cpal <- colorFactor(colorRamps::matlab.like(atest), street.shpfile@data$`HealthCarePr$HCRating`)
          
          qpal <- colorNumeric(
            palette = colorRamps::matlab.like(atest),
            domain = NULL
          )
          
          q <- round(quantile(street.shpfile@data$`HealthCarePr$HCRating`, probs = c(0, .25, .75, .95, .99,  1), na.rm = T))
          q <- unname(q)
          
          leaflet() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%
            addPolylines(
              data = street.shpfile,
              color = ~qpal(street.shpfile@data$`HealthCarePr$HCRating`),
              popup = paste0(street.shpfile@data$STREET, ": ", round(street.shpfile@data$`HealthCarePr$HCRating`,2))
            ) %>%
            addLegend("topright", pal = qpal, values = 1:10,
                      title = "Distance to Hospital",
                      opacity = 1
            )
        }
        
        else if (input$road.attribute == "retail") {
          
          street.shpfile@data <- cbind(street.shpfile@data, Retail_Food_Stores_final$scaled_Count_Less1)
          
          atest <- length(unique(street.shpfile@data$`Retail_Food_Stores_final$scaled_Count_Less1`))
          cpal <- colorFactor(colorRamps::matlab.like(atest), street.shpfile@data$`Retail_Food_Stores_final$scaled_Count_Less1`)
          
          qpal <- colorNumeric(
            palette = rev(colorRamps::matlab.like(atest)),
            domain = NULL
          )
          
          q <- round(quantile(street.shpfile@data$`Retail_Food_Stores_final$scaled_Count_Less1`, probs = c(0, .25, .75, .95, .99,  1), na.rm = T))
          q <- unname(q)
          
          leaflet() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%
            addPolylines(
              data = street.shpfile,
              color = ~qpal(street.shpfile@data$`Retail_Food_Stores_final$scaled_Count_Less1`),
              popup = paste0(street.shpfile@data$STREET, ": ", round(street.shpfile@data$`Retail_Food_Stores_final$scaled_Count_Less1`,2))
            ) %>%
            addLegend("topright", pal = qpal, values = 1:10,
                      title = "Number of Retail Stores",
                      opacity = 1
            )
        }
        
        else if (input$road.attribute == "road") {
          street.shpfile@data <- cbind(street.shpfile@data, RoadRatingsOnly2015$overall)
          
          atest <- length(unique(street.shpfile@data$`RoadRatingsOnly2015$overall`))
          cpal <- colorFactor(colorRamps::matlab.like(atest), street.shpfile@data$`RoadRatingsOnly2015$overall`)
          
          qpal <- colorNumeric(
            palette = colorRamps::matlab.like(atest),
            domain = NULL
          )
          
          q <- round(quantile(street.shpfile@data$`RoadRatingsOnly2015$overall`, probs = c(0, .25, .75, .95, .99,  1), na.rm = T))
          q <- unname(q)
          
          leaflet() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%
            addPolylines(
              data = street.shpfile,
              color = ~qpal(street.shpfile@data$`RoadRatingsOnly2015$overall`),
              popup = paste0(street.shpfile@data$STREET, ": ", round(street.shpfile@data$`RoadRatingsOnly2015$overall`,2))
            ) %>%
            addLegend("topright", pal = qpal, values = 1:10,
                      title = "Road Rating",
                      opacity = 1
            )
          
        }
        
      })
    
    # Display map for overall street ratings
    
    output$map3 <-
      
      renderLeaflet({
        
        street.shpfile@data <- cbind(street.shpfile@data, FinalRatings$finalRating2)
        
        atest <- length(unique(street.shpfile@data$`FinalRatings$finalRating2`))
        cpal <- colorFactor(rev(heat.colors(atest)), street.shpfile@data$`FinalRatings$finalRating2`)
        
        qpal <- colorNumeric(
          palette = colorRamps::matlab.like(atest),
          domain = NULL
        )
        
        q <- round(quantile(street.shpfile@data$`FinalRatings$finalRating2`, probs = c(0, .25, .75, .95, .99,  1), na.rm = T))
        q <- unname(q)
        
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
          ) %>%
          addPolylines(
            data = street.shpfile,
            color = ~qpal(street.shpfile@data$`FinalRatings$finalRating2`),
            popup = paste0(street.shpfile@data$STREET, ": ", round(street.shpfile@data$`FinalRatings$finalRating2`,2))
          ) %>%
          addLegend("topright", pal = qpal, values = 1:10,
                    title = "Overall Rating",
                    opacity = 1
          )
      })
    
    observe({
      proxy <- leafletProxy("map")
      
      proxy %>% clearControls()
    })
    
  }
)
