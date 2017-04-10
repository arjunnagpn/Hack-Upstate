# Arjun Nag Puttaiah Nagaraja,
# MS in Information Management,
# CAS in Data Science,
# Syracuse University - Spring 2017
# Oct 9, 2016

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

predictions2017.df <- read.csv("predictions2017.df.csv")

predictions2017.df <- predictions2017[predictions2017$Major >=0.5,]
uniqpredictions2017.df <- predictions2017.df[!duplicated(predictions2017.df$address),]

my_crime_Data_final2016 <- read.csv("second_crimeData_final2016.csv")

my_crime_Data_final2015 <- read.csv("second_crimeData_final2015.csv")

HealthCarePr <- read.csv("HealthCarePr.csv")

Retail_Food_Stores_final <- read.csv("Retail_Food_Stores_final.csv")

RoadRatingsOnly2015 <- read.csv("RoadRatingsOnly2015.csv")

FinalRatings <- read.csv("FinalRatings.csv")

street.shpfile <- readRDS("street.shpfile")

# Read "potholes (1).csv" file:
# Taken from: https://cityofsyracuse.github.io/RoadsChallenge/data/potholes.csv

# potholes.load <- read.csv("potholes (1).csv")
# 
# # Clean the Data
# 
# potholes.load$StreetNumber <- as.numeric(potholes.load$StreetNumber)
# 
# for (i in c(2:5,6,8)) {
#   potholes.load[,i] <- as.factor(trimws(potholes.load[,i]))
# }
# 
# potholes.load$date <- stringr::str_split_fixed(potholes.load$dtTime, " ", 2)[,1]
# 
# potholes.load$month <- stringr::str_split_fixed(potholes.load$date, "/", 3)[,1]
# potholes.load$year <- stringr::str_split_fixed(potholes.load$date, "/", 3)[,3]
# potholes.load$caldate <- stringr::str_split_fixed(potholes.load$date, "/", 3)[,2]
# 
# potholes.load$fixdate <- paste0(potholes.load$year, "-", potholes.load$month, "-", potholes.load$caldate)
# potholes.load$street <- paste0(potholes.load$StreetName, " ", potholes.load$StreetNamePostType)
# 
# potholes.load$latitude <- potholes.load$Longitude
# potholes.load$longitude <- potholes.load$Latitude
# 
# potholes.graph <- potholes.load
# potholes.graph$year <- as.numeric(potholes.graph$year)
# potholes.graph$month <- as.numeric(potholes.graph$month)
# 
# all.years <- unique(potholes.graph$year)
# 
# sim.potholes.load <- 
#   potholes.load[,c(
#     "STREET_ID", 
#     "StreetNumber", 
#     "StreetName", 
#     "StreetNamePostType", 
#     "street", 
#     "strLocation", 
#     "VehicleName", 
#     "fixdate", 
#     "latitude", 
#     "longitude")]
# 
# potholes.df <- sim.potholes.load
# 
# pothole.st <- as.data.frame(table(sim.potholes.load$street))
# colnames(pothole.st) <- c("Street Name", "Number of Potholes")
# pothole.st <- pothole.st[rev(order(pothole.st$`Number of Potholes`)),]
# rownames(pothole.st) <- NULL
# 
# mainstreets <- as.character(pothole.st$`Street Name`)
# allstreets <- c("All", mainstreets)
# 
# month.map <- data.frame()
# 
# # Read the Street Map
# # Taken from: https://cityofsyracuse.github.io/RoadsChallenge/data/street_shapefile.zip
# # File: streets.shp
# 
# ### !!!!! ###
# # AFTER DOWNLOADING THIS FILE, PLEASE SAVE IT AS A RDS FILE (USING saveRDS() FUNCTION IN R) AND THEN LOAD IT (USING loadRDS()). 
# # DOING SO WILL DRAMATICALLY INCREASE PERFORMANCE AND MINIMIZE LOAD TIME DURING APP STARTUP
# # THANK YOU
# ### !!!!! ###
# 
# # The following steps were executed for saving file as RDS:
# 
# # street.shpfile <- readOGR(".", "streets")
# # street.shpfile <- spTransform(street.shpfile, CRS("+proj=longlat +ellps=GRS80"))
# # saveRDS(street.shpfile, "street.shpfile.shp")
# 
# # Now, read the above file as RDS for faster performance:
# 
# street.shpfile <- readRDS("street.shpfile.shp")
# 
# # Read thelast 5 years street data files:
# # RoadRatings2011.csv (Taken From: https://cityofsyracuse.github.io/RoadsChallenge/data/roads/RoadRatings2011.csv)
# # RoadRatings2012.csv (Taken From: https://cityofsyracuse.github.io/RoadsChallenge/data/roads/RoadRatings2012.csv)
# # RoadRatings2013.csv (Taken From: https://cityofsyracuse.github.io/RoadsChallenge/data/roads/RoadRatings2013.csv)
# # RoadRatings2014.csv (Taken From: https://cityofsyracuse.github.io/RoadsChallenge/data/roads/RoadRatings2014.csv)
# # RoadRatings2015.csv (Taken From: https://cityofsyracuse.github.io/RoadsChallenge/data/roads/RoadRatings2015.csv)
# 
# # road2011.df <- read.csv("RoadRatings2011.csv")[,c(
# #   "streetName",
# #   "streetType",
# #   "overall",
# #   "crack",
# #   "patch",
# #   "flushOil",
# #   "class",
# #   "pavement",
# #   "streetID"
# # )]
# # road2011.df$year <- 2011
# # 
# # road2012.df <- read.csv("RoadRatings2012.csv")[,c(
# #   "streetName",
# #   "streetType",
# #   "overall",
# #   "crack",
# #   "patch",
# #   "flushOil",
# #   "class",
# #   "pavement",
# #   "streetID"
# # )]
# # road2012.df$year <- 2012
# # 
# # road2013.df <- read.csv("RoadRatings2013.csv")[,c(
# #   "streetName",
# #   "streetType",
# #   "overall",
# #   "crack",
# #   "patch",
# #   "flushOil",
# #   "class",
# #   "pavement",
# #   "streetID"
# # )]
# # road2013.df$year <- 2013
# # 
# # road2014.df <- read.csv("RoadRatings2014.csv")[,c(
# #   "streetName",
# #   "streetType",
# #   "overall",
# #   "crack",
# #   "patch",
# #   "flushOil",
# #   "class",
# #   "pavement",
# #   "streetID"
# # )]
# # road2014.df$year <- 2014
# # 
# # road2015.df <- read.csv("RoadRatings2015.csv")[,c(
# #   "streetName",
# #   "streetType",
# #   "overall",
# #   "crack",
# #   "patch",
# #   "flushOil",
# #   "class",
# #   "pavement",
# #   "streetID"
# # )]
# # road2015.df$year <- 2015
# # 
# # road.df <- rbind(
# #   road2011.df,
# #   road2012.df,
# #   road2013.df,
# #   road2014.df,
# #   road2015.df
# # )
# 
# # The above variables: road2011.df, road2012.df, road2013.df, road2014.df, road2015.df were combined into one variable: road.df.
# # road.df was saved as RDS to increase performance using the steps mentioned below:
# # saveRDS(road.df, "road.df")
# # Read the above variable as RDS for faster performance:
# 
# road.df <- readRDS("road.df")
# 
# # Clean the Data
# 
# for (i in c(1,2,6:8)) {
#   road.df[,i] <- trimws(road.df[,i])
# }
# road.df[road.df$flushOil == "",]$flushOil <- "None"
# road.df[road.df$flushOil == "?",]$flushOil <- "Not Available"
# 
# road.df[road.df$class == "",]$class <- "None"
# 
# road.df[road.df$pavement == "",]$pavement <- "None"
# 
# all.years2 <- unique(road.df$year)
# 
# all.oil <- c("All", unique(road.df$flushOil))
# 
# all.class <- c("All", unique(road.df$class))
# 
# all.pavement <- c("All", unique(road.df$pavement))
# 
# temp.street.df <- data.frame(
#   STREET_ID = street.shpfile@data$STREET_ID,
#   ind = 1:nrow(street.shpfile@data)
# )
# 
# # Prepare Data for Displaying in Dashboard
# 
# potholes.st <- as.data.frame(table(potholes.load$street))
# potholes.st <- potholes.st[rev(order(potholes.st$Freq)),]
# highest.potholes.st <- paste0(potholes.st[1,1], " - ", potholes.st[1,2])
# 
# potholes.veh <- as.data.frame(table(potholes.load$VehicleName))
# potholes.veh <- potholes.veh[rev(order(potholes.veh$Freq)),]
# highest.potholes.veh <- paste0(potholes.veh[1,1], " - ", potholes.veh[1,2])
# 
# low.road.sat <- road.df
# low.road.sat$street <- paste0(low.road.sat$streetName, " ", low.road.sat$streetType)
# low.road.sat <- low.road.sat[low.road.sat$overall == 0 | low.road.sat$overall == 1,]$street
# low.road.sat <- unique(low.road.sat)
# low.road.sat.st <- low.road.sat[!is.na(low.road.sat)][1]
# 
# low.crack.sat <- road.df
# low.crack.sat$street <- paste0(low.crack.sat$streetName, " ", low.crack.sat$streetType)
# low.crack.sat <- low.crack.sat[low.crack.sat$crack == 0 | low.crack.sat$crack == 1,]$street
# low.crack.sat <- unique(low.crack.sat)
# low.crack.sat.st <- low.crack.sat[!is.na(low.crack.sat)][1]
# 
# low.patch.sat <- road.df
# low.patch.sat$street <- paste0(low.patch.sat$streetName, " ", low.patch.sat$streetType)
# low.patch.sat <- low.patch.sat[low.patch.sat$patch == 0 | low.patch.sat$patch == 1,]$street
# low.patch.sat <- unique(low.patch.sat)
# low.patch.sat.st <- low.patch.sat[!is.na(low.patch.sat)][1]

shinyServer(
  
  function(input, output, session) {
    
    # filtered.pot.graph <- reactive({
    #   
    #   potholes.graph <- potholes.graph[potholes.graph$year == input$year,]
    #   potholes.graph <- potholes.graph[potholes.graph$month == input$month,]
    #   
    # })
    # 
    # # Data for Dashboard
    # 
    # output$progressBox1a <- renderValueBox({
    #   valueBox(
    #     nrow(potholes.load[potholes.load$year == 2016,]), 
    #     "Potholes Fixed This Year", 
    #     icon = icon("road"),
    #     color = "purple"
    #   )
    # })
    # 
    # output$progressBox1b <- renderValueBox({
    #   valueBox(
    #     highest.potholes.st, 
    #     "Highest Potholes Fixed", 
    #     icon = icon("building-o"),
    #     color = "purple"
    #   )
    # })
    # 
    # output$progressBox1c <- renderValueBox({
    #   valueBox(
    #     highest.potholes.veh, 
    #     "Durapatcher With Most Potholes Fixed", 
    #     icon = icon("bus"),
    #     color = "purple"
    #   )
    # })
    # 
    # output$progressBox2a <- renderValueBox({
    #   valueBox(
    #     low.road.sat.st, 
    #     "Street With Least Overall Satisfaction", 
    #     icon = icon("thumbs-o-down"),
    #     color = "purple"
    #   )
    # })
    # 
    # output$progressBox2b <- renderValueBox({
    #   valueBox(
    #     low.crack.sat.st, 
    #     "Street With Least Crack Rating", 
    #     icon = icon("chain-broken"),
    #     color = "purple"
    #   )
    # })
    # 
    # output$progressBox2c <- renderValueBox({
    #   valueBox(
    #     low.patch.sat.st, 
    #     "Street With Least Patch Rating", 
    #     icon = icon("repeat"),
    #     color = "purple"
    #   )
    # })
    # 
    # # Dynamic Input Widgets
    # 
    # output$o.year <- renderUI({
    #   selectInput(
    #     "year",
    #     "Select Year",
    #     choices = all.years,
    #     selected = all.years[1]
    #   )
    # })
    # 
    # output$o.street.abb2 <- renderUI({
    #   selectizeInput(
    #     'street.abb2', 
    #     'Search Street',
    #     choices = allstreets,
    #     multiple = TRUE,
    #     selected = allstreets[2:6]
    #   )
    # })
    # 
    # output$o.street.abb <- renderUI({
    #   selectizeInput(
    #     'street.abb', 
    #     'Search Street',
    #     choices = allstreets,
    #     multiple = TRUE,
    #     selected = "All"
    #   )
    # })
    # 
    # output$o.year2 <- renderUI({
    #   selectInput(
    #     "year2",
    #     "Select Year",
    #     choices = all.years2,
    #     selected = all.years2[5]
    #   )
    # })
    # 
    # output$o.oil <- renderUI({
    #   selectizeInput(
    #     "oil",
    #     "Flush Oil Type",
    #     choices = all.oil,
    #     multiple = T,
    #     selected = "All"
    #   )
    # })
    # 
    # output$o.class <- renderUI({
    #   selectizeInput(
    #     "class",
    #     "Class Type",
    #     choices = all.class,
    #     multiple = T,
    #     selected = "All"
    #   )
    # })
    # 
    # output$o.pavement <- renderUI({
    #   selectizeInput(
    #     "pavement",
    #     "Pavement Type",
    #     multiple = T,
    #     choices = all.pavement,
    #     selected = "All"
    #   )
    # })
    # 
    # # Prepare data for Potholes Map
    # 
    # filtered.pot.df <- reactive({
    #   
    #   if (is.null(input$scatter.type)) return()
    #   
    #   if (is.null(input$bus.type)) return()
    #   
    #   if (is.null(input$fix.date)) return()
    #   
    #   if (any(input$street.abb %in% "All")) {
    #     potholes.df <- potholes.df
    #   }
    #   else if (!any((input$street.abb %in% "All"))) {
    #     potholes.df <- potholes.df[potholes.df$street %in% input$street.abb,]
    #   }
    #   
    #   if (input$bus.type == "Both") {
    #     potholes.df <- potholes.df
    #   }
    #   else if (input$bus.type == "DP1") {
    #     potholes.df <- potholes.df[potholes.df$VehicleName == "DP1",]
    #   }
    #   else if (input$bus.type == "DP2") {
    #     potholes.df <- potholes.df[potholes.df$VehicleName == "DP2",]
    #   }
    #   
    #   potholes.df <- potholes.df[
    #     (input$fix.date[1] <= as.Date(potholes.df$fixdate)) &
    #     (input$fix.date[2] >= as.Date(potholes.df$fixdate)),]
    #   
    #   return(potholes.df)
    #   
    # })
    # 
    # # Prepare data for Potholes Map - Longitude
    # 
    # filtered.lon <- reactive({
    #   mean(filtered.pot.df()$longitude)
    # })
    # 
    # # Prepare data for Potholes Map - Latitude
    # 
    # filtered.lat <- reactive({
    #   mean(filtered.pot.df()$latitude)
    # })
    # 
    # # Prepare data for Street Map
    # 
    # filtered.road.df <- reactive({
    #   
    #   if (is.null(input$year2)) return()
    #   
    #   if (is.null(input$oil)) return()
    #   
    #   if (is.null(input$class)) return()
    #   
    #   if (is.null(input$pavement)) return()
    #   
    #   if (any(input$year2 %in% "All")) {
    #     road.df <- road.df
    #   }
    #   else if (!any((input$year2 %in% "All"))) {
    #     road.df <- road.df[road.df$year %in% input$year2,]
    #   }
    #   
    #   else if (any(input$oil %in% "All")) {
    #     road.df <- road.df
    #   }
    #   else if (!any((input$oil %in% "All"))) {
    #     road.df <- road.df[road.df$flushOil %in% input$oil,]
    #   }
    #   
    #   else if (any(input$class %in% "All")) {
    #     road.df <- road.df
    #   }
    #   else if (!any((input$class %in% "All"))) {
    #     road.df <- road.df[road.df$class %in% input$class,]
    #   }
    #   
    #   else if (any(input$pavement %in% "All")) {
    #     road.df <- road.df
    #   }
    #   else if (!any((input$pavement %in% "All"))) {
    #     road.df <- road.df[road.df$pavement %in% input$pavement,]
    #   }
    #   
    #   return(road.df)
    #   
    # })
    # 
    # # Prepare data for Potholes Graph - Ranking widget
    # 
    # output$ranking <- renderUI({
    #   
    #   if (is.null(input$year)) return()
    #   
    #   if (is.null(input$month)) return()
    #   
    #   potholes.graph <- potholes.graph[potholes.graph$year == input$year,]
    #   potholes.graph <- potholes.graph[potholes.graph$month == input$month,]
    #   
    #   if (nrow(potholes.graph)==0) return()
    #   
    #   x.df3 <- table(potholes.graph$street)
    #   x.df3.df <- as.data.frame(x.df3)
    #   x.df3.df <- x.df3.df[rev(order(x.df3.df$Freq)),]
    #   rownames(x.df3.df) <- NULL
    #   
    #   if (nrow(x.df3.df) >=5){
    #     sliderInput("rank", 
    #                 "Choose Rank", 
    #                 min = 1, 
    #                 max = nrow(x.df3.df), 
    #                 value = c(1, 5), 
    #                 step = 1
    #     )
    #   }
    #   
    # })
    # 
    # # Bargraph cwhich displays streets with highest potholes
    # 
    # output$o.bar1 <- renderPlot({
    #   
    #   if (is.null(input$year)) return()
    #   
    #   if (is.null(input$month)) return()
    #   
    #   if (is.null(input$rank)) return()
    #   
    #   potholes.graph <- potholes.graph[potholes.graph$year == input$year,]
    #   potholes.graph <- potholes.graph[potholes.graph$month == input$month,]
    #   
    #   if (nrow(potholes.graph)==0) return()
    #   
    #   x.df3 <- table(potholes.graph$street)
    #   x.df3.df <- as.data.frame(x.df3)
    #   x.df3.df <- x.df3.df[rev(order(x.df3.df$Freq)),]
    #   rownames(x.df3.df) <- NULL
    #   
    #   x.df3.df <- x.df3.df[input$rank[1]:input$rank[2],]
    #   
    #   mar.default <- c(5,4,4,2) + 0.1
    #   par(mar = mar.default + c(6, 0, 0, 0)) 
    #   
    #   b <- barplot(
    #     x.df3.df$Freq,
    #     border = 'white',
    #     xaxt='n', 
    #     ann=FALSE,
    #     col = "darkorange",
    #     ylab = "Potholes"
    #   )
    #   text(
    #     b, 
    #     par("usr")[3], 
    #     labels = x.df3.df$Var1, 
    #     srt = 45, 
    #     adj = c(1.1,1.1), 
    #     xpd = TRUE
    #   )
    #   axis(2)
    # })
    # 
    # # Stacked Bargraph which displays potholes v/s street names
    # 
    # output$o.bar2 <- renderPlot({
    #   
    #   if (is.null(input$year)) return()
    #   
    #   if (is.null(input$month)) return()
    #   
    #   if (is.null(input$rank)) return()
    #   
    #   if (is.null(input$street.abb2)) return()
    #   
    #   if (any(input$street.abb2 %in% "All")) {
    #     potholes.graph <- potholes.load
    #     potholes.graph$year <- as.numeric(potholes.graph$year)
    #     potholes.graph$month <- as.numeric(potholes.graph$month)
    #   }
    #   
    #   else if (!(any(input$street.abb2 %in% "All"))) {
    #     potholes.graph <- potholes.graph[potholes.graph$street %in% input$street.abb2,]
    #     potholes.graph <- potholes.graph[potholes.graph$year == input$year,]
    #     potholes.graph <- potholes.graph[potholes.graph$month == input$month,]
    #   }
    #   
    #   if (nrow(potholes.graph)==0) return()
    #   
    #   x.df3 <- table(potholes.graph$VehicleName, potholes.graph$street)
    #   
    #   x.df3.df <- as.data.frame(x.df3)
    #   
    #   mar.default <- c(5,4,4,2) + 0.1
    #   par(mar = mar.default + c(6, 0, 0, 0)) 
    #   
    #   b <- barplot(
    #     x.df3,
    #     main="",
    #     xlab="",
    #     col = c("darkviolet", "darkorange"),
    #     border = 'white',
    #     legend = rownames(x.df3),
    #     ylab = "Potholes",
    #     xaxt='n'
    #     )
    #   text(
    #     b, 
    #     par("usr")[3], 
    #     labels = unique(x.df3.df$Var2), 
    #     srt = 45, 
    #     adj = c(1.1,1.1), 
    #     xpd = TRUE
    #   )
    #   axis(2)
    #   
    # })
    # 
    # # Data Table for Potholes
    # 
    # output$table.df <- DT::renderDataTable({
    #   
    #   if (is.null(input$scatter.type)) return()
    #   
    #   if (is.null(input$bus.type)) return()
    #   
    #   if (is.null(input$fix.date)) return()
    #   
    #   filtered.pot.tab <- filtered.pot.df()
    #   
    #   filtered.pot.tab <- filtered.pot.tab[,c(
    #     "street",
    #     "strLocation",
    #     "VehicleName",
    #     "fixdate"
    #   )]
    #   
    #   colnames(filtered.pot.tab) <- c(
    #     "Street Name",
    #     "Address",
    #     "Durapatch Truck",
    #     "Date Fixed"
    #   )
    #   
    #   return(filtered.pot.tab)
    #   
    # })
    # 
    # # Data Table for Streets
    # 
    # output$table.df2 <- DT::renderDataTable({
    #   
    #   if (is.null(input$road.attribute)) return()
    #   
    #   if (is.null(input$year2)) return()
    #   
    #   if (is.null(input$oil)) return()
    #   
    #   if (is.null(input$class)) return()
    #   
    #   if (is.null(input$pavement)) return()
    #   
    #   filtered.road.tab.temp <- filtered.road.df()
    #   filtered.road.tab.temp$street <- paste0(
    #     filtered.road.tab.temp$streetName, 
    #     " ",
    #     filtered.road.tab.temp$streetType
    #   )
    #   
    #   filtered.road.tab <- filtered.road.tab.temp[,c(
    #     "street",
    #     "overall",
    #     "crack",
    #     "patch",
    #     "flushOil",
    #     "class",
    #     "pavement",
    #     "year"
    #   )]
    #   
    #   colnames(filtered.road.tab) <- c(
    #     "Street",
    #     "Overall Rating",
    #     "Crack Rating",
    #     "Patch Rating",
    #     "Oil Category",
    #     "Class Type",
    #     "Pavement Type",
    #     "Year"
    #   )
    #   
    #   rownames(filtered.road.tab) <- NULL
    #   
    #   return(filtered.road.tab)
    #   
    # })
    
    
    # Display Map for Potholes
    
    output$map <- 
      
      renderLeaflet({
        
        # if (is.null(input$scatter.type)) return()
        # 
        # if (is.null(input$bus.type)) return()
        # 
        # if (is.null(input$fix.date)) return()
        # 
        # if (input$scatter.type == 1) {
        #   
        #   leaflet(filtered.pot.df()) %>% 
        #     setView(lng = filtered.lon(), lat = filtered.lat(), zoom = 11) %>%
        #     addTiles(
        #       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
        #     ) %>%
        #     addCircleMarkers(
        #       radius = 5,
        #       color = "darkorange",
        #       fillColor = "darkorange", 
        #       stroke = FALSE, 
        #       fillOpacity = 0.5,
        #       clusterOptions = markerClusterOptions(),
        #       popup = paste0(filtered.pot.df()$strLocation)
        #     ) 
        # }
        # 
        # else if (input$scatter.type == 2) {
        #   
        #   leaflet(filtered.pot.df()) %>% 
        #     setView(lng = filtered.lon(), lat = filtered.lat(), zoom = 11) %>%
        #     addTiles(
        #       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
        #     ) %>%
        #     addCircles(
        #       radius = ~25,
        #       weight = 1, 
        #       color = "darkorange",
        #       fillColor = "darkorange", 
        #       fillOpacity = 0.5, 
        #       popup = paste0(filtered.pot.df()$strLocation)
        #     )
        # }
        
        
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
    
    
    # Display Map for Streets

    output$map2 <-

      renderLeaflet({

        # if (is.null(input$road.attribute)) return()
        # 
        # if (is.null(input$year2)) return()
        # 
        # if (is.null(input$oil)) return()
        # 
        # if (is.null(input$class)) return()
        # 
        # if (is.null(input$pavement)) return()

        # f.road.df <- filtered.road.df()

        # road.df.mod <- f.road.df[,c("overall", "crack", "patch", "streetID")]
        # 
        # for (i in c(1:3)) {
        #   road.df.mod[is.na(road.df.mod[,i]),] <- 0
        # }
        # 
        # length(unique(road.df.mod$streetID))
        # 
        # road.df.mod <- road.df.mod[,c(input$road.attribute, "streetID")]
        # colnames(road.df.mod) <- c("att", "streetID")
        # road.df.mod <- group_by(road.df.mod, streetID) %>%
        #   summarize(attri = median(att))


        # merged.road.df <- merge(x = road.df.mod, y = temp.street.df, by.x = "streetID", by.y = "STREET_ID", all.y = T)
        # 
        # merged.road.df <- merged.road.df[order(merged.road.df$ind),]
        # 
        # merged.road.df[is.na(merged.road.df$attri),]$attri <- 0
        # merged.road.df$attri <- as.integer(merged.road.df$attri)
        # merged.road.df.2 <- as.data.frame(merged.road.df$attri)
        # 
        # street.shpfile@data <- cbind(street.shpfile@data, merged.road.df.2)

        # atest <- length(unique(street.shpfile@data$`merged.road.df$attri`))
        # cpal <- colorFactor(rev(heat.colors(atest)), street.shpfile@data$`merged.road.df$attri`)
        # 
        # qpal <- colorNumeric(
        #   palette = rev(heat.colors(atest)),
        #   domain = street.shpfile@data$`merged.road.df$attri`
        # )
        # 
        # q <- round(quantile(street.shpfile@data$`merged.road.df$attri`, probs = c(0, .25, .75, .95, .99,  1), na.rm = T))
        # q <- unname(q)
        # 
        # leaflet() %>%
        #   addTiles() %>%
        #   addPolylines(
        #     data = street.shpfile,
        #     color = ~cpal(street.shpfile@data$`merged.road.df$attri`),
        #     popup = paste0(street.shpfile@data$STREET, ": ", street.shpfile@data$`merged.road.df$attri`)
        #   ) %>%
        #   addLegend("topright", pal = qpal, values = q,
        #             title = "Rating",
        #             opacity = 1
        #   )
        
        if (input$road.attribute == "healthcare") {
          street.shpfile@data <- cbind(street.shpfile@data, HealthCarePr$HCRating)
          
          atest <- length(unique(street.shpfile@data$`HealthCarePr$HCRating`))
          cpal <- colorFactor(colorRamps::matlab.like(atest), street.shpfile@data$`HealthCarePr$HCRating`)
          
          qpal <- colorNumeric(
            # palette = rev(heat.colors(atest)),
            palette = colorRamps::matlab.like(atest),
            # domain = street.shpfile@data$`HealthCarePr$HCRating`
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
            # palette = rev(heat.colors(atest)),
            palette = rev(colorRamps::matlab.like(atest)),
            # domain = street.shpfile@data$`Retail_Food_Stores_final$scaled_Count_Less1`
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
            # palette = rev(heat.colors(atest)),
            palette = colorRamps::matlab.like(atest),
            # domain = street.shpfile@data$`RoadRatingsOnly2015$overall`
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
    
    output$map3 <-
      
      renderLeaflet({
        
        street.shpfile@data <- cbind(street.shpfile@data, FinalRatings$finalRating2)
        
        atest <- length(unique(street.shpfile@data$`FinalRatings$finalRating2`))
        cpal <- colorFactor(rev(heat.colors(atest)), street.shpfile@data$`FinalRatings$finalRating2`)
        
        qpal <- colorNumeric(
          # palette = rev(heat.colors(atest)),
          palette = colorRamps::matlab.like(atest),
          # domain = street.shpfile@data$`FinalRatings$finalRating2`
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
