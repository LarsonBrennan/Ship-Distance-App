library(shiny)
library(leaflet)
library(RColorBrewer)
library(readr)
library(dplyr)
library(geosphere)
library(purrr)
library(shiny.semantic)
library(ggplot2)
#ships <- read_csv("~/Desktop/rprojectsforresume/appsilonJobProject/ships.csv")

ui <- semanticPage(
  header(title = div("SHIP DISTANCE APPLICATION", img(src="image2.png", height = "5%", width = "5%") ), description = "an application for finding longest ship distance in a certain period of time",icon = NULL),
  sidebar_layout(sidebar_panel(
                                 htmlOutput("typeOfShip_selector", style="color:#FFA319;font-size:95%"),
                                 htmlOutput("nameOfShip_selector", style="color:#FFA319;font-size:95%"),
                                 h5("LONGEST DISTANCE TRAVELED IN METERS:",style="font-size:95%", textOutput("renderDistanceText")),
                                 h5("NAME OF PORT:", style="font-size:95%", textOutput("renderPortText")),
                                 h5("NAME OF DESTINATION:", style="font-size:95%", textOutput("renderDestinationText")),
                                ),
                   main_panel(tags$style(type = "text/css", "#map {height: calc(50vh - 10px) !important;}"),
                              leafletOutput("map", width = "100%", height = "50%")),
                   mirrored = FALSE, min_height = "auto"),

  sidebar_layout(
    main_panel(plotOutput("subSetTimeOnSpeedPlot")),
    main_panel(plotOutput("shipTypeOnSpeedPlot")),
    mirrored = FALSE, min_height = "auto")
)



server <- function(input, output, session) {
  output$typeOfShip_selector <- renderUI({
    
    selectInput(
      inputId = "typeOfShip", 
      label = "TYPE OF SHIP:",
      choices = as.character(unique(twoShips$ship_type)),
      selected = "Tug")
    
  })
  
  output$nameOfShip_selector <- renderUI({
    
    available <- twoShips[twoShips$ship_type == input$typeOfShip, "SHIPNAME"]
    
    selectInput(
      inputId = "nameOfShip", 
      label = "NAME OF SHIP:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })

  getDataSet <- function(nameOfShip) {
    shipName<-nameOfShip
    shipSubSet<<-dplyr::filter(ships, SHIPNAME == shipName)
  } 


  
  output$renderDistanceText <- renderText({
    maxValueList<-distancesDataset()
    colnames(maxValueList) <- c("LAT", "LON", "distance")
    topDistance <- toString(maxValueList$distance[2])
    topDistance
  })
  
  output$renderPortText <- renderText({
    shipSubSet<-getDataSet(input$nameOfShip)
    getPortText(shipSubSet)
  })
  
  getPortText <- function(data) {
    shipSubSet<-data
    shipPort <-shipSubSet$port[1]
    shipPort
  }
  
  output$renderDestinationText <- renderText({
    shipSubSet<-getDataSet(input$nameOfShip)
    getDestinationText(shipSubSet)
  })
  
  getDestinationText <- function(data) {
    shipSubSet<-data
    shipPort <-shipSubSet$DESTINATION[1]
    shipPort
  }
  
  output$subSetTimeOnSpeedPlot <- renderPlot({
    shipSubSet<-getDataSet(input$nameOfShip)
    subSetTimeOnSpeed(shipSubSet)
  })
  
  subSetTimeOnSpeed <- function(data){
    shipSubSet<-data
    
    myplot<-ggplot(shipSubSet, aes(x=DATETIME, y=SPEED)) + 
      geom_point(shape=18, color="orange", size = 5)
    
    myplot + theme(panel.background = element_rect(fill = 'white', colour = 'white'))
  }
  output$shipTypeOnSpeedPlot <- renderPlot({
    shipSubSet<-getDataSet(input$nameOfShip)
    shipTypeOnSpeed(shipSubSet)
  })
  
  shipTypeOnSpeed <- function(data){
    shipSubSet<-data
    myplot <-ggplot(shipSubSet, aes(x=COURSE, y=SPEED)) + 
      geom_point(shape=18, color="orange", size = 5)+
      geom_smooth(method=lm,  linetype="dashed",
                  color="white", fill="orange")
    
    myplot + theme(panel.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  
  
  
  distancesDataset <- reactive({
    shipsInitial<-getDataSet(input$nameOfShip)
    shipsLatLon<-data.frame(shipsInitial$LAT, shipsInitial$LON)
    
    
    
    get_geo_distance = function(long1, lat1, long2, lat2, units = "km") {
      loadNamespace("purrr")
      loadNamespace("geosphere")
      longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
      longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
      distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
      distance_m = sapply(distance_list, function(col) { col[1] })
      if (units == "km") {
        distance = distance_m / 1000.0;
      }
      else if (units == "miles") {
        distance = distance_m / 1609.344
      }
      else {
        distance = distance_m
        # This will return in meter as same way as distHaversine function. 
      }
      distance
    }
    
    lastRowNumber<-nrow(shipsLatLon)
  shipsLatLonFirst <- shipsLatLon[1:lastRowNumber,]
  shipsLatLonSecond <- shipsLatLon[1:lastRowNumber,]
  shipsLatLonFirst <- shipsLatLonFirst[-1,]
  
  shipsLatLonFirst[nrow(shipsLatLonFirst)+1,]<- c(0,0)
  
  distance<-get_geo_distance(shipsLatLonFirst$shipsInitial.LON, shipsLatLonFirst$shipsInitial.LAT, shipsLatLonSecond$shipsInitial.LON, shipsLatLonSecond$shipsInitial.LAT, units = "km")
  shipsLatLonSecond<-cbind(shipsLatLonSecond, distance)
  shipsLatLonSecond$distance <- shipsLatLonSecond$distance * 1000
  lastRowNumber<-nrow(shipsLatLonSecond)
  shipsLatLonSecond$distance[lastRowNumber] <- 0
  shipsLatLonSecond<-shipsLatLonSecond %>%
    mutate(distance=lag(distance))
  shipsLatLonSecond$distance[1] <- 0
  
  distancesData <- shipsLatLonSecond
  
  newDataSet <- distancesData
 
  
  maxValue<-max(shipsLatLonSecond$distance, na.rm = TRUE)
  
  inds = which(shipsLatLonSecond$distance == maxValue)
  # We use lapply() to get all rows for all indices, result is a list
  rows <- lapply(inds, function(x) (x-1):(x))
  # With unlist() you get all relevant rows
  maxValueList <<- shipsLatLonSecond[unlist(rows),]
  })
  
  

  output$map <- renderLeaflet({
    
    maxValueList <-  distancesDataset()
    colnames(maxValueList) <- c("LAT", "LON", "distance")

    leaflet() %>% addTiles() %>%
      addPolylines(data = maxValueList,
                       lng = ~LON, 
                       lat = ~LAT,
                       weight = 3,
                       opacity = 3,
                  color = "orange" ) %>%
      
      addCircleMarkers(lng = maxValueList$LON[1], lat = maxValueList$LAT[1], color = "white",
                       label = "STARTING POINT",
                       labelOptions = labelOptions(noHide = T, textsize = "10px",
                                                   style = list("color" = "orange")) 
                       ) %>%
    addCircleMarkers(lng = maxValueList$LON[2], lat = maxValueList$LAT[2],  color = "white",
                     label = "END POINT",
                     labelOptions = labelOptions(noHide = T, textsize = "10px",
                                                 style = list("color" = "orange")
                                                 ) 
    )

  })
  
}

shinyApp(ui, server)
