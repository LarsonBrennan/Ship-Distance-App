#load libraries to be used in the app
library(shiny)
library(leaflet)
library(RColorBrewer)
library(readr)
library(dplyr)
library(geosphere)
library(purrr)
library(shiny.semantic)
library(ggplot2)


#load the smaller version of the ships dataset and create a dataframe known as twoships
#for vessel type qnd vessel name selection inputs
ships<-shipsSample
twoShips <- data.frame(shipsSample$ship_type, shipsSample$SHIPNAME)
colnames(twoShips) <- c("ship_type", "SHIPNAME")

#ui for the app
ui <- semanticPage(
  #header that displays the title, image, and description at the top of the app when it is running
  header(title = div("SHIP DISTANCE APPLICATION", img(src="image2.png", height = "5%", width = "5%") ), description = "an application for finding longest ship travel distance in a certain period of time",icon = NULL),
  sidebar_layout(sidebar_panel(
    #the htmlOutput attributes are created below with the selection input for choosing the vessel
    #type and vessel name
                                 htmlOutput("typeOfShip_selector", style="font-size:95%"),
                                 htmlOutput("nameOfShip_selector", style="font-size:95%"),
                                 #the h5 elements are for dynamically updating the meters traveled, name of the port,
                                 #and the name of the destination, based on what the htmlOutput attributes are
                                 h5("LONGEST DISTANCE TRAVELED IN METERS:",style="font-size:95%", textOutput("renderDistanceText")),
                                 h5("NAME OF PORT:", style="font-size:95%", textOutput("renderPortText")),
                                 h5("NAME OF DESTINATION:", style="font-size:95%", textOutput("renderDestinationText")),
                                ),
                   
                   #This main panel displays the map with the distance traveled on it
                   main_panel(tags$style(type = "text/css", "#map {height: calc(50vh - 10px) !important;}"),
                              leafletOutput("map", width = "100%", height = "50%")),
                   mirrored = FALSE, min_height = "auto"),
  
  #This sidebar layout is below the map and above selection inputs. It displays
  #two plots, one that shows the how fast the ship was going at certain times
  #and another plot that shows the effect course direction has on speed
  sidebar_layout(
    main_panel(plotOutput("subSetTimeOnSpeedPlot")),
    main_panel(plotOutput("shipCourseOnSpeedPlot")),
    mirrored = FALSE, min_height = "auto")
)


#this is the server section of the app
server <- function(input, output, session) {
  
  #this renderUI portion is for deciding what kind of ship will be selected.
  #it calls the twoShips data frame created at the beginning of the app and
  #selects the type of ship based on that. The default option is a tug boat.
  output$typeOfShip_selector <- renderUI({
    
    selectInput(
      inputId = "typeOfShip", 
      label = "TYPE OF SHIP:",
      choices = as.character(unique(twoShips$ship_type)),
      selected = "Tug")
    
  })
  
  #this is another renderUI portion, and it determines what the ship name is based
  #on user selection. It takes the previously selected ship type, and only displays
  #names of ships that are under that ship type. 
  output$nameOfShip_selector <- renderUI({
    
    available <- twoShips[twoShips$ship_type == input$typeOfShip, "SHIPNAME"]
    
    selectInput(
      inputId = "nameOfShip", 
      label = "NAME OF SHIP:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })

  #This portion of the app renders the top distance and displays the value on the 
  #rshiny app. It calls the distancesDataSet function created below, and puts the 
  #dataframe from that function into a new dataframe called maxValueList. 
  #Columns are then renamed to lat, lon, and istance. Then the top distance is 
  #added to a string variable called topDistance that is then displayed. 
  output$renderDistanceText <- renderText({
    maxValueList<-distancesDataset()
    colnames(maxValueList) <- c("LAT", "LON", "distance")
    topDistance <- toString(maxValueList$distance[2])
    topDistance
  })
  
  #This portion of the app render the name of the port that the ship is from. It calls
  #the getDataSet and inputs the name of the ship the user selects.It then calls the 
  #getPortText function below.
  output$renderPortText <- renderText({
    shipSubSet<-getDataSet(input$nameOfShip)
    getPortText(shipSubSet)
  })
  
  #This portion of the app creates a function that takes in the name of the ship,
  #and it selects for the name of the port. It takes the first row of the dataset
  #and returns the port name from the port variable column.
  getPortText <- function(data) {
    shipSubSet<-data
    shipPort <-shipSubSet$port[1]
    shipPort
  }
  
  #This portion of the app render the name of the destination that the ship is going to. It calls
  #the getDataSet and inputs the name of the ship the user selects.It then calls the 
  #getDestinationText function below.
  output$renderDestinationText <- renderText({
    shipSubSet<-getDataSet(input$nameOfShip)
    getDestinationText(shipSubSet)
  })
  
  #This portion of the app creates a function that takes in the name of the ship,
  #and it selects for the name of the destination. It takes the first row of the dataset
  #and returns the destination name from the destination variable column.
  getDestinationText <- function(data) {
    shipSubSet<-data
    shipPort <-shipSubSet$DESTINATION[1]
    shipPort
  }
  
  #This portion of the app renders the speed of the ship at certain times by displaying
  #a scatterplot diagram of this. It calls the getDataSet function by the name of the ship
  #that the user selects. It then calls the subSetTimeOnSpeed function from below, and
  #inputs the shipSubSet dataframe so the function can be run and the graph can be displayed.
  output$subSetTimeOnSpeedPlot <- renderPlot({
    shipSubSet<-getDataSet(input$nameOfShip)
    subSetTimeOnSpeed(shipSubSet)
  })
  
  #This portion of the app returns a plot to be displayed by the above output$subSetTimeOnSpeedPlot
  #renderPlot output. It creates a dataframe when the input is a dataframe, and it then
  #creates a scatterplot showing the speeds of the ship at certain times.
  subSetTimeOnSpeed <- function(data){
    shipSubSet<-data
    
    myplot<-ggplot(shipSubSet, aes(x=DATETIME, y=SPEED)) + 
      geom_point(shape=18, color="orange", size = 5)
    
    myplot + theme(panel.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  #This portion of the app renders the speed of the ship and how course influences the speed by displaying
  #a scatterplot diagram of this and a simple regression line. It calls the getDataSet function by the name of the ship
  #that the user selects. It then calls the shipCourseOnSpeed function from below, and
  #inputs the shipSubSet dataframe so the function can be run and the graph can be displayed.
  output$shipCourseOnSpeedPlot <- renderPlot({
    shipSubSet<-getDataSet(input$nameOfShip)
    shipCourseOnSpeed(shipSubSet)
  })

  #This portion of the app returns a plot to be displayed by the above output$shipCourseOnSpeedPlot
  #renderPlot output. It creates a dataframe when the input is a dataframe, and it then
  #creates a scatterplot showing the speeds of the ship and how course direction influences
  #the speed. A simple regression is also added to the plot.
  shipCourseOnSpeed <- function(data){
    shipSubSet<-data
    myplot <-ggplot(shipSubSet, aes(x=COURSE, y=SPEED)) + 
      geom_point(shape=18, color="orange", size = 5)+
      geom_smooth(method=lm,  linetype="dashed",
                  color="white", fill="orange")
    
    myplot + theme(panel.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  
  
  
  #This function gets the ship dataset and filters it for a specific ship name 
  #based on the name being called in the function. It then creates a subset dataset
  #based on that name with all of the variables from the normal dataset being used
  getDataSet <- function(nameOfShip) {
    shipName<-nameOfShip
    shipSubSet<-dplyr::filter(ships, SHIPNAME == shipName)
  } 
  
  
  

    
  
  #This reactive piece is for calculating the ship distance and returning the 
  #longitude, latitude, and longest distance for the row with the longest distance
  #and the row before that row
  distancesDataset <- reactive({
    
    #Function for calculating the distances travelled. It is possible to use a 
    #a different function from the geosphere library, but I prefer this function
    #because it is easier to tweak
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
      }
      distance
    }
    
    #call the getDataSet function and input the name of the ship nameOfShipSelector renderUI
    #portion above. Put the dataset into a new value called shipsInitial.
    shipsInitial<-getDataSet(input$nameOfShip)
    
    #take only the LAT and LON variables of the newly created dataframe and put 
    #them into a new dataframe called shipsLatLon
    shipsLatLon<-data.frame(shipsInitial$LAT, shipsInitial$LON)
  
    #find the number of rows in the new dataframe shipsLatLon
    lastRowNumber<-nrow(shipsLatLon)
    
    #create a new dataframe called shipsLatLonFirst and take all of the rows from the first
    #instance to the last
  shipsLatLonFirst <- shipsLatLon[1:lastRowNumber,]
  
  #create a new dataframe called shipsLatLonSecond and take all of the rows from the first
  #instance to the last
  shipsLatLonSecond <- shipsLatLon[1:lastRowNumber,]
  
  #delete the first row of the shipsLatLonFirst dataframe
  shipsLatLonFirst <- shipsLatLonFirst[-1,]
  
  #go throught the shipsLatLonFirst dataframe, and add the values 0,0 to the end for the 
  #lat and lon coordinates.
  shipsLatLonFirst[nrow(shipsLatLonFirst)+1,]<- c(0,0)
  
  #calculate the distances between the shipsLatLonFirst and the shipsLatLonSecond dataframes
  #and put them into a vector called distance
  distance<-get_geo_distance(shipsLatLonFirst$shipsInitial.LON, shipsLatLonFirst$shipsInitial.LAT, shipsLatLonSecond$shipsInitial.LON, shipsLatLonSecond$shipsInitial.LAT, units = "km")
  
  #combine the vector with the dataframe shipsLatLonSecond so a distance column is created
  #with the distance values in that column
  shipsLatLonSecond<-cbind(shipsLatLonSecond, distance)
  
  #the values in the distances column are in km and should be in meters instead. This converts
  #them into meters
  shipsLatLonSecond$distance <- shipsLatLonSecond$distance * 1000
  
  #find the number of rows in the shipsLatLonSecond dataframe
  lastRowNumber<-nrow(shipsLatLonSecond)
  
  #put the distance traveled of 0 at the end of the shipsLatLonSecond distance variable
  #column. The shipsLatLonSecond$distance[lastRowNumber] makes sure that 0 is placed at the end in the right column
  shipsLatLonSecond$distance[lastRowNumber] <- 0
  
  #shifts the values in the distance column up down one place. This is why we placed 0 at the end of the column, so it will be replaced
  #by the final distance traveled value above it.
  shipsLatLonSecond<-shipsLatLonSecond %>%
    mutate(distance=lag(distance))
  
  #places the distance traveled in the first column at 0. This is because this is the initial distance. The ship has not traveled yet.
  #There is no value there now because of the column being shifted down one place, so now we can fill it without replacing
  #a different value that was initially there.
  shipsLatLonSecond$distance[1] <- 0
  
  #find the max value in the distance column and put it into an integer value called maxValue.
  maxValue<-max(shipsLatLonSecond$distance, na.rm = TRUE)
  
  #these three lines of code finds the instances where the maxValue variable is, and then puts the 
  #distance, lon, and lat of the largest distance instances, and the distance, lon, and lat values
  #of the previous row(s). A dataframe called maxValueList is created. Only the last instance of the 
  #maxValue being found in the distances column is added to the maxValueList.
  inds = which(shipsLatLonSecond$distance == maxValue)
  rows <- lapply(inds, function(x) (x-1):(x))
  maxValueList <- shipsLatLonSecond[unlist(rows),]
  })
  
  
  
  
  
  
  #this portion of the app is for the creation of the map that is to be displayed
  output$map <- renderLeaflet({
 
    #this line of code calls the distancesDataset function from above, and takes that dataframe
    #and puts it into a new dataframe called maxValueList
    maxValueList <-  distancesDataset()
    
    #give variable names for the new dataframe created
    colnames(maxValueList) <- c("LAT", "LON", "distance")
    
    #call the leaflet function and create the map
    leaflet() %>% addTiles() %>%
      
      #the addPolylines function creates lines between the lon and the lat of the initial
      #point and the ending point. 
      addPolylines(data = maxValueList,
                       lng = ~LON, 
                       lat = ~LAT,
                       weight = 3,
                       opacity = 3,
                  color = "orange" ) %>%
      
      #this portion adds a circle marker for the initial point on a map
      addCircleMarkers(lng = maxValueList$LON[1], lat = maxValueList$LAT[1], color = "white",
                       label = "STARTING POINT",
                       labelOptions = labelOptions(noHide = T, textsize = "10px",
                                                   style = list("color" = "orange")) 
                       ) %>%
      
      #this portion adds a circle marker for the ending point on a map
    addCircleMarkers(lng = maxValueList$LON[2], lat = maxValueList$LAT[2],  color = "white",
                     label = "END POINT",
                     labelOptions = labelOptions(noHide = T, textsize = "10px",
                                                 style = list("color" = "orange")
                                               ) 
    )
  })
}

shinyApp(ui, server)
