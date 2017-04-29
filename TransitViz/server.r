library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit

# 1=South, 2=East, 3=West, 4=North
dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")

# Download data from the Twin Cities Metro Transit API
# http://svc.metrotransit.org/NexTrip/help
getMetroData <- function(path) {
  url <- paste0("http://svc.metrotransit.org/NexTrip/", path, "?format=json")
  jsonlite::fromJSON(url)
}

server <- function(input, output, session) {
  
  v <- reactiveValues(msg = "")

  # Route select input box
  output$routeSelectrADIO <- renderUI({

    routeNums <- linesRelevent
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    
    
    radioButtons("routeNum2", label = h3("Select Route"),
                 choices = routeNums, 
                 selected = routeNums[2])
    
  })
  
  output$routeSelect <- renderUI({
    
    routeNums <- linesRelevent
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
  })
    
  
output$busmap <- renderLeaflet({
    
    map <- leaflet() %>%
      addProviderTiles('Esri.WorldStreetMap',group='EsriStreet') %>%
      addProviderTiles('Esri.WorldImagery',group='EsriImagery') %>%
      addProviderTiles('CartoDB.Positron',group='CartoDB-1') %>% 
      addLayersControl(
        baseGroups = c('CartoDB-1','EsriStreet','EsriImagery'),
        overlayGroups = c('lines'),
        options = layersControlOptions(collapsed = T)  )
    
    if (!is.null(input$routeNum2) ){
    if(input$routeNum2!='0'){
      
      dispMessage <- paste0('Name: ',paste0(unlist(lineObj[[ input$routeNum2 ]]$lineNames),collapse = ' ') )
      dispMessage <- paste0(dispMessage,'<br>')
      dispMessage <- paste0(dispMessage,'Longname: ',paste0(unlist(lineObj[[ input$routeNum2 ]]$lineLongNames),collapse = ' ') )
      dispMessage <- paste0(dispMessage,'<br>')
      dispMessage <- paste0(dispMessage,'Headway: ',paste0(unlist(lineObj[[ input$routeNum2 ]]$lineHeadway),collapse = ' ') )
      dispMessage <- paste0(dispMessage,'<br>')
      dispMessage <- paste0(dispMessage,'Mode: ',paste0(unlist(lineObj[[ input$routeNum2 ]]$lineMode),collapse = ' ') )
      v$msg <- dispMessage
      allLines <- list(createLines(abs(lineObj[[ input$routeNum2 ]]$nodeVector),1))
      spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet@proj4string)
      map <- map %>%addPolylines(data = spatialLinesObject,group='lines',color='#31a354',opacity=1,weight=3)
      stopSP <- createStops(lineObj[[ input$routeNum2 ]]$nodeVector)
      map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='lines',radius = 5,fillOpacity = 0,color='#2c7fb8',weight=2)
    } else{
      allLines <- lapply(1:length(linesRelevent),function(x) createLines(abs(lineObj[[ linesRelevent[x] ]]$nodeVector),x) )
      spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet@proj4string)
      map <- map %>%addPolylines(data = spatialLinesObject,group='lines',color='#31a354',opacity=1,weight=4)
      
    }
  }    
    map
  })
  
  output$message <- renderText(v$msg)
  
}
