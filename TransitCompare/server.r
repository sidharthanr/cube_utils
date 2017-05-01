library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit

server <- function(input, output, session) {
  
  v <- reactiveValues(msg1 = "")
  v <- reactiveValues(msg2 = "")

  # Route select input box
  output$routeSelectRadio <- renderUI({

    routeNums <- linesRelevent
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(None = 0, routeNums)
    radioButtons("routeNum2",label = 'select',
                 choices = routeNums, 
                 selected = routeNums[2])
    
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
      if(input$routeNum2!='0' & !(is.null(lineObj1[[ input$routeNum2 ]])) ){
        
        dispMessage <- paste0('Name: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Longname: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineLongNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Headway: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineHeadway),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Mode: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineMode),collapse = ' ') )
        v$msg1 <- dispMessage
        allLines <- list(createLines(nodeNetCoord1,abs(lineObj1[[ input$routeNum2 ]]$nodeVector),1))
        spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet1@proj4string)
        map <- map %>%addPolylines(data = spatialLinesObject,group='lines',color='#31a354',opacity=1,weight=3)
        stopSP <- createStops(nodeNetCoord1,lineObj1[[ input$routeNum2 ]]$nodeVector)
        map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='lines',radius = 5,fillOpacity = 0,color='#31a354',weight=2)
      } else{
        v$msg1 <- 'Line not present'
      }
      if(input$routeNum2!='0' & !(is.null(lineObj2[[ input$routeNum2 ]])) ){
        
        dispMessage <- paste0('Name: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Longname: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineLongNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Headway: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineHeadway),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Mode: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineMode),collapse = ' ') )
        v$msg2 <- dispMessage
        allLines <- list(createLines(nodeNetCoord2,abs(lineObj2[[ input$routeNum2 ]]$nodeVector),1))
        spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet1@proj4string)
        map <- map %>%addPolylines(data = spatialLinesObject,group='lines',color='#dd1c77',opacity=0.5,weight=5)
        stopSP <- createStops(nodeNetCoord2,lineObj2[[ input$routeNum2 ]]$nodeVector)
        map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='lines',radius = 8,fillOpacity = 0,color='#dd1c77',weight=2)
      } else{
        v$msg2 <- 'Line not present'
      }
    }   
    map
  })
  
  output$message1 <- renderText(v$msg1)
  output$message2 <- renderText(v$msg2)
  
}
