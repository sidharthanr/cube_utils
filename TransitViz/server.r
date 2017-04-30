library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit

server <- function(input, output, session) {
  
  v <- reactiveValues(msg = "")

  # Route select input box
  output$routeSelectRadio <- renderUI({

    routeNums <- linesRelevent
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    
    
    radioButtons("routeNum2",label = 'select',
                 choices = routeNums, 
                 selected = routeNums[2])
    
  })
  
  output$routeSelectCombo <- renderUI({
    
    routeNums <- linesRelevent
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(None = 0, routeNums)
    selectInput("routeNumCombo", label = h3("Select First Route (in Purple)"), choices = routeNums, selected = routeNums[2])
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
        map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='lines',radius = 5,fillOpacity = 0,color='#31a354',weight=2)
      } else{
        allLines <- lapply(1:length(linesRelevent),function(x) createLines(abs(lineObj[[ linesRelevent[x] ]]$nodeVector),x) )
        spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet@proj4string)
        map <- map %>%addPolylines(data = spatialLinesObject,group='lines',color='#31a354',opacity=1,weight=4)
        
      }
    }
    if (!is.null(input$routeNumCombo) ){
      if(input$routeNumCombo!='0'){
        allLines <- list(createLines(abs(lineObj[[ input$routeNumCombo ]]$nodeVector),1))
        spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet@proj4string)
        map <- map %>%addPolylines(data = spatialLinesObject,group='lines',color='#dd1c77',opacity=1,weight=3)
        stopSP <- createStops(lineObj[[ input$routeNumCombo ]]$nodeVector)
        map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='lines',radius = 5,fillOpacity = 0,color='#dd1c77',weight=2)
      } else{

        
      }
    }    
    map
  })
  
  output$message <- renderText(v$msg)
  
}
