library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit

server <- function(input, output, session) {
  
  v <- reactiveValues(msg1 = "")
  v <- reactiveValues(msg2 = "")

  linesReleventFn <- reactive({
    lat <- unlist(lapply(str_split(input$slider1,' '),as.numeric))
    lng <- unlist(lapply(str_split(input$slider2,' '),as.numeric))
    c_SW <- c(lng[1],lat[1])
    c_NE <- c(lng[2],lat[2])
    linesRelevent1 <- createReleventLines(nodeNet1,lineObj1,c_SW,c_NE)
    linesRelevent2 <- createReleventLines(nodeNet2,lineObj2,c_SW,c_NE)
    unique(c(linesRelevent1,linesRelevent2))
  })
  
  
  output$stickyTransitLineText <- renderUI({
    textInput("stickyTransitLine", label = h3("Sticky Lines"), value = "")
  })
  # Route select input box
  output$routeSelectRadio <- renderUI({

    linesRelevent <- linesReleventFn()
    routeNums <- linesRelevent
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(None = 0, routeNums)
    radioButtons("routeNum2",label = 'select',
                 choices = routeNums, 
                 selected = routeNums[2])
    
  })
  
  # verticalSlider
  output$verticalSlider <- renderUI({
    sliderInput("slider1", label = ("Latitude filter"), min = bMat[2,1], 
                max = bMat[2,2], value = c(bMat[2,1],bMat[2,2]))
  })
  # horizontalSlider
  output$horizontalSlider <- renderUI({
    sliderInput("slider2", label = ("Longitude filter"), min = bMat[1,1], 
                max = bMat[1,2], value = c(bMat[1,1],bMat[1,2]))
  })
  
output$busmap <- renderLeaflet({
  
  linesRelevent <- linesReleventFn()
  # Temporary hard code

  lat <- unlist(lapply(str_split(input$slider1,' '),as.numeric))
  lng <- unlist(lapply(str_split(input$slider2,' '),as.numeric))
  
    map <- leaflet() %>%
      addProviderTiles('Esri.WorldStreetMap',group='EsriStreet') %>%
      addProviderTiles('Esri.WorldImagery',group='EsriImagery') %>%
      addProviderTiles('CartoDB.Positron',group='CartoDB-1') %>% 
      addLayersControl(
        baseGroups = c('CartoDB-1','EsriStreet','EsriImagery'),
        overlayGroups = c('Line 1','Line 2','Sticky Line','Region'),
        options = layersControlOptions(collapsed = T)  )
    
    if (!is.null(input$routeNum2) ){
      if(input$routeNum2!='0' & !(is.null(lineObj1[[ input$routeNum2 ]])) ){
        
        dispMessage <- paste0('Name: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Line number: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineNumber),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')        
        dispMessage <- paste0(dispMessage,'Longname: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineLongNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Headway: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineHeadway),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Mode: ',paste0(unlist(lineObj1[[ input$routeNum2 ]]$lineMode),collapse = ' ') )
        v$msg1 <- dispMessage
        allLines <- list(createLines(nodeNetCoord1,abs(lineObj1[[ input$routeNum2 ]]$nodeVector),1))
        spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet1@proj4string)
        map <- map %>%addPolylines(data = spatialLinesObject,group='Line 1',color='#2ca25f',opacity=1,weight=3)
        stopSP <- createStops(nodeNetCoord1,lineObj1[[ input$routeNum2 ]]$nodeVector)
        map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='Line 1',radius = 5,fillOpacity = 0,color='#2ca25f',weight=2)
      } else{
        v$msg1 <- 'Line not present'
      }
      if(input$routeNum2!='0' & !(is.null(lineObj2[[ input$routeNum2 ]])) ){
        
        dispMessage <- paste0('Name: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Line number: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineNumber),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Longname: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineLongNames),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Headway: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineHeadway),collapse = ' ') )
        dispMessage <- paste0(dispMessage,'<br>')
        dispMessage <- paste0(dispMessage,'Mode: ',paste0(unlist(lineObj2[[ input$routeNum2 ]]$lineMode),collapse = ' ') )
        v$msg2 <- dispMessage
        allLines <- list(createLines(nodeNetCoord2,abs(lineObj2[[ input$routeNum2 ]]$nodeVector),1))
        spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet1@proj4string)
        map <- map %>%addPolylines(data = spatialLinesObject,group='Line 2',color='#dd1c77',opacity=0.5,weight=5)
        stopSP <- createStops(nodeNetCoord2,lineObj2[[ input$routeNum2 ]]$nodeVector)
        map <- map %>%addCircleMarkers(data = stopSP,lng = ~x, lat=~y, group='Line 2',radius = 8,fillOpacity = 0,color='#dd1c77',weight=2)
      } else{
        v$msg2 <- 'Line not present'
      }
    }
    if (!is.null(input$slider1) ){
        map <- map %>%addRectangles(
        lng1=lng[2], lat1=lat[2],
        lng2=lng[1], lat2=lat[1],
        fillColor = "transparent",group = 'Region',weight = 1,color='#fec44f'
      ) 
    }
    
    if (!is.null(input$stickyTransitLine) ){
      if (!is.null(lineObj2[[ input$stickyTransitLine ]]) ){
        
        #linesSticky <- unlist(str_split(input$stickyTransitLine,','))
        #print(linesSticky)
        #allLines <- lapply(1:length(linesSticky),function(x) createLines(abs(lineObj2[[ linesSticky[x] ]]$nodeVector),x) )
        #spatialLinesObject_Fixed <- SpatialLines(LinesList = allLines, proj4string = nodeNet2@proj4string)
        
        spatialLinesObject_Fixed <- SpatialLines(list(createLines(nodeNetCoord1,abs(lineObj1[[ input$stickyTransitLine ]]$nodeVector),1)), proj4string = nodeNet1@proj4string)
        map <- map %>%
          addPolylines(data = spatialLinesObject_Fixed,group='Sticky Line',color='#fec44f',weight = 4,opacity=1)
          
      }
    }
    

    
    
    map
  })
  
  output$message1 <- renderText(v$msg1)
  output$message2 <- renderText(v$msg2)
  
}
