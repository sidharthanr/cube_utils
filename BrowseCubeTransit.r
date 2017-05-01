.libPaths( c( .libPaths(), "C:/Users/sidharthanr/Documents/R/win-library/3.2"))
arguments <- commandArgs(trailingOnly=TRUE)
for (i in 1:length(arguments)) {
  print(paste("arg",as.character(i),"=",arguments[i]))
}

library(shiny)
library(leaflet)
library(shinyFiles)
library(rgdal)
library(stringr)
library(tidyverse)
library(data.table)
library(rgeos)


source('C:\\Projects\\development\\cube_utils\\TransitLineFileRead.R')

DrName <- arguments[1]
shapeName <- paste0(arguments[2],'_node')
lineName <- arguments[3]

serpmCRS <- CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-81 +k=0.9999411764705882 +x_0=199999.9999999999 +y_0=0
             +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
ARCCRS <- CRS('+init=ESRI:102667')

setProjWeb <- function(inSh,origPrj){
  inSh@proj4string <- (origPrj)
  inSh  <- spTransform(inSh, CRS("+init=epsg:4269"))
}
cropByCen <- function(inSh,x1,x2,y1,y2){
  cenDT = data.table(gCentroid(inSh, byid = TRUE)@coords)
  retSH <- inSh[cenDT$y>y1 & cenDT$y<y2 & cenDT$x>x1 & cenDT$x<x2,]
  return(retSH)
}

nodeNet <- readOGR(DrName, shapeName)
nodeNet <- setProjWeb(nodeNet,serpmCRS)


lineObj  <- readTLFile(paste0(DrName,'\\',lineName))

allNodes <- unique(abs(unlist(lapply(lineObj,function(x) x$nodeVector))))

nodeNet <- nodeNet[nodeNet@data$N %in% allNodes,]
nodeNet_Crop <- cropByCen(nodeNet,-80.421741,-80.261497,25.734167,25.790575)
nrow(nodeNet_Crop)

nodeNet_Crop <- nodeNet

linesRelevent=list()
for(iline in 1:length(lineObj)){
  tempNodes <- abs(lineObj[[iline]]$nodeVector)
  check <- (tempNodes %in% nodeNet_Crop@data$N)  
  if(sum(check==TRUE)>0) linesRelevent <- append(linesRelevent,lineObj[[iline]]$lineNames)
}
linesRelevent <- sort(unlist(linesRelevent))

nodeNetCoord <- data.frame(nodeNet@coords %>% cbind(nodeNet@data$N)) %>% tbl_df() %>% 
  setNames(c('x','y','N')) %>% data.table() %>% setkey(N)

createLines <- function(selnodes,idvar){
dt <- nodeNetCoord[list(selnodes)][,N:=NULL]
line <- Line(coords = dt)
lines <- Lines(slinelist = line, ID = idvar)
return(lines)
}
createStops <- function(inpnodevector){
  neededNodes <- inpnodevector[inpnodevector>0]
  dt <- nodeNetCoord[list(neededNodes)]
  return(dt)
}
allLines <- lapply(1:length(linesRelevent),function(x) createLines(abs(lineObj[[ linesRelevent[x] ]]$nodeVector),x) )
spatialLinesObject <- SpatialLines(LinesList = allLines, proj4string = nodeNet@proj4string)

options(shiny.launch.browser=TRUE)
source('C:\\Projects\\development\\cube_utils\\TransitViz\\ui.r')
source('C:\\Projects\\development\\cube_utils\\TransitViz\\server.r')
shinyApp(ui, server)


