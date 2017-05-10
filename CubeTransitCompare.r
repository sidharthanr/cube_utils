#   .libPaths( c( .libPaths(), "C:/Users/sidharthanr/Documents/R/win-library/3.2"))
#   arguments <- commandArgs(trailingOnly=TRUE)
#   for (i in 1:length(arguments)) {
#     print(paste("arg",as.character(i),"=",arguments[i]))
#   }

#   DrName <- arguments[1]
#   shapeName <- paste0(arguments[2],'_node')
#   lineName <- arguments[3]

DrName <- 'E:\\projects\\MDT_185782A\\Tasks\\SMARTPlan\\Kendall_Investigation\\Networks'
shapeName1 <- 'Kendall_2010_Project_node'
lineName1 <- 'Kendall_2010_Project_Transit.lin'

shapeName2 <- 'Kendall_2040_Project_node'
lineName2 <- 'Kendall_2040_Project_Transit.lin'


library(shiny)
library(leaflet)
library(shinyFiles)
library(rgdal)
library(stringr)
library(tidyverse)
library(data.table)
library(rgeos)


source('C:\\Projects\\development\\cube_utils\\TransitLineFileRead.R')



serpmCRS <- CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-81 +k=0.9999411764705882 +x_0=199999.9999999999 +y_0=0
             +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
ARCCRS <- CRS('+init=ESRI:102667')

setProjWeb <- function(inSh,origPrj){
  inSh@proj4string <- (origPrj)
  inSh  <- spTransform(inSh, CRS("+init=epsg:4269"))
}

cropByCen <- function(inSh,SW_C,NE_C){
  cenDT = data.table(gCentroid(inSh, byid = TRUE)@coords)
  retSH <- inSh[cenDT$y>SW_C[2] & cenDT$y<NE_C[2] & cenDT$x>SW_C[1] & cenDT$x<NE_C[1],]
  return(retSH)
}

lineObj1  <- readTLFile(paste0(DrName,'\\',lineName1))
allNodes1 <- unique(abs(unlist(lapply(lineObj1,function(x) x$nodeVector))))
nodeNet1 <- setProjWeb(readOGR(DrName, shapeName1),serpmCRS)
nodeNet1 <- nodeNet1[nodeNet1@data$N %in% allNodes1,]
bMat <- round(nodeNet1@bbox,3)

createReleventLines <- function(nodeNet,lineObj,SW_Coord,NE_Coord){
  nodeNet_rel <- cropByCen(nodeNet,SW_Coord,NE_Coord)
  linesRelevent=list()
  for(iline in 1:length(lineObj)){
    tempNodes <- abs(lineObj[[iline]]$nodeVector)
    check <- (tempNodes %in% nodeNet_rel@data$N)  
    if(sum(check==TRUE)>0) linesRelevent <- append(linesRelevent,lineObj[[iline]]$lineNames)
  }
  linesRelevent <- sort(unlist(linesRelevent))
  return(linesRelevent)
}

linesRelevent1 <- createReleventLines(nodeNet1,lineObj1,nodeNet1@bbox[,1],nodeNet1@bbox[,2])
nodeNetCoord1 <- data.frame(nodeNet1@coords %>% cbind(nodeNet1@data$N)) %>% tbl_df() %>% 
  setNames(c('x','y','N')) %>% data.table() %>% setkey(N)

lineObj2  <- readTLFile(paste0(DrName,'\\',lineName2))
allNodes2 <- unique(abs(unlist(lapply(lineObj2,function(x) x$nodeVector))))
nodeNet2 <- setProjWeb(readOGR(DrName, shapeName2),serpmCRS)
nodeNet2 <- nodeNet2[nodeNet2@data$N %in% allNodes2,]
linesRelevent2 <- createReleventLines(nodeNet2,lineObj2,nodeNet2@bbox[,1],nodeNet2@bbox[,2])
nodeNetCoord2 <- data.frame(nodeNet2@coords %>% cbind(nodeNet2@data$N)) %>% tbl_df() %>% 
  setNames(c('x','y','N')) %>% data.table() %>% setkey(N)

linesRelevent <- unique(c(linesRelevent1,linesRelevent2))


createLines <- function(nodeNetCoord,selnodes,idvar){
dt <- nodeNetCoord[list(selnodes)][,N:=NULL]
line <- Line(coords = dt)
lines <- Lines(slinelist = line, ID = idvar)
return(lines)
}
createStops <- function(nodeNetCoord,inpnodevector){
  neededNodes <- inpnodevector[inpnodevector>0]
  dt <- nodeNetCoord[list(neededNodes)]
  return(dt)
}

source('C:\\Projects\\development\\cube_utils\\TransitCompare\\ui.r')
source('C:\\Projects\\development\\cube_utils\\TransitCompare\\server.r')
shinyApp(ui, server)


