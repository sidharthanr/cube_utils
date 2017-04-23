library(data.table)
library(tidyverse)

library(rgdal)
library(leaflet)
library(maptools)
library(htmltools)
library(htmlwidgets)
DrName <- 'c:\\Temp\\Research\\SampleCubeNet'


serpmCRS <- ("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-81 +k=0.9999411764705882 +x_0=199999.9999999999 +y_0=0
             +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

setProjWeb <- function(inSh,origPrj){
  inSh@proj4string <- CRS(origPrj)
  inSh  <- spTransform(inSh, CRS("+init=epsg:4269"))
}

shapeNameList <- c('S7_10_REV_10132015 - EW Study_link','S7_10_REV_10132015 - EW Study_node',
                   'S7_10_20170402_2_link','S7_10_20170402_2_node'
                   )

readShapeAndAssign <- function(inum){
  readShapeAndAssign <- readOGR(DrName, shapeNameList[inum])
}

# system.time(linkNet1 <- readShapeAndAssign(1) )
# system.time(nodeNet1 <- readShapeAndAssign(2) )
# system.time(linkNet2 <- readShapeAndAssign(3) )
# system.time(nodeNet2 <- readShapeAndAssign(4) )


library(parallel)
cl <- makeCluster(3)
clusterEvalQ(cl, library("rgdal")) 
clusterExport(cl, c('shapeNameList','readShapeAndAssign','DrName'))
system.time(retShapes <- parLapply(cl,c(1:4),function(ii) readShapeAndAssign(ii) ) )
stopCluster(cl)

linkNet1 <- retShapes[[1]]
nodeNet1 <- retShapes[[2]]
linkNet2 <- retShapes[[3]]
nodeNet2 <- retShapes[[4]]

rm(retShapes)

# compare link data
diffLink1 <- dplyr::anti_join(linkNet1@data,linkNet2@data) %>% mutate(recType='InNetA')
diffLink2 <- dplyr::anti_join(linkNet2@data,linkNet1@data) %>% mutate(recType='InNetB')

diffNode1 <- dplyr::anti_join(nodeNet1@data,nodeNet2@data) %>% mutate(recType='InNetA')
diffNode2 <- dplyr::anti_join(nodeNet2@data,nodeNet1@data) %>% mutate(recType='InNetB')

diffLink <- rbind(diffLink1,diffLink2)
diffLink <- diffLink %>% left_join(diffLink %>% group_by(A,B) %>% summarise(numOccur=n()))
diffLink <- diffLink %>% mutate(
  recType = ifelse(numOccur==2,'InBoth',recType
  )
)
table(diffLink$recType)
  
diffNode <- rbind(diffNode1,diffNode2)
diffNode <- diffNode %>% left_join(diffNode %>% group_by(N) %>% summarise(numOccur=n()))
diffNode <- diffNode %>% mutate(
  recType = ifelse(numOccur==2,'InBoth',recType
  )
)

#write_csv(diffLink,'diffLink.csv')
#write_csv(diffNode,'diffNode.csv')


# Retain only links that have changed
changedLinks1 <- linkNet1[paste0(linkNet1@data$A,'_',linkNet1@data$B) %in% paste0(diffLink1$A,'_',diffLink1$B),]
changedLinks2 <- linkNet2[paste0(linkNet2@data$A,'_',linkNet2@data$B) %in% paste0(diffLink$A,'_',diffLink$B),]
changedLinks1@data <- changedLinks1@data %>% left_join(diffLink %>% select(A,B,recType) %>% filter(recType!='InNetB') %>% unique())
changedLinks2@data <- changedLinks2@data %>% left_join(diffLink %>% select(A,B,recType) %>% filter(recType!='InNetA') %>% unique())
row.names(changedLinks1) <- paste0(1:nrow(changedLinks1))
row.names(changedLinks2) <- paste0((nrow(changedLinks1)+1):(nrow(changedLinks1)+nrow(changedLinks2)))
changedLinks <- spRbind(changedLinks1,changedLinks2)
changedLinks@data <- changedLinks@data %>%   mutate(txtLbl = paste0('A=',A,',B=',B))
changedLinks <- setProjWeb(changedLinks,serpmCRS)

# Retain only nodess that have changed
changedNodes1 <- nodeNet1[paste0(nodeNet1@data$N) %in% paste0(diffNode1$N),]
changedNodes2 <- nodeNet2[paste0(nodeNet2@data$N) %in% paste0(diffNode$N),]
changedNodes1@data <- changedNodes1@data %>% left_join(diffNode %>% select(N,recType) %>% filter(recType!='InNetB') %>% unique())
changedNodes2@data <- changedNodes2@data %>% left_join(diffNode %>% select(N,recType) %>% filter(recType!='InNetA') %>% unique())
row.names(changedNodes1) <- paste0(1:nrow(changedNodes1))
row.names(changedNodes2) <- paste0((nrow(changedNodes1)+1):(nrow(changedNodes1)+nrow(changedNodes2)))
changedNodes <- spRbind(changedNodes1,changedNodes2)
changedNodes@data <- changedNodes@data %>%   mutate(txtLbl = paste0('N=',N))
changedNodes <- setProjWeb(changedNodes,serpmCRS)

InNetALinks <- changedLinks[changedLinks$recType=='InNetA',]
InNetBLinks <- changedLinks[changedLinks$recType=='InNetB',]
InBothLinks <- changedLinks[changedLinks$recType=='InBoth',]

InNetANodes <- changedNodes[changedNodes$recType=='InNetA',]
InNetBNodes <- changedNodes[changedNodes$recType=='InNetB',]
InBothNodes <- changedNodes[changedNodes$recType=='InBoth',]

ll <- leaflet() %>%  addTiles() %>%
  addProviderTiles('Esri.WorldStreetMap',group='EsriStreet') %>%
  addProviderTiles('Esri.WorldImagery',group='EsriImagery') %>%
  addProviderTiles('CartoDB.Positron',group='CartoDB-1')

if(nrow(InNetALinks)>0){
  ll <- ll %>%   addPolylines(data=InNetALinks,group='InNetA',stroke=T,weight = 2,fillOpacity = 0,color='#2ca25f',
                              label=lapply(InNetALinks@data$txtLbl, function(x) HTML(x)),
                              labelOptions = labelOptions(noHide = T))
} 
if(nrow(InNetBLinks)>0){
  ll <- ll %>%   addPolylines(data=InNetBLinks,group='InNetB',stroke=T,weight = 2,fillOpacity = 0,color='#8856a7',
                              label=lapply(InNetBLinks@data$txtLbl, function(x) HTML(x)),
                              labelOptions = labelOptions(noHide = T))
}
if(nrow(InBothLinks)>0){
  ll <- ll %>%   addPolylines(data=InBothLinks,group='InBoth',stroke=T,weight = 2,fillOpacity = 0,color='#e34a33',
                              label=lapply(InBothLinks@data$txtLbl, function(x) HTML(x)),
                              labelOptions = labelOptions(noHide = T))
}
if(nrow(InNetANodes)>0){
  ll <- ll %>%   addMarkers(data=InNetANodes,group='InNetA',
                            label=lapply(InNetANodes@data$txtLbl, function(x) HTML(x)),
                            labelOptions = lapply(1:nrow(InNetANodes@data), function(x) {
                              labelOptions(noHide = T,textsize = "12px", textOnly = TRUE)
                            }))
}
if(nrow(InNetBNodes)>0){
  ll <- ll %>%   addMarkers(data=InNetBNodes,group='InNetB',
                            label=lapply(InNetBNodes@data$txtLbl, function(x) HTML(x)),
                            labelOptions = lapply(1:nrow(InNetBNodes@data), function(x) {
                              labelOptions(noHide = T,textsize = "12px", textOnly = TRUE)
                            }))
}
if(nrow(InBothNodes)>0){
  ll <- ll %>%   addMarkers(data=InBothNodes,group='InBoth',
                            label=lapply(InBothNodes@data$txtLbl, function(x) HTML(x)),
                            labelOptions = lapply(1:nrow(InBothNodes@data), function(x) {
                              labelOptions(noHide = T,textsize = "12px", textOnly = TRUE)
                            }))
}

ll <- ll %>%  
  addLayersControl(
    baseGroups = c('CartoDB-1','EsriStreet','EsriImagery'),
    overlayGroups = c('InNetA','InNetB','InBoth'),
    options = layersControlOptions(collapsed = T)  )

ll

saveWidget(ll, file = 'NetworkComparison.html', selfcontained = F)



