library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "SERPM Area Transit"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,	
      box(width = NULL, solidHeader = TRUE,
        leafletOutput("busmap", height = 850)
      )
      
    ),
    column(width = 3,
	  p(h3('Select the transit line')),
    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 400px",width = NULL, status = "warning",
           uiOutput("routeSelectRadio")
      ),
	  p(h3('From first file (in purple')),
    box(width = NULL, status = "warning",        
        htmlOutput("message1")
      ),
	  p(h3('From second file (in green')),
	  box(width = NULL, status = "warning",        
	      htmlOutput("message2")
	  )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)