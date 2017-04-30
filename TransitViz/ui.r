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
	  uiOutput("routeSelectCombo"),
	  p(h3('Select Second Route (in Green)')),
    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 400px",width = NULL, status = "warning",
           uiOutput("routeSelectRadio")
      ),
	  p(h3('Second Line Details')),
      box(width = NULL, status = "warning",        
        htmlOutput("message")
      )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)