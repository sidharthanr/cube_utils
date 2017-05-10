library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "SERPM Area Transit"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,	
      box(width = NULL, solidHeader = TRUE,
        leafletOutput("busmap", height = 650)
      )
    ),
    column(width = 3,

	  p(h3('Select the transit line')),
    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 200px",width = NULL, status = "warning",
           uiOutput("routeSelectRadio")
      ),
	  p(h4(paste0(lineName1,': (in green)'))),
    box(width = NULL, status = "warning",        
        htmlOutput("message1")
      ),
	  p(h4(paste0(lineName2,': (in purple)'))),
	  box(width = NULL, status = "warning",        
	      htmlOutput("message2")
	  ),
	  wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 300px",width = NULL, status = "warning",
	            uiOutput("verticalSlider"),   
	            uiOutput("horizontalSlider")
	  ),
	  box(width = NULL, status = "warning",        
	      uiOutput("stickyTransitLineText")
	  )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)