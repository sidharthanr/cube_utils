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
    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 400px",width = NULL, status = "warning",
           uiOutput("routeSelectrADIO")
      ),
      box(width = NULL, status = "warning",
        #uiOutput("routeSelect"),
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