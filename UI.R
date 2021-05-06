# Define UI for app that draws a histogram ----
ui<-navbarPage("RES-Q Open Hospital",
               tabPanel("DTN Dashboard"),
               includeCSS("custom.css"),
               #, position = c("fixed-top"),
  
  mainPanel(column(width = 12, offset = 5,
    plotOutput("visual1", width = "70%", height = "650px"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    plotOutput("visual2", width = "70%", height = "650px"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    plotOutput("visual3", width = "70%", height = "650px"),
    # br(),
    # br(),
    # br(),
    # br(),
    # br(),
    # br(),
    # plotlyOutput("visual4", width = "70%", height = "650px")
  )),
  
  tags$footer()
)
