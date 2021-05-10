# Define UI for app that draws a histogram ----
ui<-navbarPage("RES-Q Open Hospital",
               tabPanel("DTN Dashboard", fluid=TRUE,
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Select_hospital_ID",
                                        label = "Select Hospital",
                                        choices = sort(unique(resq_clean$hospital)),
                                        selected = 1),
                          
                          ),
               
  
                          mainPanel(column(width = 12, offset = 1,
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
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            plotOutput("visual4", width = "70%", height = "650px")
                          )
                          
                        )
                        
    )
    
  )
  
)

