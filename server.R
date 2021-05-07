# Define server logic required to draw plots ----
server <- shinyServer(function(input, output) {
  
  filterCountryData <- reactive({
    filter(resq_clean, country == input$Select_country_ID)
  })

  filterHospitalData <- reactive({
    filter(resq_clean, hospital == input$Select_hospital_ID)
  })

  manipulateHospitalData<-reactive({
    hospital_median_df <- join_df %>%
      group_by(hospital, year) %>%
      filter(hospital == input$Select_hospital_ID) %>%
      summarise(hospitalMedian = median(DTNMedian)) %>%
      ungroup()
  })
  
  manipulateCountryData<-reactive({
    country_median_df <- join_df %>%
      group_by(country, year) %>%
      filter(country == input$Select_country_ID) %>%
      summarise(countryMedian = median(DTNMedian)) %>%
      ungroup()
  })
  
  mergeCountryHospitalData<-reactive({
    merged_countryHospital<-merge(manipulateCountryData(), manipulateHospitalData(), by = "year")
  })
  
  manipulateCohortData<-reactive({
    DTN_breaks <- c(-Inf, 30, 45, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, Inf)
    DTN_break_labels <- c("<30", "30-45", "45-60", "60-80", "80-100", "100-120", "120-140",
                          "140-160", "160-180", "180-200", "200-220", "220-240", ">240")
    
    cohort_df <- join_df %>%
      ungroup() %>%
      group_by(runningYear) %>%
      mutate(
        DTNMedianCohort = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()
    
    cohort_df <- cohort_df %>%
      filter(year == 2018) %>%
      #filter(hospital_median_df$runningYear == 1) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(startingDTNMedianCohort = DTNMedianCohort) %>%
      merge(cohort_df)
    
    cohort_filter_df <- cohort_df %>%
      filter(startingDTNMedianCohort == "80-100", year == manipulateHospitalData()$year) %>%
      group_by(year) %>%
      summarise(
        cohortMeanofMedian = mean(DTNMedian)
      )
  })
  


    


  

  


  output$visual1 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      #geom_smooth(se = FALSE, alpha = .5, color = "purple") +
      geom_line(color="purple", size = 2) +
      geom_point(shape = 21, color = "purple", fill = "white", size = 5, stroke = 1.7) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = "<span style = 'color: purple;'>Your hospital</span>",
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9),
      )
  )

  output$visual2 <- renderPlot(
    mergeCountryHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = "purple", size = 2) +
      geom_point(shape = 21, color = "purple", fill = "white", size = 5, stroke = 1.7) +
      geom_line(aes(y = countryMedian), color = "red", size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = "red", fill = "white", size = 5, stroke = 1.7) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = "<span style = 'color: purple;'>Your hospital</span>compared to the <span style = 'color: red;'>national median</span>",
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9),
      )
  )

  output$visual3 <- renderPlot(
    mergeCountryHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = "purple", size = 2) +
      geom_point(shape = 21, color = "purple", fill = "white", size = 5, stroke = 1.7) +
      geom_line(aes(y = countryMedian), color = "red", size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = "red", fill = "white", size = 5, stroke = 1.7) +
      geom_line(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), color = "#56B4E9", size = 2) +
      geom_point(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), shape = 21, color = "#56B4E9", fill = "white", size = 5, stroke = 1.7) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = "<span style = 'color: purple;'>Your hospital</span> compared to the <span style = 'color: red;'>national median</span>
        and a <span style = 'color: #56B4E9;'>group of hospitals</span> similar to your first two years in RES-Q",
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9)
      )
  )

  # output$visual4 <- renderPlotly(
  #     plot_ly(
  #       type = "scatter",
  #       mode = "markers",
  #       data = cohort_filter_df, x = ~runningYear, y = ~DTNMean
  #       # colors = pal,
  #     ) %>% config(
  #       displayModeBar = F
  #     ) %>%
  #       layout(
  #         xaxis = list(title = "xaxis", fixedrange=TRUE),
  #         yaxis = list(title = "yaxis", fixedrange=TRUE),
  #         legend = list(orientation = "h")
  #       )
  # )
})

