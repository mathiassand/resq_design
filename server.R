# Define server logic required to draw plots ----
server <- shinyServer(function(input, output) {
  manipulateHospitalData <- reactive({
    hospital_df <- join_df %>%
      ungroup() %>%
      group_by(year) %>%
      mutate(
        DTNMedianHospital = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()

    hospital_start_df <- hospital_df %>%
      group_by(hospital) %>%
      filter(year == min(year)) %>%
      select(hospital, DTNMedianHospital) %>%
      rename(startingDTNMedianHospital = DTNMedianHospital) %>%
      merge(hospital_df)

    hospital_median_df <- hospital_start_df %>%
      group_by(hospital, startingDTNMedianHospital, country, year) %>%
      filter(hospital == input$Select_hospital_ID) %>%
      summarise(hospitalMedian = median(DTNMedian)) %>%
      ungroup()

    country_median_df <- join_df %>%
      group_by(year) %>%
      filter(country == hospital_median_df$country, DTNCount >= 3) %>%
      summarise(
        countryMedian = median(DTNMedian), 
        country_n=n()) %>%
      ungroup()

    merged_country_hospital <- merge(country_median_df, hospital_median_df, by = "year")
  })

  manipulateTop10HospitalData <- reactive ({
    top10_cohort_df <- join_df %>%
      ungroup() %>%
      group_by(year) %>%
      filter(DTNCount >= 3) %>%
      mutate(
        DTNMedianCohort = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()
      

    top10_cohort_max_df <- top10_cohort_df %>%
      filter(country == manipulateHospitalData()$country & year == max(manipulateHospitalData()$year)) %>%
      arrange(desc(-DTNMedian)) %>%
      slice(1:10) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(latestDTNMediantop10 = DTNMedianCohort) %>%
      merge(top10_cohort_df)
      

      
    top10_filter_df <- top10_cohort_max_df %>%
      filter(year == manipulateHospitalData()$year) %>%
      group_by(year) %>%
      summarise(
        top10Median = median(DTNMedian),
        top10_n = n()
      )
    
  top10_filter_df <- merge(top10_filter_df, manipulateHospitalData(), by = "year")
  })


  manipulateCohortData <- reactive({
    cohort_df <- join_df %>%
      ungroup() %>%
      group_by(year) %>%
      filter(DTNCount >= 3) %>%
      mutate(
        DTNMedianCohort = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()

    cohort_start <- cohort_df %>%
      filter(country == manipulateHospitalData()$country & year == min(manipulateHospitalData()$year)) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(startingDTNMedianCohort = DTNMedianCohort) %>%
      merge(cohort_df) 

    # if it cannot find hospitals inside the country with the same starting DTN, then it cannot create a cohort. Problem for countries with few 
    # hospitals registered.
    cohort_filter_df <- cohort_start %>%
      filter(startingDTNMedianCohort == manipulateHospitalData()$startingDTNMedianHospital) %>%
      group_by(year) %>%
      summarise(
        cohortMeanofMedian = median(DTNMedian),
        cohort_n = n()
      )

  final_merge <- merge(manipulateTop10HospitalData(), cohort_filter_df)
  })
  
  
  dataEnds <- reactive({
    dataEnds <- manipulateCohortData() %>%
      slice(which.max(year))
  })
 
  
  output$visual1 <- renderPlot({
    manipulateCohortData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateCohortData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateCohortData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plot1,
        subtitle = subtitle_plot1,
        y = "Median DTN (minutes)", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 23),
        plot.subtitle = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50", size = 15),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50", size = 15),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) +
      geom_text_repel(data = dataEnds(), aes(x = year, y = hospitalMedian, label = hospitalLabel),
                      color = hospital_c,
                      fontface = "bold",
                      size = fontSize,
                      nudge_x = 0.25, direction = "x", hjust = "left",
                      segment.linetype = 0)
  }) 

  output$visual2 <- renderPlot({
    manipulateCohortData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = country_c, size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = country_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateCohortData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateCohortData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plot2,
        subtitle = subtitle_plot2,
        y = "Median DTN (minutes)", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 23),
        plot.subtitle = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50", size = 15),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50", size = 15),
        panel.background = element_rect(fill = "white", color = "grey50"),
        panel.grid.major.x = element_blank()
      ) +
      geom_text_repel(data = dataEnds(), aes(x = year, y = hospitalMedian, label = hospitalLabel),
                       color = hospital_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.25, direction = "x", hjust = "left",
                       segment.linetype = 0) + 
      geom_text_repel(data = dataEnds(), aes(x = year, y = countryMedian, label = countryLabel),
                       color = country_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.25, direction = "x", hjust = "left",
                       nudge_y = -1,
                       segment.linetype = 0)
      
  })

  output$visual3 <- renderPlot({
    manipulateCohortData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = "grey50", size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = "grey50", size = 5.5, stroke = 1.7, fill = "white") +
      geom_line(aes(x = year, y = cohortMeanofMedian), color = cohort_c, size = 2) +
      geom_point(aes(x = year, y = cohortMeanofMedian), shape = 21, color = cohort_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateCohortData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateCohortData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plot3,
        subtitle = subtitle_plots,
        y = "Median DTN (minutes)", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 23),
        plot.subtitle = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50", size = 15),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50", size = 15),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) +
      geom_text_repel(data = dataEnds(), aes(x = year, y = hospitalMedian, label = hospitalLabel),
                       color = hospital_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.25, direction = "x", hjust = "left",
                       segment.linetype = 0,
                       label.size = NA) + 
      geom_text_repel(data = dataEnds(), aes(x = year, y = cohortMeanofMedian, label = cohortLabel),
                       color = cohort_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.25, direction = "x", hjust = "left",
                       nudge_y = -1,
                       segment.linetype = 0,
                       label.size = NA)
  })

  output$visual4 <- renderPlot({
    manipulateCohortData() %>%
      ungroup() %>%
        ggplot(aes(x = year, y = hospitalMedian)) +
        geom_line(aes(y = countryMedian), color = "grey50", size = 2) +
        geom_point(aes(y = countryMedian), shape = 21, color = "grey50", fill = "white", size = 5.5, stroke = 1.7) +
        geom_line(aes(x = year, y = cohortMeanofMedian), color = "grey50", size = 2) +
        geom_point(aes(x = year, y = cohortMeanofMedian), shape = 21, color = "grey50", fill = "white", size = 5.5, stroke = 1.7) +
        geom_line(aes(x = year, y = top10Median), color = top10_c, size = 2) +
        geom_point(aes(x = year, y = top10Median), shape = 21, color = top10_c, fill = "white", size = 5.5, stroke = 1.7) +
        geom_line(color = hospital_c, size = 2) +
        geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateCohortData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateCohortData()$year) +
        expand_limits(x = 2021) +
        labs(
          color = "grey50",
          title = title_plot4,
          subtitle = subtitle_plots,
          y = "Median DTN (minutes)", x = "Years in RES-Q"
        ) +
        theme(
          plot.title = element_markdown(size = 23),
          plot.subtitle = element_markdown(size = 15),
          axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50", size = 15),
          axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50", size = 15),
          panel.background = element_rect(fill = "white", color = "grey50")
        )  +
        geom_text_repel(data = dataEnds(), aes(x = year, y = hospitalMedian, label = hospitalLabel),
                         color = hospital_c,
                         fontface = "bold",
                         size = fontSize,
                         nudge_x = 0.25, direction = "x", hjust = "left",
                         segment.linetype = 0,
                         label.size = NA) + 
        geom_text_repel(data = dataEnds(), aes(x = year, y = top10Median, label = top10Label),
                         color = top10_c,
                         fontface = "bold",
                         size = fontSize,
                         nudge_x = 0.25, direction = "x", hjust = "left",
                         nudge_y = -1,
                         segment.linetype = 0,
                         label.size = NA)
  })
  
  output$visual5 <- renderPlot({
    manipulateCohortData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = country_c, size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = country_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(aes(x = year, y = cohortMeanofMedian), color = cohort_c, size = 2) +
      geom_point(aes(x = year, y = cohortMeanofMedian), shape = 21, color = cohort_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(aes(x = year, y = top10Median), color = top10_c, size = 2) +
      geom_point(aes(x = year, y = top10Median), shape = 21, color = top10_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateCohortData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateCohortData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plot5,
        subtitle = subtitle_plots,
        y = "Median DTN (minutes)", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 23),
        plot.subtitle = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50", size = 15),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50", size = 15),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) + 
      geom_text_repel(data = dataEnds(), aes(x = year, y = hospitalMedian, label = hospitalLabel),
                       color = hospital_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.3, direction = "x", hjust = "left",
                       segment.linetype = 0
                       ) + 
      geom_text_repel(data = dataEnds(), aes(x = year, y = countryMedian, label = countryLabel),
                       color = country_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.3, direction = "x", hjust = "left",
                       nudge_y = -1,
                       segment.linetype = 0
                       ) +
      geom_text_repel(data = dataEnds(), aes(x = year, y = cohortMeanofMedian, label = cohortSummaryLabel),
                       color = cohort_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.45, direction = "x", hjust = "left",
                       nudge_y = -2,
                       segment.linetype = 0
                       ) +
      geom_text_repel(data = dataEnds(), aes(x = year, y = top10Median, label = top10SummaryLabel),
                       color = top10_c,
                       fontface = "bold",
                       size = fontSize,
                       nudge_x = 0.45, direction = "x", hjust = "left",
                       nudge_y = -2,
                       segment.linetype = 0
                       )
  })
  
})
