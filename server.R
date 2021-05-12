# Define server logic required to draw plots ----
server <- shinyServer(function(input, output) {
  manipulateStatusData <- reactive({
    # Current Status Dashboard variables
    resq_CS_df <- resq_clean %>%
      # filtering for years after 2015 only, as Mirek told us RES-Q was added in 2016.
      # Dataset has a lot of misstypes, or errors? 1900, 2013, 2014 etc.
      # filtering for DTN less than 10 and larger than 400, ask Mirek told us these are "normal" cutting points
      filter(
        hospital == input$Select_hospital_ID,
        year >= 2016, year <= 2020, DTN > 10, DTN < 400,
        `stroke type` == "ischemic"
      ) %>%
      group_by(hospital, year) %>%
      summarise(
        firstYear = min(year),
        latestYear = max(year),
        fastestDTN = min(DTN),
        slowestDTN = max(DTN),
        medianDTN = median(DTN),
        averageDTN = mean(DTN),
        DTNCount = n_distinct(DTN),
        patient_count = n_distinct(subject_id)
      ) %>%
      ungroup()
  })


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
    # filter_country_DTNCount <- join_df %>%
    #   group_by(year) %>%
    #   filter(country == manipulateHospitalData()$country & DTNCount > 3)
    
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
    
  #top10_filter_df <- merge(top10_filter_df, manipulateHospitalData(), by = "year")

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
   # browser()
    # if it cannot find hospitals inside the country with the same starting DTN, then it cannot create a cohort. Problem for countries with few 
    # hospitals registered.
    cohort_filter_df <- cohort_start %>%
      filter(year == manipulateHospitalData()$year & startingDTNMedianCohort == manipulateHospitalData()$startingDTNMedianHospital) %>%
      group_by(year) %>%
      summarise(
        cohortMeanofMedian = median(DTNMedian),
        n = n()
      )


  })
  
  grob_x <- 0.50
  fontSize <- 12
  hospital_c <- "#39AEC6"
  country_c <- "#C37165"
  top10_c <- "#9F65C3"
  cohort_c <- "#88C365"
  title_plots <- "<span style = 'color: grey50;'>Door-to-needle time (DTN) progress in minutes within your country</span>"
  
  hospitalText <- grobTree(textGrob("your hospital", x = 0.79, y = 0.6, hjust = 0, gp = gpar(col = hospital_c, fontsize = fontSize, fontface = "bold")))
  countryText <- grobTree(textGrob("your country", x = 0.79, y = 0.56, hjust = 0, gp = gpar(col = country_c, fontsize = fontSize, fontface = "bold")))
  cohortText <- grobTree(textGrob("hospitals with similar\nstarting year DTN as you", x = 0.79, y = 0.515, hjust = 0, gp = gpar(col = cohort_c, fontsize = fontSize, fontface = "bold")))
  top10Text <- grobTree(textGrob("hospitals with best DTN\nin current year", x = 0.79, y = 0.45, hjust = 0, gp = gpar(col = top10_c, fontsize = fontSize, fontface = "bold")))
  medianHospitalText <- grobTree(textGrob("Compared with the:", x = 0.225, y = 0.125, hjust = 0, gp = gpar(col = "grey50", fontsize = fontSize)))
  yourText <- grobTree(textGrob("Your:", x = 0.01, y = 0.125, hjust = 0, gp = gpar(col = "grey50", fontsize = fontSize)))
  
  countryText2 <- grobTree(textGrob("country", x = 0.145, y = 0.125, hjust = 0, gp = gpar(col = country_c, fontsize = 15, fontface = "bold")))
  cohortText2 <- grobTree(textGrob("median of hospitals DTN\nsimilar to your hospital's\nfirst year in RES-Q\nwithin your country", x = 0.4, y = 0.09, hjust = 0, gp = gpar(col = cohort_c, fontsize = 15, fontface = "bold")))
  top10Text2 <- grobTree(textGrob("median of current best DTN\nwithin your country", x = 0.675, y = 0.109, hjust = 0, gp = gpar(col = top10_c, fontsize = 15, fontface = "bold")))
  
  
  output$visual1 <- renderPlot({
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      expand_limits(x = 2021) +
      #theme_bw(color = "grey50") + 
      labs(
        title = title_plots,
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) + annotation_custom(hospitalText)
    
      #geom_text(aes(label = round(hospitalMedian), color = "#56B4E9", nudge_y = 0.8))
  }) 

  output$visual2 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = country_c, size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = country_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plots,
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
        panel.background = element_rect(fill = "white", color = "grey50"),
        panel.grid.major.x = element_blank()
      ) + 
      annotation_custom(hospitalText) + annotation_custom(countryText)
  )

  output$visual3 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = "grey50", size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = "grey50", size = 5.5, stroke = 1.7, fill = "white") +
      geom_line(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), color = cohort_c, size = 2) +
      geom_point(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), shape = 21, color = cohort_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plots,
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) +
      annotation_custom(hospitalText) + annotation_custom(cohortText)
  )

  output$visual4 <- renderPlot(
      manipulateHospitalData() %>%
        ungroup() %>%
        ggplot(aes(x = year, y = hospitalMedian)) +
        geom_line(aes(y = countryMedian), color = "grey50", size = 2) +
        geom_point(aes(y = countryMedian), shape = 21, color = "grey50", fill = "white", size = 5.5, stroke = 1.7) +
        geom_line(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), color = "grey50", size = 2) +
        geom_point(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), shape = 21, color = "grey50", fill = "white", size = 5.5, stroke = 1.7) +
        geom_line(data = manipulateTop10HospitalData(), aes(x = year, y = top10Median), color = top10_c, size = 2) +
        geom_point(data = manipulateTop10HospitalData(), aes(x = year, y = top10Median), shape = 21, color = top10_c, fill = "white", size = 5.5, stroke = 1.7) +
        geom_line(color = hospital_c, size = 2) +
        geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
        expandy(manipulateHospitalData()$DTNMedian, 0) +
        scale_x_continuous(breaks = manipulateHospitalData()$year) +
        expand_limits(x = 2021) +
        labs(
          title = title_plots,
          y = "Median DTN", x = "Years in RES-Q"
        ) +
        theme(
          plot.title = element_markdown(size = 20),
          axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
          axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
          panel.background = element_rect(fill = "white", color = "grey50")
        ) + annotation_custom(hospitalText) + annotation_custom(top10Text) 
    )
  
  output$visual5 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = country_c, size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = country_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), color = cohort_c, size = 2) +
      geom_point(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), shape = 21, color = cohort_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(data = manipulateTop10HospitalData(), aes(x = year, y = top10Median), color = top10_c, size = 2) +
      geom_point(data = manipulateTop10HospitalData(), aes(x = year, y = top10Median), shape = 21, color = top10_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      expand_limits(x = 2021) +
      labs(
        title = title_plots,
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) + annotation_custom(hospitalText) + annotation_custom(top10Text) +  
          annotation_custom(countryText) + annotation_custom(cohortText)
  )
  
})
