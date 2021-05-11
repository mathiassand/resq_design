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
      group_by(runningYear) %>%
      mutate(
        DTNMedianHospital = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()

    hospital_start_df <- hospital_df %>%
      group_by(hospital) %>%
      filter(runningYear == min(runningYear)) %>%
      select(hospital, DTNMedianHospital) %>%
      rename(startingDTNMedianHospital = DTNMedianHospital) %>%
      merge(hospital_df)

    # hospital_end_df <- hospital_df %>%
    #   group_by(hospital) %>%
    #   filter(runningYear == max(runningYear)) %>%
    #   select(hospital, DTNMedianHospital) %>%
    #   rename(lastDTNMedianHospital = DTNMedianHospital) %>%
    #   merge(hospital_df)
    # 
    # merge_start_end <- merge(hospital_start_df, hospital_end_df)

    hospital_median_df <- hospital_start_df %>%
      group_by(hospital, startingDTNMedianHospital, country, runningYear, year) %>%
      filter(hospital == input$Select_hospital_ID) %>%
      summarise(hospitalMedian = median(DTNMedian)) %>%
      ungroup()

    hospital_median_df <- hospital_median_df %>%
      group_by(hospital) %>%
      mutate(maxRunningYear = max(runningYear)) %>%
      ungroup()


    country_median_df <- join_df %>%
      group_by(year) %>%
      filter(country == hospital_median_df$country, DTNCount >= 3) %>%
      summarise(countryMedian = median(DTNMedian), country_n=n()) %>%
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
      filter(country == manipulateHospitalData()$country & DTNCount >= 3) %>%
      mutate(
        DTNMedianCohort = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()
      

    top10_cohort_max_df <- top10_cohort_df %>%
      filter(year == max(manipulateHospitalData()$year)) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(latestDTNMediantop10 = DTNMedianCohort) %>%
      merge(top10_cohort_df)
      

    
    top10_filter_df <- top10_cohort_max_df %>%
      filter(year == manipulateHospitalData()$year) %>%
      arrange(desc(-DTNMedian)) %>%
      slice(1:10) %>%
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
      mutate(
        DTNMedianCohort = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()

    cohort_start <- cohort_df %>%
      filter(runningYear == 1, country == manipulateHospitalData()$country) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(startingDTNMedianCohort = DTNMedianCohort) %>%
      merge(cohort_df) 

    
    cohort_filter_df <- cohort_start %>%
      filter(startingDTNMedianCohort == manipulateHospitalData()$startingDTNMedianHospital & year == manipulateHospitalData()$year) %>%
      group_by(year) %>%
      summarise(
        cohortMeanofMedian = mean(DTNMedian),
        n = n()
      )

  })

    # condition1_end <- cohort_df %>%
    #   #group_by(hospital) %>%
    #   filter(runningYear == max(runningYear)) %>%
    #   select(hospital, DTNMedianCohort) %>%
    #   rename(lastDTNMedianCohort = DTNMedianCohort) %>%
    #   merge(cohort_df)
    
    #condition1_end

    #merge_condition1 <- merge(condition1_start, condition1_end)

    # condition1_end <- condition1_end %>%
    #   filter(
    #     year == manipulateHospitalData()$year 
    #     #startingDTNMedianCohort == manipulateHospitalData()$startingDTNMedianHospital
    #   )
    
    # condition1_start <- condition1_start %>%
    #   filter(
    #     year == manipulateHospitalData()$year 
    #     #startingDTNMedianCohort == manipulateHospitalData()$startingDTNMedianHospital
    #   )

    # cohort_filter_df <- condition1_start %>%
    #   filter(runningYear == 1) %>%
    #   select(hospital, DTNMedianCohort) %>%
    #   rename(startingDTNMedianCohort = DTNMedianCohort) %>%
    #   merge(condition1_start)




    
    # cohort_filter_df <- merge_condition1 %>%
    #   filter(lastDTNMedianCohort <= manipulateHospitalData()$lastDTNMedianHospital & year == manipulateHospitalData()$year) %>%
    #   group_by(year) %>%
    #   summarise(
    #     cohortMeanofMedian = mean(DTNMedian)
    #   )
    # browser()
    



    # can add more statements: we probably want to have something like 2 first years of cohort to match 2 first of hospital
    # and then have hospitals above or equal to 4 maxRunningYears have the 2 last years be different from the cohorts (with cohort have more improvement)
    # if (manipulateHospitalData()$maxRunningYear >= 2) {
    #       filter(lastDTNMedianCohort == "<30") %>%
    #       group_by(year, startingDTNMedianCohort) %>%
    #       summarise(
    #         cohortMeanofMedian = mean(DTNMedian)
    # )
    # }
    # else if (manipulateHospitalData()$maxRunningYear == 3) {
    #   cohort_filter_df <- merge_condition1 %>%
    #     filter(lastDTNMedianCohort == "<30") %>%
    #     group_by(year, startingDTNMedianCohort) %>%
    #     summarise(
    #       cohortMeanofMedian = mean(DTNMedian)
    #     )
    # }
    # if (manipulateHospitalData()$maxRunningYear >= 4) {
    #   cohort_filter_df <- merge_condition1 %>%
    #     filter(runningYear == max(runningYear)) %>%
    #     group_by(year, lastDTNMedianCohort) %>%
    #     summarise(
    #       cohortMeanofMedian = mean(DTNMedian)
    #     )
    # }






    # firstSecondRunningYear <- cohort_df %>%
    #   filter(runningYear <= 2) %>%
    #   select(hospital, DTNMedianCohort) %>%
    #   rename(startingDTNMedianCohort = DTNMedianCohort) %>%
    #   merge(cohort_df)
    #
    # lastRunningYears <- firstSecondRunningYear %>%
    #   filter(runningYear >= 3) %>%
    #   select(hospital, DTNMedianCohort) %>%
    #   rename(lastDTNMedianCohort = DTNMedianCohort) %>%
    #   merge(cohort_df)


    # this one should work just fine, though need it to differentiate between last two points, and make cohort the better version
    # if(manipulateHospitalData()$maxRunningYear <= 2) {
    #
    # }
  




  # output$testText<-renderText({
  #   str_patientcount<-({paste("Number of patients with Ischemic Stroke:", manipulateStatusData()$patient_count)})
  #   str_DTNpatientcount<-({paste("Number of DTN recorded for patients with Ischemic Stroke:", manipulateStatusData()$DTNCount)})
  # })
  
  
  # output$visual1 <- renderPlotly({
  #   p<-ggplot(manipulateHospitalData(), aes(x = year, y = hospitalMedian)) +
  #     geom_line(color = hospital_c, size = 2) +
  #     geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
  #     #theme_classic() +
  #     expandy(manipulateHospitalData()$DTNMedian, 0) +
  #     scale_x_continuous(breaks = manipulateHospitalData()$year) +
  #     labs(
  #       title = "<span style = 'color: grey50;'>Yearly median door-to-needle progress</span>",
  #       y = "Median DTN", x = "Years in RES-Q"
  #     ) +
  #     theme(
  #       plot.title = element_markdown(size = 20),
  #       axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
  #       axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
  #       panel.background = element_rect(fill = "white", color = "grey50")
  #     ) + annotation_custom(hospitalText)
  #   
  #   p <- ggplotly(p)
  #   p
  #   #geom_text(aes(label = round(hospitalMedian), color = "#56B4E9", nudge_y = 0.8))
  # }) 
  
  grob_x <- 0.50
  hospital_c <- "#39AEC6"
  country_c <- "#C37165"
  top10_c <- "#9F65C3"
  cohort_c <- "#88C365"
  title_plots <- "<span style = 'color: grey50;'>Door-to-needle time (DTN) progress in minutes</span>"
  
  
  hospitalText <- grobTree(textGrob("hospital", x = grob_x, y = 0.30, hjust = 0, gp = gpar(col = hospital_c, fontsize = 18, fontface = "bold")))
  countryText <- grobTree(textGrob("country", x = grob_x, y = 0.25, hjust = 0, gp = gpar(col = country_c, fontsize = 18, fontface = "bold")))
  cohortText <- grobTree(textGrob("similar DTN to your hospital's\nfirst year in RES-Q within your country", x = grob_x, y = 0.175, hjust = 0, gp = gpar(col = cohort_c, fontsize = 18, fontface = "bold")))
  top10Text <- grobTree(textGrob("current best DTN within your country", x = grob_x, y = 0.1, hjust = 0, gp = gpar(col = top10_c, fontsize = 18, fontface = "bold")))
  medianHospitalText <- grobTree(textGrob("Median of hospitals with:", x = 0.24, y = 0.2, hjust = 0, gp = gpar(col = "grey50", fontsize = 18)))
  yourText <- grobTree(textGrob("Your:", x = 0.44, y = 0.3, hjust = 0, gp = gpar(col = "grey50", fontsize = 18)))

  
  
  output$visual1 <- renderPlot({
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      #theme_classic() +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = title_plots,
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
        panel.background = element_rect(fill = "white", color = "grey50")
      ) + annotation_custom(hospitalText) + annotation_custom(yourText)
    
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
      theme_classic() +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
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
      annotation_custom(hospitalText) + annotation_custom(countryText) + annotation_custom(yourText)
  )

  output$visual3 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(aes(y = countryMedian), color = country_c, size = 2) +
      geom_point(aes(y = countryMedian), shape = 21, color = country_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), color = cohort_c, size = 2) +
      geom_point(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), shape = 21, color = cohort_c, fill = "white", size = 5.5, stroke = 1.7) +
      geom_line(color = hospital_c, size = 2) +
      geom_point(shape = 21, color = hospital_c, fill = "white", size = 5.5, stroke = 1.7) +
      theme_classic() +
      expandy(manipulateHospitalData()$DTNMedian, 0) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
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
      annotation_custom(hospitalText) + annotation_custom(countryText) + annotation_custom(cohortText)+ annotation_custom(medianHospitalText) + annotation_custom(yourText)
  )

  output$visual4 <- renderPlot(
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

        theme_classic() +
        expandy(manipulateHospitalData()$DTNMedian, 0) +
        scale_x_continuous(breaks = manipulateHospitalData()$year) +
        labs(
          title = title_plots,
          y = "Median DTN", x = "Years in RES-Q"
        ) +
        theme(
          plot.title = element_markdown(size = 20),
          axis.title.x = element_text(hjust = .035, vjust = 0.2, color = "grey50"),
          axis.title.y = element_text(hjust = 0.95, vjust = 0.9, color = "grey50"),
          panel.background = element_rect(fill = "white", color = "grey50")
        ) + annotation_custom(hospitalText) + annotation_custom(countryText) + annotation_custom(cohortText) + annotation_custom(top10Text)+ annotation_custom(medianHospitalText) + annotation_custom(yourText)
    )
  
})
