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

    hospital_end_df <- hospital_df %>%
      group_by(hospital) %>%
      filter(runningYear == max(runningYear)) %>%
      select(hospital, DTNMedianHospital) %>%
      rename(lastDTNMedianHospital = DTNMedianHospital) %>%
      merge(hospital_df)

    merge_start_end <- left_join(hospital_start_df, hospital_end_df)

    hospital_median_df <- merge_start_end %>%
      group_by(hospital, startingDTNMedianHospital, lastDTNMedianHospital, country, runningYear, year) %>%
      filter(hospital == input$Select_hospital_ID) %>%
      summarise(hospitalMedian = median(DTNMedian)) %>%
      ungroup()

    hospital_median_df <- hospital_median_df %>%
      group_by(hospital) %>%
      mutate(maxRunningYear = max(runningYear)) %>%
      ungroup()


    country_median_df <- join_df %>%
      group_by(year) %>%
      filter(country == hospital_median_df$country) %>%
      summarise(countryMedian = median(DTNMedian)) %>%
      ungroup()

    merged_country_hospital <- merge(country_median_df, hospital_median_df, by = "year")
  })




  manipulateCohortData <- reactive({
    cohort_df <- join_df %>%
      ungroup() %>%
      group_by(runningYear) %>%
      mutate(
        DTNMedianCohort = cut(DTNMedian, breaks = DTN_breaks, labels = DTN_break_labels)
      ) %>%
      ungroup()

    condition1_start <- cohort_df %>%
      group_by(hospital) %>%
      filter(runningYear == min(runningYear)) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(startingDTNMedianCohort = DTNMedianCohort) %>%
      merge(cohort_df) %>%
      ungroup()

    condition1_end <- cohort_df %>%
      group_by(hospital) %>%
      filter(runningYear == max(runningYear)) %>%
      select(hospital, DTNMedianCohort) %>%
      rename(lastDTNMedianCohort = DTNMedianCohort) %>%
      merge(cohort_df) %>%
      ungroup()

    merge_condition1 <- left_join(condition1_start, condition1_end)

    merge_condition1 <- merge_condition1 %>%
      filter(
        year == manipulateHospitalData()$year &
          startingDTNMedianCohort == manipulateHospitalData()$startingDTNMedianHospital
      )
    
    cohort_filter_df <- merge_condition1 %>%
      filter(lastDTNMedianCohort == "<30") %>%
      group_by(year, startingDTNMedianCohort) %>%
      summarise(
        cohortMeanofMedian = mean(DTNMedian)
      )


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
    # else if (manipulateHospitalData()$maxRunningYear >= 4) {
    #   cohort_filter_df <- merge_condition1 %>%
    #     filter(lastDTNMedianCohort == "<30") %>%
    #     group_by(year, startingDTNMedianCohort) %>%
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
  })




  # output$testText<-renderText({
  #   str_patientcount<-({paste("Number of patients with Ischemic Stroke:", manipulateStatusData()$patient_count)})
  #   str_DTNpatientcount<-({paste("Number of DTN recorded for patients with Ischemic Stroke:", manipulateStatusData()$DTNCount)})
  # })

  output$visual1 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = "#56B4E9", size = 2) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = "<span style = 'color: #56B4E9;'>Your hospital</span>",
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9)
      ) #+ scale_color_manual(name="Text", values = colors)
  )

  output$visual2 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = "#56B4E9", size = 2) +
      geom_line(aes(y = countryMedian), color = "#F8766D", size = 2) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = "<span style = 'color: #56B4E9;'>Your hospital</span>compared to the <span style = 'color: #F8766D;'>national median</span>",
        y = "Median DTN", x = "Years in RES-Q", color = "Legend"
      ) +
      theme(
        plot.title = element_markdown(size = 20),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9)
      )
  )

  output$visual3 <- renderPlot(
    manipulateHospitalData() %>%
      ungroup() %>%
      ggplot(aes(x = year, y = hospitalMedian)) +
      geom_line(color = "#56B4E9", size = 2) +
      geom_line(aes(y = countryMedian), color = "#F8766D", size = 2) +
      geom_line(data = manipulateCohortData(), aes(x = year, y = cohortMeanofMedian), color = "#FFC300", size = 2) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = manipulateHospitalData()$year) +
      labs(
        title = "<span style = 'color: #56B4E9;'>Your hospital</span> compared to the <span style = 'color: #F8766D;'>national median</span>
        and a <span style = 'color: #FFC300;'>group of hospitals</span> similar to your start in RES-Q",
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
