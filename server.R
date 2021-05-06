# Define server logic required to draw plots ----
server <- shinyServer(function(input, output) {
  output$visual1 <- renderPlot(
    ch_median_df %>%
      ungroup() %>%
      ggplot(aes(x = runningYear, y = hospitalMedian)) +
      geom_smooth(se = FALSE, alpha = .5, color = "purple") +
      geom_point(shape = 21, color = "purple", fill = "white", size = 5, stroke = 1.7) +
      geom_smooth(aes(y = countryMedian), se = FALSE, alpha = .5, color = "red") +
      geom_point(aes(y = countryMedian), shape = 21, color = "red", fill = "white", size = 5, stroke = 1.7) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = seq(1, 6, 1)) +
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

  output$visual2 <- renderPlot(
    ch_median_df %>%
      ungroup() %>%
      ggplot(aes(x = runningYear, y = hospitalMedian)) +
      geom_smooth(se = FALSE, alpha = .5, color = "purple") +
      geom_point(shape = 21, color = "purple", fill = "white", size = 5, stroke = 1.7) +
      geom_smooth(aes(y = countryMedian), se = FALSE, alpha = .5, color = "red") +
      geom_point(aes(y = countryMedian), shape = 21, color = "red", fill = "white", size = 5, stroke = 1.7) +
      geom_smooth(data = cohort_filter_df, aes(x = runningYear, y = cohortMeanofMedian), se = FALSE, alpha = .5, color = "#56B4E9") +
      geom_point(data = cohort_filter_df, aes(x = runningYear, y = cohortMeanofMedian), shape = 21, color = "#56B4E9", fill = "white", size = 5, stroke = 1.7) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = seq(1, 6, 1)) +
      labs(
        title = "<span style = 'color: purple;'>Your hospital</span> compared to the <span style = 'color: red;'>national median</span>
        and a <span style = 'color: #56B4E9;'>group of hospitals</span> similar to your first three years in RES-Q",
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9),
      )
  )

  output$visual3 <- renderPlot(
    ch_median_df %>%
      ungroup() %>%
      ggplot(aes(x = runningYear, y = hospitalMedian)) +
      geom_smooth(se = FALSE, alpha = .5, color = "purple") +
      geom_point(shape = 21, color = "purple", fill = "white", size = 5, stroke = 1.7) +
      geom_smooth(aes(y = countryMedian), se = FALSE, alpha = .5, color = "red") +
      geom_point(aes(y = countryMedian), shape = 21, color = "red", fill = "white", size = 5, stroke = 1.7) +
      geom_smooth(data = cohort_filter_df, aes(x = runningYear, y = cohortMeanofMedian), se = FALSE, alpha = .5, color = "#56B4E9") +
      geom_point(data = cohort_filter_df, aes(x = runningYear, y = cohortMeanofMedian), shape = 21, color = "#56B4E9", fill = "white", size = 5, stroke = 1.7) +
      theme_classic() +
      coord_cartesian(ylim = c(0, 150)) +
      scale_x_continuous(breaks = seq(1, 6, 1)) +
      labs(
        title = "<span style = 'color: purple;'>Your hospital</span> compared to the <span style = 'color: red;'>national median</span>
        and a <span style = 'color: #56B4E9;'>group of hospitals</span> similar to your first three years in RES-Q",
        y = "Median DTN", x = "Years in RES-Q"
      ) +
      theme(
        plot.title = element_markdown(size = 15),
        axis.title.x = element_text(hjust = .05, vjust = 0.2),
        axis.title.y = element_text(hjust = 0.95, vjust = 0.9)
      ) +
      annotate("rect", xmin = 3.1, xmax = 6.1, ymin = 0, ymax = 150, alpha = 0.2, color = "grey", fill = "grey") +
      annotate("text", size = 6, x = 4.5, y = 10, label = "Possible future")
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

