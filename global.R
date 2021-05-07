library(tidyverse)
library(tibble)
library(skimr)
library(magrittr)
library(purrr)
library(ggtext)
library(grid)
library(ggrepel)
library(directlabels)
library(reshape2)
library(shiny)
library(plotly)


resq_anon <- read_csv("anon_data_Hendrik.csv")

resq_clean <- na.omit(resq_anon)

#Current Status Dashboard variables
resq_106_CS_df <- resq_clean %>%
  # filtering for years after 2015 only, as Mirek told us RES-Q was added in 2016.
  # Dataset has a lot of misstypes, or errors? 1900, 2013, 2014 etc.
  # filtering for DTN less than 10 and larger than 400, ask Mirek told us these are "normal" cutting points
  filter(year >= 2016, year <= 2020, DTN > 10, DTN < 400, hospital == "Hospital #196", `stroke type` == "ischemic") %>%
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

resq_median <- resq_clean %>%
  group_by(country, hospital, year) %>%
  filter(DTN < 400, DTN > 10, year >= 2016) %>%
  summarise(DTNMedian = median(DTN)) %>%
  ungroup()

resq_df <- resq_clean %>%
  # filtering for years after 2015 only, as Mirek told us RES-Q was added in 2016.
  # Dataset has a lot of misstypes, or errors? 1900, 2013, 2014 etc.
  filter(year >= 2016) %>%
  group_by(hospital) %>%
  summarise(
    firstYear = min(year),
    latestYear = max(year),
    noOfYears = n_distinct(year)
  ) %>%
  ungroup()

join_df <- merge(resq_df, resq_median, by = "hospital") %>% mutate(runningYear = year - firstYear + 1)


# VISUAL 2 CUTS, FIRST THREE CUTS ARE BASED ON ANGELS INITITATIVE CUTS, DIVIDED 60-120 FURTHER
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

country_median_df <- join_df %>%
  group_by(country, year) %>%
  filter(country == "Country #53") %>%
  summarise(countryMedian = median(DTNMedian)) %>%
  ungroup()

hospital_median_df <- join_df %>%
  group_by(hospital, year) %>%
  filter(hospital == "Hospital #338") %>%
  summarise(hospitalMedian = median(DTNMedian)) %>%
  ungroup()

ch_median_df <- merge(country_median_df, hospital_median_df, by = "year")

# if(hospital_median_df$runningYear == 2){
#   
# }

cohort_df <- cohort_df %>%
  filter(year == 2018) %>%
  #filter(hospital_median_df$runningYear == 1) %>%
  select(hospital, DTNMedianCohort) %>%
  rename(startingDTNMedianCohort = DTNMedianCohort) %>%
  merge(cohort_df)

cohort_filter_df <- cohort_df %>%
  filter(startingDTNMedianCohort == "80-100", year == hospital_median_df$year) %>%
  group_by(year) %>%
  summarise(
    cohortMeanofMedian = mean(DTNMedian)
  )
