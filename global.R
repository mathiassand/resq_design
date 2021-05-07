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
