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

# colors<-c("Hospital" = "#56B4E9", "National" = "#F8766D", "Cohort" = "#FFC300")

resq_median <- resq_clean %>%
  group_by(country, hospital, year) %>%
  filter(DTN < 400, DTN > 10, year >= 2016, year < 2021) %>%
  summarise(DTNMedian = median(DTN),
            DTNCount = n_distinct(DTN),
            patient_count = n_distinct(subject_id)) %>%
  ungroup()

resq_df <- resq_clean %>%
  # filtering for years after 2015 only, as Mirek told us RES-Q was added in 2016.
  # Dataset has a lot of misstypes, or errors? 1900, 2013, 2014 etc.
  filter(year >= 2016) %>%
  group_by(hospital) %>%
  summarise(
    firstYear = min(year),
    latestYear = max(year)
  )

join_df <- merge(resq_df, resq_median, by = "hospital") %>% mutate(runningYear = year - firstYear + 1)

join_df <- join_df %>%
  group_by(hospital) %>%
  mutate(
    maxRunningYear = max(runningYear)
  ) %>% ungroup()

DTN_breaks <- c(-Inf, 30, 45, 60, 80, 90, 100, 120, Inf)
DTN_break_labels <- c("<30", "30-45", "45-60", "60-80", "80-90", "90-100", "100-120", ">120")

expandy = function(vec, ymin=NULL) {
  
  max.val = max(vec, na.rm=TRUE)
  min.log = floor(log10(max.val))
  
  expand_limits(y=c(ymin, ceiling(max.val/10^min.log)*10^min.log))
}

# DTN_breaks <- c(-Inf, 30, 45, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, Inf)
# DTN_break_labels <- c(
#   "<30", "30-45", "45-60", "60-80", "80-100", "100-120", "120-140",
#   "140-160", "160-180", "180-200", "200-220", "220-240", ">240"
# )
