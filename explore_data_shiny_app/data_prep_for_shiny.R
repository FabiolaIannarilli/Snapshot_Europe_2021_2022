# Prep Snapshot Europe 2021-22 data for shiny app

# load libraries
library(tidyverse)
library(lubridate)

# load data
depl <- read.csv(file = "data_output/DataS1/SNAPSHOT_Europe_2021_2022_deployments.csv")
sequences <- read.csv(file = "data_output/DataS1/SNAPSHOT_Europe_2021_2022_sequences.csv")

# prep for subproject selector
subprojs <- sort(unique(depl$subproject_name))

# prep data for map (spatial coverage)
loc <- depl %>% 
  select(subproject_name, placename, latitude, longitude) %>% 
  distinct()

# prep for temporal coverage
n_daily_seq <- sequences %>%
  mutate(record_date = as.Date(ymd_hms(start_time))) %>%
  group_by(deployment_id, record_date) %>%
  summarise(n_seq = n()) %>% 
  left_join(depl %>% 
              select(subproject_name, placename, deployment_id) %>% 
              distinct())

depl_info <- depl %>%
  select(subproject_name, placename, deployment_id, start_date, end_date, year) %>%
  mutate(start_date = as.Date(ymd_hms(start_date)),
         end_date = as.Date(ymd_hms(end_date))) 

# prep data for taxonomic coverage
n_species_seq <- sequences %>%
  group_by(deployment_id, common_name) %>%
  summarise(n_seq = n()) %>% 
  left_join(depl %>% 
              select(subproject_name, placename, deployment_id) %>% 
              distinct())


save(list = c("subprojs", "loc", "n_daily_seq", "depl_info", "n_species_seq"), 
     file = "explore_data_shiny_app/data_for_shiny/data_prep.RData")

