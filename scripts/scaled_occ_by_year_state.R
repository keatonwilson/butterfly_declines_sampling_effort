# effort-controlled counts over time
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-06

# packages
library(tidyverse)
library(lubridate)

# importing data
effort = read_csv("./data/effort_summary.csv")
occ = read_csv("./data/big_butterfly_occs_all_species.csv")

# joining
joined_occ = occ %>%
  mutate(year = year(eventDate)) %>%
  filter(year >= 1985 & year < 2020) %>%
  group_by(year, species, stateProvince) %>%
  summarize(n = n()) %>%
  left_join(effort, by = 'year') %>%
  mutate(index = n/year_total_person_days)


joined_occ = joined_occ %>%
  rename(counts = n, effort = year_total_person_days, 
         effort_scaled_counts = index, state = stateProvince)

write_csv(joined_occ, path = "./output/effort_scaled_counts_by_state.csv")
