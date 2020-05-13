# Slimming and filtering inat data
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-25

# libraries
library(tidyverse)
library(lubridate)

# reading in data
inat_data = read_tsv("./data/inat_data/occurrence.txt")

# slimming down
state_list = c("California", "Oregon", "Washington", "Idaho", "Montana", 
               "Wymoning", "Nevada", "Utah", "Arizona", "New Mexico", 
               "Colorado")

inat_slim = inat_data %>%
  select(identifier, recordedBy, scientificName, 
         eventDate, countryCode, stateProvince, 
         decimalLatitude, decimalLongitude) %>%
  filter(countryCode == "US", stateProvince %in% state_list, 
         year(eventDate) >= 1985 & year(eventDate) < 2020) %>%
  drop_na()

inat_slim %>%
  summarize(earliest_date = min(eventDate), 
            recent_date = max(eventDate))

# generating a unique combination of user-day efforts 
# (general, not location specific)
# 
effort_summary = inat_slim %>%
  select(recordedBy, eventDate, stateProvince) %>%
  transmute(date = date(eventDate), 
            recordedBy = recordedBy, 
            state = stateProvince) %>%
  distinct() %>%
  mutate(year = year(date)) %>%
  group_by(year, state) %>%
  summarize(year_total_person_days = n()) 


effort_summary %>%
  ggplot(aes(x = year, y = year_total_person_days)) +
  geom_line()

write_csv(effort_summary, path = "./data/effort_summary.csv")






