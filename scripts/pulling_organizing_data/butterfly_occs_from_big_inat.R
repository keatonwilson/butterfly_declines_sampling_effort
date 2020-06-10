# Attempting to filter out all butterfly data
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-07

library(tidyverse)
library(lubridate)
# reading in data
inat_data = read_tsv("./data/inat_data/occurrence.txt")

# slimming down
state_list = c("California", "Oregon", "Washington", "Idaho", "Montana", 
               "Wyoming", "Nevada", "Utah", "Arizona", "New Mexico", 
               "Colorado")

inat_west_small = inat_data %>%
  filter(countryCode == "US", stateProvince %in% state_list, 
         year(eventDate) >= 1985 & year(eventDate) < 2020)

rm(inat_data)

inat_working = inat_west_small %>%
  select(species, acceptedScientificName, order,
         family, genus, decimalLatitude, decimalLongitude,
         countryCode, stateProvince, eventDate
         )

leps = inat_working %>%
  filter(order == "Lepidoptera")

family_list = c("Hedylidae", "Hesperiidae", "Lycaenidae", 
                "Nymphalidae", "Papilionidae", "Pieridae", 
                "Riodinidae")

butts = leps %>%
  filter(family %in% family_list) %>%
  drop_na()

# writing to csv
write_csv(butts, "./data/big_butterfly_occs_all_species.csv")

write_csv(butts %>% 
            select(acceptedScientificName, species, genus, family, order, 
                   decimalLatitude, decimalLongitude, countryCode, 
                   stateProvince, eventDate), 
          "~/Desktop/butterfly_occ_export.csv")
