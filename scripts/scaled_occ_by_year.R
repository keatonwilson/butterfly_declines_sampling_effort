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
  group_by(year, species) %>%
  summarize(n = n()) %>%
  left_join(effort, by = 'year') %>%
  mutate(index = n/year_total_person_days)

top_6 = joined_occ %>%
  ungroup() %>%
  group_by(species) %>%
    summarize(total_counts = sum(n)) %>%
  arrange(desc(total_counts)) %>% 
  head() %>%
  pull(species)

joined_occ %>%
  filter(species %in% top_6) %>%
  ggplot(aes(x = year, y = index)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  theme_classic() +
  geom_smooth(method = "lm") +
  facet_wrap(~ species) +
  ylab("Yearly total occurences/number of observer days per year") +
  xlab("Year") +
  theme(strip.text = element_text(face = "italic"))

# plots
g1 = ggplot(joined_occ, aes(x = year, y = index)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  theme_classic() +
  geom_smooth(method = "lm") +
  facet_wrap(~ name) +
  ylab("Yearly total occurences/number of observer days per year") +
  xlab("Year") +
  theme(strip.text = element_text(face = "italic"))

ggsave(filename = "./output/scaled_occ_by_year.png", plot = g1)

joined_occ = joined_occ %>%
  rename(counts = n, effort = year_total_person_days, 
         effort_scaled_counts = index)

write_csv(joined_occ, path = "./output/effort_scaled_counts.csv")
