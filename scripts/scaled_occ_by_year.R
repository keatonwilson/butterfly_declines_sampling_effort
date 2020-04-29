# effort-controlled counts over time
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-25

# packages
library(tidyverse)
library(lubridate)

# importing data
effort = read_csv("./data/effort_summary.csv")
occ = read_csv("./data/full_occs.csv")

occ_inat = occ %>%
  filter(prov == "inat")


# joining
joined_occ = occ_inat %>%
  mutate(year = year(date)) %>%
  filter(year >= 1985 & year < 2020) %>%
  group_by(year, name) %>%
  summarize(n = n()) %>%
  left_join(effort, by = 'year') %>%
  mutate(index = n/year_total_person_days)

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
