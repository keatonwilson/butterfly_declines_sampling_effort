# effort-controlled counts over time
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-25

# packages
library(tidyverse)
library(lubridate)

# importing data
effort = read_csv("./data/effort_summary.csv")
occ = read_csv("./data/candidate_occurences.csv")

# joining
joined_occ = occ %>%
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
  ylab("Yearly total occurences/number of bserver days per year") +
  xlab("Year") +
  theme(strip.text = element_text(face = "italic"))

ggsave(filename = "./output/scaled_occ_by_year.png", plot = g1)
