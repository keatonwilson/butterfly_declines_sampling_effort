# 95% confidence ellipses by species
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-08

# packages
library(tidyverse)
library(car)
library(rgdal)
library(sp)
library(SpatialEpi)


# reading in occ data
butt_occs = read_csv("./data/big_butterfly_occs_all_species.csv")

# filtering out species with less than 10 observations
butt_occs = butt_occs %>% 
  group_by(species) %>%
  summarize(n = n()) %>%
  right_join(butt_occs, by = "species") %>%
  filter(n >= 10)

# Splitting into a list by species
butt_split = split(butt_occs, butt_occs$species)
butt_split_mini = butt_split[c(1,2,3)]

# building area function
area_calc = function(df){
  # pulling out coords
  df_coords = df %>%
    dplyr::select(x = decimalLongitude, y = decimalLatitude) 
  # converting to km
  coords_grid = SpatialEpi::latlong2grid(df_coords)
  
  # calculating center
  me = apply(coords_grid, 2, mean)
  
  # calculating variance (shape)
  v <- var(coords_grid)
  
  # radius
  rad <- sqrt(2*qf(0.5, 2, nrow(coords_grid)-1))
  
  #building the ellipse
  z <- ellipse(me, v, rad, segments=1001, draw = FALSE)
  
  #calculating distance to center for each segment point
  dist2center <- sqrt(rowSums((t(t(z)-me))^2))
  
  # calculating the area
  area = pi*min(dist2center)*max(dist2center)
  
  # binding and returning
  df_out = data.frame(name = df$species[1], 
                      area = area)
  return(df_out)
}

# testing
ranges = bind_rows(lapply(butt_split, area_calc))
write_csv(ranges, "./output/range_confs.csv")
