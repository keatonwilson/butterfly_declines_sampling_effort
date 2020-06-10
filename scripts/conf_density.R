# Calculate area based on kernal density estimation
# Jeffrey C. Oliver
# jcoliver@arizona.edu
# 2020-05-18

rm(list = ls())

################################################################################
library(ks)         # kernel density estimation
library(pracma)     # area of polygon calculation
library(tidyverse)  # data wrangling, especially dplyr
library(SpatialEpi) # conversion to kilometers

# reading in data
butt_occs = read_csv("./data/big_butterfly_occs_all_species.csv")

# filtering out species with less than 10 observations
butt_occs = butt_occs %>% 
  dplyr::group_by(species) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::right_join(butt_occs, by = "species") %>%
  dplyr::filter(n >= 10)

# Splitting into a list by species
butt_split = split(butt_occs, butt_occs$species)

# Extract a few items
butt_split_mini = butt_split[c(1,2,3)]

#' Calculate area based on two-dimensional kernel density estimate
#' 
#' @param df data frame with longitude and latitude coordinates in decimal 
#' degrees; assumes columns are named "decimalLongitude" and "decimalLatitude", 
#' respectively; df should also have column "species"
#' @param conf_level numeric conficence level between 0 and 1.
density_area_calc = function(df, conf_level = 0.95) {
  # Make sure conf_level is appropriate
  if (conf_level < 0 || conf_level > 1.0) {
    message("conf_level in density_area_calc must be between zero and one.")
    return(NA)
  }
  
  # Extract only coordinates
  df_coords = df %>%
    dplyr::select(x = decimalLongitude, y = decimalLatitude)
  
  # Convert to kilometers
  coords_grid = SpatialEpi::latlong2grid(df_coords)
  
  # A bit easier to work with matrices for kernel density estimations
  coords_matrix = as.matrix(coords_grid)

  # Use Hscv for cross-validation of bandwidth to use  
  bandwidth_select = ks::Hscv(x = coords_matrix)

  dens_function = ks::kde(x = coords_matrix, 
                          H = bandwidth_select,
                          compute.cont = TRUE)
  
  # ks::kde provides the complement for determining envelope; that is for 
  # calculating the density of 95% of observations, need to use the element 
  # named "5%" that is part of list returned from ks::kde. i.e. passing
  # ks::kde$cont["5%"] to the levels argument of contourLines will provide the 
  # contours that contain the 95% of the density. So we need to convert the 
  # conf_level argument passed to this function to a character in this format.

  # Since elements are integers from 0 to 100, we also need to round to nearest
  # integer.
  level = (1 - conf_level) * 100
  level = round(x = level, digits = 0)
  
  # Create string that will be used to access named element
  level_string = paste0(level, "%")
  
  # Calculate contour lines; contourLines will produce as many elements as there 
  # are non-overlapping contours
  all_contours = contourLines(x = dens_function$eval.points[[1]],
                               y = dens_function$eval.points[[2]],
                               z = dens_function$estimate,
                               levels = dens_function$cont[level_string])
  
  # Each element of all_contours has a non-overlapping contour; need to do area
  # calculation for each contour. For most species, all_contours will have a 
  # single element; only species with disjunct distributions will have multiple 
  # elements in all_contours
  area_list <- lapply(X = all_contours, 
                      FUN = function(x) {
                        abs(pracma::polyarea(x = x[["x"]], 
                                             y = x[["y"]]))
                      })
  total_area <- sum(unlist(area_list))
  
  # Bind and return
  df_out = data.frame(name = df$species[1], 
                      area = total_area, 
                      average_lat = mean(df$decimalLatitude), 
                      average_long = mean(df$decimalLongitude), 
                      max_lat = max(df$decimalLatitude), 
                      min_lat = min(df$decimalLatitude), 
                      max_long = max(df$decimalLongitude), 
                      min_long = min(df$decimalLongitude))
  return(df_out)
}

# To test
ranges = bind_rows(lapply(X = butt_split, 
                          FUN = density_area_calc))
write_csv(ranges, "./output/range_confs_density.csv")