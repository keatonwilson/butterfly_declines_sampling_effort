# Exploring sampling effort from iNat data
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-21

# packages
library(tidyverse)
library(rinat)
library(lubridate)

# setting up bounding box
min_long = -100
max_long = -120
min_lat = 30
max_lat = 49

bounds = c(min_lat, min_long, max_lat, max_long)

# So, let's just test
# test = get_inat_obs(year = 1975, 
#                     quality = "research", 
#                     bounds = bounds, 
#                     maxresults = 10000)

# ok, let's set up a loop to loop through this
# Because of the size of these data, we're going to to loop by month and year

year_list = seq(from = 1985, to = 2020, by = 1)
month_list = seq(1:12)

inat_data = data.frame()
for(i in seq_along(year_list)){
  for(j in seq_along(month_list)){
    for(k in 1:31){
      temp = try(get_inat_obs(year = year_list[[i]], 
                   month = month_list[[j]],
                   day = k,
                   maxresults = 10000,
                   bounds = bounds, 
                   quality = "research"))
      if(class(temp) == "try-error"){
        print(paste0("No data for ", 
                     year_list[[i]], 
                     "-", 
                     month_list[[j]], 
                     "-",
                     k,
                     " combination"))
      } else if(nrow(temp) == 10000){
        print("Max size reached for this month/year combination: problematic")
        temp = temp %>% 
          select(scientific_name, datetime, user_login) %>%
          mutate(date = date(datetime)) %>%
          select(-datetime, -scientific_name) %>%
          distinct()
        
        inat_data = bind_rows(inat_data, temp)
        print(dim(inat_data))
      } else {
        temp = temp %>% 
          select(scientific_name, datetime, user_login) %>%
          mutate(date = date(datetime)) %>%
          select(-datetime, -scientific_name) %>%
          distinct()
        inat_data = bind_rows(inat_data, temp)
        print(dim(inat_data))
      }
    }  
  }
  size = nrow(inat_data)
  print(paste("Number of total records =", size, "at year", year_list[[i]]))
}

# saving data
saveRDS(inat_data, "./data/inat_data.rds")