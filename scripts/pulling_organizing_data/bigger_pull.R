# Pulling occurence records for more species
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-26

# packages
library(tidyverse)
library(stringr)
library(spocc)

full_list = read_csv("./data/declines_tax_probs.csv")
# filtering out tax problems
no_probs = full_list %>%
  filter(tax_prob == FALSE)

collapsed_names = paste(word(no_probs$species, 1), 
                        word(no_probs$species, 2))
#problem species
naughty_list = c("Pontia protodice", "Anthocharis stella")
collapsed_names = collapsed_names[collapsed_names != "Pontia protodice"]
collapsed_names = str_replace(collapsed_names, "Everes comyntas", 
                              "Cupido comyntas")

butt_obs = function(names){
  df = data.frame()
  for (i in 1:length(names)){
    print(paste("Querying", names[i]))
    sub = occ(query = names[i], from = c("gbif", "inat"), limit = 10000, 
              has_coords=TRUE, 
              gbifopts=list(continent='north_america'), 
              geometry = c(-140, 20, -90, 60))
    
    sub_gbif_df = sub$gbif$data[[1]] %>%
      dplyr::select(longitude, 
                    latitude, 
                    src_name = species, 
                    date = eventDate, key) %>%
      mutate(key = as.numeric(key)) %>%
      mutate(prov = "gbif") %>%
      mutate(name = names[i])
    
    
    sub_inat_df = sub$inat$data[[1]] %>%
      dplyr::select(longitude, 
                    latitude, 
                    src_name = name, 
                    date = observed_on, 
                    key = id) %>%
      mutate(longitude = as.numeric(longitude), 
             latitude = as.numeric(latitude), 
             key = as.numeric(key)) %>%
      mutate(prov = "inat") %>%
      mutate(name = names[i])
    
    df = bind_rows(sub_gbif_df, sub_inat_df, df)
    
  }
  return(df)
}


#Running function above
butterfly_data = butt_obs(collapsed_names)

# Will need to do some duplicate removal
butterfly_data_clean = butterfly_data %>%
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude),
         name = word(name, 1, 2)) %>%
  distinct(longitude, latitude, date, name, .keep_all = TRUE) %>%
  drop_na()
