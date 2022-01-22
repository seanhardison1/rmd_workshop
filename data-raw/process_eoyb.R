library(tidyverse)

# research question - how have densities of smooth cordgrass (S. alterniflora) 
# changed over time across the VCR?

# download and process data----
spart_bio <- 
  read.csv('http://www.vcrlter.virginia.edu/cgi-bin/fetchdataVCR.cgi/1/VCR09159/EOYB_data.csv') %>%
  filter(speciesName == "Spartina alterniflora") %>% 
  
  # select and rename columns
  dplyr::select(year = EOYBYear,
                site = siteName,
                transect = Transect,
                habitat_type = locationName,
                replicate = Replicate,
                tot_biomass = totalMass,
                latitude,
                longitude) %>% 

  # remove "Alt. transition" habitat type which has low sample size
  filter(habitat_type != "Alt. Transition",
         
         # remove transects D, E, and F, which also have small sample size
         !transect %in% c("D", "E", "F")) %>% 
  
  # replicate is nested within transect, so aggregating up to
  # the level of transect by finding mean biomass per transect
  # simplifies the analysis
  group_by(year, site, habitat_type, transect) %>% 
  dplyr::summarise(mean_transect_biomass = mean(tot_biomass, na.rm = T)) %>% 

  # one of the biomass observations is NA - drop it
  filter(!is.na(mean_transect_biomass)) %>% 
  
  # convert categorical variables to factors
  mutate(site = factor(site),
         transect = factor(transect, levels = c("A","B","C","D")),
         habitat_type = factor(habitat_type,
                               levels = c("Transition (4)",
                                          "High Marsh (3)",
                                          "Low Marsh (2)",
                                          "Creekbank (1)"
                                          )))

# visualize----
ggplot(spart_bio) +
  geom_point(aes(x = year, y = mean_transect_biomass)) +
  facet_grid(habitat_type~transect)

# save processed data----
save(spart_bio, file = here::here("data/processed_spartina_biomass.rdata"))
