

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)

# Directories
datadir <- "data/gaines"

# Load data
data <-  readRDS(file.path(datadir, "gaines_data_for_eez_analysis.Rds"))


# Build data
################################################################################

# Sum range across all of sovereign's EEZ areas
data1 <- data %>% 
  ungroup() %>% 
  filter(scenario=="No Adaptation") %>% 
  group_by(rcp, sovereign_iso3, sovereign, species, year) %>% 
  summarize(range_km2=sum(range_eez_km2, na.rm=T))

# Classify each RCP-EEZ-stock as:
# Never - species never resides in EEZ
# Constant - species always resides in EEZ
# Entering - species is initially absent but enters at some point
# Leaving - species is initially present but leaves at some point
key <- data1 %>% 
  ungroup() %>% 
  group_by(rcp, sovereign_iso3, sovereign, species) %>% 
  summarize(type=ifelse(sum(range_km2)==0, "never",
                        ifelse(any(range_km2==0)==F, "constant",
                               ifelse(range_km2[year==2012]==0 & any(range_km2>0), "entering", 
                                      ifelse(range_km2[year==2012]>0 & any(range_km2==0), "leaving", "other")))))

# Inspect sample size
# n_rcp * n_spp * n_nation = 4 * 779 * 159
nrow(key)
table(key$type)

# Add classification to data
data <- data %>% 
  left_join(key, by=c("rcp", "sovereign_iso3", "sovereign", "species"))


# Summarize contributions to MSY and catch by type
results <- data %>%
  group_by(rcp, sovereign_iso3, sovereign, scenario, type, year) %>% 
  summarize(msy=sum(msy_eez, na.rm=T),
            catch=sum(harvest_eez, na.rm=T))


# Inspect example results
sresults <- filter(results, sovereign_iso3=="USA")

                        
# Export data
################################################################################

# Export data
save(key, results, file=file.path(datadir, "gaines_country_level_catch_by_shift_type.Rdata"))



