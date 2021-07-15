
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/gaines/raw"
plotdir <- "figures/paper"
outputdir <- "data/gaines"

# Load data
outcomes_orig <- readRDS(file.path(datadir, "global_cc_1nation_manuscript_2019Feb12.rds"))

# Build data
################################################################################

# Build species list
spp_list <- outcomes_orig %>% 
  select(SciName, CommName, SpeciesID, ) %>% 
  unique() %>% 
  arrange(SciName)

# Export data
write.csv(spp_list, file.path(outputdir, "gaines_etal_2018_species_list.csv"), row.names=F)





