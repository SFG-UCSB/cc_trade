###################################
## Tracey Mangin ------------------
## September 6, 2019 --------------
## country-level climate change ---

## load libraries
library(sf)
library(tidyverse)

## Directory path
pathstart <- '~/Box/Global Climate Project/'
pathstartcc <- '~/Box/PackardCC/'

## Load spatial data from Jorge
load('~/Box/Global Climate Project/input_files/Data prep/Spatial data/UpdateResults_11_2017.RData')

cc_outputs <- readRDS(paste0(pathstart, "Outputs/Results/Final Runs/compiled_global_cc_results_20171005_1nation.rds"))

## read in data
rratios26 <- readRDS(paste0(pathstartcc, "range_ratio_data/rratios_xsp_xyr_xeez_26.RDS"))
rratios45 <- readRDS(paste0(pathstartcc, "range_ratio_data/rratios_xsp_xyr_xeez_45.RDS"))
rratios60 <- readRDS(paste0(pathstartcc, "range_ratio_data/rratios_xsp_xyr_xeez_60.RDS"))
rratios85 <- readRDS(paste0(pathstartcc, "range_ratio_data/rratios_xsp_xyr_xeez_85.RDS"))

## create one df with all RCPs
rratiosdf <- rbind(rratios26, rratios45, rratios60, rratios85)

rratiosdf2 <- rratiosdf %>%
  rename(SciName = species) 

## remove neis, filter for FMSY
sp_ids <- unique(cc_outputs$SpeciesID)
sp_ids2 <- sp_ids[1:779]

fmsy_outputs <- cc_outputs %>%
  filter(discount_rate == 0.00,
         policy == "FMSY",
         SpeciesID %in% sp_ids2,
         cc_presence == "climate_change",
         year >= 2012 & year <= 2100) %>%
  select(SciName, SpeciesID, CommName, RCP, year, biomass, harvest, profit, FvFMSY)

## combine outputs
fmsy_x_eez <- left_join(fmsy_outputs, rratiosdf2) %>%
  select(EEZID, EEZ, Sovereign, Country, SciName, CommName, RCP, year, total_biomass = biomass, 
         total_profit = profit, total_harvest = harvest, rangekm2, total_range, rel_range = r) %>%
  mutate(eez_x_sp_h = rel_range * total_harvest,
         eez_x_sp_p = rel_range * total_profit)








