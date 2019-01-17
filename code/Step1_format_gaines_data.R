
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
datadir <- "data/gaines/raw"
plotdir <- "figures/edf_targets"
outdir <- "data/gaines"

# Load data
data_orig <- readRDS(file.path(datadir, "eez_delta_k_df.rds"))

# Load final results
results_orig <- readRDS(file.path(datadir, "global_cc_manuscript_results_1nation_20171005.rds"))

# Shape parameter
p <- 0.188
pdiv <- (p+1)^(1/p)


# Derive r values from Tracey's data
################################################################################

# MSY time series aren't provided
# However, in this PT parameterization, r=FMSY
# Thus, MSY = r*K/pdiv where r=FMSY

# BMSY = K/pdiv (from Gaines et al 2018 supplement)
# MSY = rk/pdiv (from Gaines et al 2018 supplement)
# FMSY = r (apparently true)

# Here is proof:
r_test <- results_orig %>% 
  mutate(r=FMSY, 
         msy=r*K/pdiv) %>% 
  filter(year==2012)
plot(msy ~ MSY2012, r_test)

# Learn about data
################################################################################

# DATA - changes in range size and carrying capacity
# --------------------------------------------------------------------
# rangekm2 = range (km2) of species(i) in EEZ(j) in year(t) 
# total_range = range (km2) of species(i) in year(t), globally
# r = rangekm2 / total_range = proportion of the range of species(i) in year(t) inside EEZ(j)
# adj_rratio = doesn't matter
# total_k = carrying capacity of species(i) in year(t), globally
# eez_k = carrying capacity of species(i) in EEZ(j) in year(t) 
# total_k0 = carrying capacity of species(i) in 2012, globally
# eez_k0 = carrying capacity of species(i) in EEZ(j) in 2012
# delta_k = eez_k - eez_k0 = change in carrying capacity relative to 2012
# delta_k_r = eez_k / eez_k0 = proportion of K in of species(i) in EEZ(j) in year(t) relative to 2012

# I want to know how biomass, catch, and profit has changed
# for species(i) in EEZ(j) in year(t) relative to 2012

# This means I have to allocate the global biomass, harvest, and profit for species(i) in year(t)
# to its constituent EEZs, EEZ(j). I'm going to assume that these are spread perfect uniformly
# across its range which means I want to multipy each value by range(EEZ)/range(total) 
# then compare difference between 2012 and the reference year.

# Inspect one scenario (RCP-EEZ-species)
sdata <- filter(data_orig, RCP=="RCP26" & Country=="United States" & SciName=="Gadus morhua")

# Format scenario
sdata1 <- sdata %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         eez_id=eezid,
         range_eez_km2=rangekm2,
         range_tot_km2=total_range, 
         range_prop=r, range_prop_adj=adj_rratio, 
         k_tot=total_k, k_eez=eez_k, k0_eez=eez_k0, k0_tot=total_k0) %>% 
  # Add columns
  mutate(k_prop=k_eez / k_tot, # I don't trust this because Kprops don't sum to one later
         k_prop1=range_prop)  %>% # so I trust this value
  # Rearrange columns 
  select(rcp, sovereign, country, eez_id, eez, 
         species_id, species, comm_name, year,
         range_eez_km2, range_tot_km2, range_prop,
         k_eez, k_tot, k_prop, k0_eez, k0_tot, k_prop1)
plot(k_prop ~ k_prop1, sdata1)

# Subset one species from results
sresults <- filter(results_orig, SciName=="Gadus morhua" & cc_presence=="climate_change")

# Inspect scenarios
# 5 RCPs: none, 2.6, 4.5, 6.0, 8.5
# 4 scenarios: none, full, prod only, range only
# 3 discount rates
# Years: 2012-2100 (89)
scenarios <- sresults %>%
  group_by(RCP, cc_presence, scenario, HCR, discount_rate) %>% 
  summarize(n=n())

# Format results
sresults1 <- sresults %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         bbmsy=bvbmsy,
         profit_discounted=discounted_prof,
         fmort=fish_mort_rate,
         ffmsy=fvfmsy) %>% 
  # Add MSY
  mutate(r=fmsy,
         msy=r*k/pdiv) %>% 
  # Remove non-critical scenarios
  filter(cc_presence=="climate_change" & discount_rate==0)
  

# Format full dataset
################################################################################

# Add soverign ISO3s
joint_areas <- c("Australia - Papua New Guinea", "Australia/Indonesia", 
                 "Disputed", "Joint Development", "Joint Regime")
iso_key <- data.frame(sovereign=sort(unique(data_orig$Sovereign)), stringsAsFactors=F) %>% 
  mutate(sovereign=as.character(sovereign),
         sovereign_iso3=countrycode(sovereign, "country.name", "iso3c"),
         sovereign_iso3=ifelse(sovereign=="Micronesia", "FSM", sovereign_iso3), 
         sovereign_iso3=ifelse(sovereign=="High Seas", "ABNJ", sovereign_iso3), 
         sovereign_iso3=ifelse(sovereign %in% joint_areas, "Other", sovereign_iso3))
iso_key$sovereign[is.na(iso_key$sovereign_iso3)]

# Format data
data <- data_orig %>%
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         eez_id=eezid,
         range_eez_km2=rangekm2,
         range_tot_km2=total_range, 
         range_prop=r, range_prop_adj=adj_rratio, 
         k_tot=total_k, k_eez=eez_k, k0_eez=eez_k0, k0_tot=total_k0) %>% 
  # Add ISO3 sovereign
  left_join(iso_key, by="sovereign") %>% 
  # Add columns
  mutate(k_prop=k_eez / k_tot,
         range_prop=ifelse(range_tot_km2==0, 0, range_prop)) %>% 
  # Rearrange columns 
  select(rcp, sovereign_iso3, sovereign, country, eez_id, eez, 
         species_id, species, comm_name, year,
         range_eez_km2, range_tot_km2, range_prop,
         k_eez, k_tot, k_prop, k0_eez, k0_tot)

# Format results
results <- results_orig %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         bbmsy=bvbmsy,
         profit_discounted=discounted_prof,
         fmort=fish_mort_rate,
         ffmsy=fvfmsy) %>% 
  # Add MSY
  mutate(r=fmsy,
         msy=r*k/pdiv) %>% 
  # Remove non-critical scenarios
  filter(cc_presence=="climate_change" & discount_rate==0)

# Perform some data checks
# Infinite B/BMSY when BMSY is 0
results_check <- results %>%
  filter(k==0)

# Merge EEZ range shift projections and global biomass, catch, and profit projections
# Final should be nrow(data) * 4 scenarios : nrow(data) * 4
final <- data %>% 
  select(rcp:range_prop) %>% 
  # Add biomass, harvest, profit
  left_join(select(results, rcp, scenario, species, year, r, k, msy, bmsy, fmsy, bbmsy, ffmsy, fmort, biomass, harvest, profit), by=c("rcp", "species", "year")) %>% 
  # Calculcate proportions in EEZ
  mutate(msy_eez=msy*range_prop,
         biomass_eez=biomass*range_prop,
         harvest_eez=harvest*range_prop,
         profit_eez=profit*range_prop) %>% 
  # Housekeeping tasks
  mutate(bbmsy=ifelse(is.infinite(bbmsy), NA, bbmsy)) # This happens when K=0 because BMSY=0


# Export full dataset
################################################################################

# Export
saveRDS(final, file.path(outdir, "gaines_data_for_eez_analysis.Rds"))

