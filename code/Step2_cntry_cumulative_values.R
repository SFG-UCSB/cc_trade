
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
data <- readRDS(file.path(datadir, "gaines_data_for_eez_analysis.Rds"))


# Build data
################################################################################

# Function for percent difference
perc_diff <- function(x1, x2){
  out <- (x2-x1) / x1 * 100
}

# Global cumulative catch, profits, and p(above B/BMSY)
gdata <- data %>% 
  group_by(rcp, scenario) %>% 
  summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
            msy_tot2=sum(msy_eez[year==2100], na.rm=T),
            c_tot=sum(harvest_eez, na.rm=T),
            p_tot=sum(profit_eez, na.rm=T),
            bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
            bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
  # Calculate cumulative catch and profits relative to no adaptation
  ungroup() %>% 
  group_by(rcp) %>% 
  mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
         p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
         b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])

# Country-level cumulative catch, profits, and p(above B/BMSY)
cdata <- data %>% 
  group_by(sovereign_iso3, sovereign, rcp, scenario) %>% 
  summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
            msy_tot2=sum(msy_eez[year==2100], na.rm=T),
            c_tot=sum(harvest_eez, na.rm=T),
            p_tot=sum(profit_eez, na.rm=T),
            bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
            bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
  # Calculate cumulative catch and profits relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, rcp) %>% 
  mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
         p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
         b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])

# Country-species-level cumulative catch, profits, and p(above B/BMSY)
csdata <- data %>% 
  group_by(sovereign_iso3, sovereign, rcp, scenario, species) %>% 
  summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
            msy_tot2=sum(msy_eez[year==2100], na.rm=T),
            c_tot=sum(harvest_eez, na.rm=T),
            p_tot=sum(profit_eez, na.rm=T),
            bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
            bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
  # Calculate cumulative catch and profits relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, rcp, species) %>% 
  mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
         p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
         b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])


# Export data
################################################################################

# Export results
saveRDS(gdata, file.path(datadir, "gaines_global_level_results.Rds"))
saveRDS(cdata, file.path(datadir, "gaines_country_level_results.Rds"))
saveRDS(csdata, file.path(datadir, "gaines_country_stock_level_results.Rds"))

