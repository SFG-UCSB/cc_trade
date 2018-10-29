###################################
## Tracey Mangin ------------------
## October 27, 2018 ---------------
## climate change trade project ---

## load libraries
library(sf)
library(tidyverse)

## Directory paths
pathstart <- '~/Box/PackardCC/'
pathstartglobal <- '~/Box/Global Climate Project/'

## read in data
rratios26 <- readRDS(paste0(pathstart, "range_ratio_data/rratios_xsp_xyr_xeez_26.RDS"))
rratios45 <- readRDS(paste0(pathstart, "range_ratio_data/rratios_xsp_xyr_xeez_45.RDS"))
rratios60 <- readRDS(paste0(pathstart, "range_ratio_data/rratios_xsp_xyr_xeez_60.RDS"))
rratios85 <- readRDS(paste0(pathstart, "range_ratio_data/rratios_xsp_xyr_xeez_85.RDS"))

## create one df with all RCPs
rratiosdf <- rbind(rratios26, rratios45, rratios60, rratios85)

## model outputs - cut off = 0.01
outputs01 <- readRDS(paste0(pathstart, 'Outputs/Results/Shift1n/cutoff01/global_cc_manuscript_results_1nation_co01_20171027.rds')) %>%
  filter(RCP %in% c("RCP26", "RCP45", "RCP60", "RCP85"))

## Read in compressed high res map
map_highres <- st_read(dsn = paste0(pathstart, "plot_data/World_EEZ_v8_20140228/Simplified_high_res"), layer = "simpl_high_res_map_2014") ## high resolution

## Calculate total K for each species 
## function to calculate EEZ NPV under different management scenarios
calc_k_eez <- function(spdf, outputdf, threshold) {
  
  spdf <- rratiosdf
  outputdf <- outputs01
  threshold <- 0.01
  
  ## make some changes to the rratios dataframe
  tmp1 <- spdf %>%
    rename(SciName = species) %>%
    mutate(adj_rratio = ifelse(r < threshold, 0, r)) 
  
  ## Calculate total K for each species in each years
  outputtmp1 <- outputdf %>%
    filter(discount_rate == 0.00,
           cc_presence == "climate_change",
           scenario == "Full Adaptation") %>%
    select(RCP, SciName, SpeciesID,  CommName, year, K) %>%
    group_by(RCP, SciName, SpeciesID, CommName, year) %>%
    summarise(total_K = sum(K)) %>%
    ungroup()
  
  ## Join outputs with range dataframe
  outputtmp2 <- left_join(outputtmp1, tmp1) %>%
    mutate(eez_prop_K = total_disc_profit * adj_rratio) %>%
    group_by(RCP, EEZID, scenario) %>%
    summarize(eez_npv = sum(eez_prop_profit, na.rm = T)) %>%
    ungroup()
  
  outputfull <- outputtmp3 %>%
    filter(scenario == "Full Adaptation") %>%
    rename(opt_npv_cc = eez_npv) %>%
    select(RCP, EEZID, opt_npv_cc)
  
  outputprod <- outputtmp3 %>%
    filter(scenario == "Productivity Only") %>%
    rename(prod_npv_cc = eez_npv) %>%
    select(RCP, EEZID, prod_npv_cc)
  
  outputtmp4 <- left_join(outputfull, outputprod) 
  
  outputtmp5 <- outputtmp4 %>%
    mutate(prod_vs_opt_scaled = ifelse(prod_npv_cc == 0 & opt_npv_cc == 0, 0, (prod_npv_cc - opt_npv_cc) / opt_npv_cc), 
           prod_vs_opt_abs = ifelse(prod_npv_cc == 0 & opt_npv_cc == 0, 0, (prod_npv_cc - opt_npv_cc))) %>%
    select(EEZID, RCP, prod_vs_opt_scaled, prod_vs_opt_abs)
  
  outputtmp5$EEZID <- as.integer(outputtmp5$EEZID)
  
  return(outputtmp5)
  
}

eez_npv_comparisondf <- calc_npv_eez(spdf = rratiosdf, outputdf = outputs01, ddrate = 0.05, threshold = 0.01) %>%
  rename(EEZ_ID = EEZID)

write.csv(eez_npv_comparisondf, paste0(pathstart, "/Outputs/eez_npv_comparisondf_082118.csv"), row.names = FALSE)


