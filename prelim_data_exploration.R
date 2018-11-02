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

## Load spatial data from Jorge
load('~/Box/Global Climate Project/input_files/Data prep/Spatial data/UpdateResults_11_2017.RData')

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

## Calculate change in range two ways: 1) By EEZ for each species and 2) by EEZ (total range)
## --------------------------------------------------------------------------------------------------

## range threshhold
threshold <- 0.01
  
## make some changes to the rratios dataframe
rratiosdf2 <- rratiosdf %>%
  rename(SciName = species) %>%
  mutate(adj_rratio = ifelse(r < threshold, 0, r)) ## if the ratio of range is < 1%, the adjusted range is 0
  
## join with the K outputs and determine K in each country in each year
## -------------------------------------------------------------------
k_over_time_df <- outputs01 %>%
  filter(discount_rate == 0.00,
         cc_presence == "climate_change",
         scenario == "Full Adaptation") %>%
  select(RCP, SciName, SpeciesID,  CommName, year, K) %>%
  rename(total_k = K) %>%
  left_join(rratiosdf2) %>%
  mutate(eez_k = adj_rratio * total_k) %>%
  select(RCP:year, EEZID:adj_rratio, total_k, eez_k)

k_2012 <- k_over_time_df %>%
  filter(year == 2012) %>%
  mutate(total_k0 = total_k,
         eez_k0 = eez_k) %>%
  select(RCP, SciName, SpeciesID, EEZID, EEZ, total_k0, eez_k0)

k_df <- k_over_time_df %>%
  left_join(k_2012)

## calculate change in K for each species
delta_k_df <- k_df %>%
  mutate(delta_k = eez_k - eez_k0,
         delta_k_r = delta_k / eez_k0)


## now for each eez
delta_k_eez_df <- k_df %>%
  group_by(RCP, year, EEZID, EEZ, Sovereign, Country) %>%
  summarise(total_eez_k = sum(eez_k, na.rm = T),
            total_k = sum(total_k, na.rm = T),
            total_eez_k0 = sum(eez_k0, na.rm = T),
            total_k0 = sum(total_k0, na.rm = T)) %>%
  ungroup() %>%
  mutate(delta_k_eez = total_eez_k - total_eez_k0,
         detta_k_eez_r = delta_k_eez / total_eez_k0)





# ## 1) Calculate K in each country and compare to 2012
# ## -------------------------------------------------------------------
# outputs01_sub <- outputs01 %>%
#   filter(discount_rate == 0.00,
#          cc_presence == "climate_change",
#          scenario == "Full Adaptation") %>%
#   select(RCP, SciName, SpeciesID,  CommName, year) %>%
#   left_join(rratiosdf2)
#   
# initial_range <- outputs01_sub %>%
#   filter(year == 2012) %>%
#   select(RCP, SpeciesID, EEZID, rangekm2, total_range, r, adj_rratio) %>%
#   rename(init_rangekm2 = rangekm2,
#          init_total_range = total_range,
#          init_r = r,
#          init_adj_rratio = adj_rratio)
# 
# # ## change in range by species by EEZ 
# # sp_output <- outputs01_sub %>%
# #   filter(year %in% c(2050, 2100)) %>%
# #   left_join(initial_range) %>%
# #   mutate(change_rrange = adj_rratio- init_adj_rratio)
# # 
# # ## clean it up
# # sp_output2 <- sp_output %>%
# #   select(RCP:Country, init_adj_rratio, adj_rratio, change_rrange)
# 
# 
# ## 2) Calculate change in range ratio for entire EEZ (sum of all ranges)
# ## ------------------------------------------------------------------------------
# init_total_eez_range <- outputs01_sub %>%
#   filter(year == 2012) %>%
#   select(RCP, SpeciesID, EEZID, rangekm2) %>%
#   group_by(RCP, EEZID) %>%
#   summarise(total_range_eezkm2 = sum(rangekm2)) %>%
#   ungroup()
# 
# ## total range all sp 
# total_range_df <- outputs01_sub %>%
#   filter(year %in% c(2012, 2050, 2100)) %>%
#   select(RCP, SpeciesID, year, total_range) %>%
#   group_by(RCP, year) %>%
#   summarise(total_range_all_km2 = sum(total_range)) %>%
#   ungroup()
#   
# ## total range all sp 2012
# total_range_all_2012_df <- total_range_df %>%
#   filter(year == 2012, 
#          RCP == "RCP26") 
# 
# total_range_all_2012 <- as.numeric(total_range_all_2012_df[,3])
# 
# ## join the two dfs to calculate ratio for 2012
# init_rratio_df <- init_total_eez_range %>%
#   mutate(total_global_range = total_range_all_2012) %>%
#   mutate(init_rratio = total_range_eezkm2 / total_global_range) %>%
#   rename(init_range_eezkm2 = total_range_eezkm2,
#          init_total_range_all = total_global_range)
# 
# ## calc eez totals in 2050 and 2100
# total_eez_range <- outputs01_sub %>%
#   filter(year %in% c(2050, 2100)) %>%
#   select(RCP, SpeciesID, EEZID, year, rangekm2) %>%
#   group_by(RCP, EEZID, year) %>%
#   summarise(total_range_eezkm2 = sum(rangekm2)) %>%
#   ungroup() %>%
#   left_join(total_range_df) %>%
#   mutate(rratio = total_range_eezkm2 / total_range_all_km2)
# 
# 
# ## join and calculate differences in r ratio
# total_range_ratios <- total_eez_range %>%
#   left_join(init_rratio_df) %>%
#   mutate(change_rrange = rratio - init_rratio)
# 
# id_df <- EEZ_HS_ids %>%
#   rename(EEZID = EEZ_ID) %>%
#   select(EEZID: Sovereign)
#   
# ## clean it up
# total_rratios <- total_range_ratios %>%
#   select(RCP, EEZID, year, rratio, init_rratio, change_rrange) %>%
#   left_join(id_df) %>%
#   select(RCP, EEZID, EEZ, Country, Sovereign, year:change_rrange)
#   
# 
# ## Check out what's happening in Peru to make sure this all makes sense
# ## ------------------------------------------------------------------------
# sp_output_pe <- sp_output %>%
#   filter(EEZID == 138,
#          year %in% c(2100),
#          adj_rratio > 0 & init_adj_rratio > 0) %>%
#   group_by(Country) %>%
#   summarise(sum_range = sum(rangekm2),
#             sum_all_2100 = sum(total_range),
#             sum_init_range = sum(init_rangekm2),
#             sum_all_init = sum(init_total_range)) %>%
#   ungroup()
          
## Thoughts
## A few things going on here. The % of total range of a stock at any given year can be different if the stock is shifting.
## Another thing, however, is that the stock may still be in the EEZ (let's say 100%), but it shrinks (or grows). This
## is not a transboundary issue in that the resource is moving to another country, but that the available resource is
## shrinking (or growing) (i.e. productiivity issue.).
## Also, of course, both could happen.
## There are small differences in total range % even though range for all stocks in Peru for example are shrinking, and I
## believe this is because the overall range is also shrinking. 
## --> Maybe the denominator should be initial range?
## How to show both issues?
## What do we want to show for the short slide deck with EDF?





## Select target countries
## (United States, Mexico, Peru, Chile, European Union, China, Myanmar, Japan, Indonesia, Philippines, Pacific Islands and Vietnam).
## EU members: Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany,
## Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia,
## Slovenia, Spain, Sweden, United Kingdom
## ---------------------------------------------------------------------------------


head(sp_output2)
head(total_rratios)

eezid_subset <- c(160, 163, 170, 135, 138, 224, 225, 59, 71, 187, 81, 141, 145, 175, 70, 174, 179, 176, 80, 181, 184, 69, 189,
                  68, 177, 178, 55, 57, 183, 72, 188, 58, 182, 222, 180, 227, 228, 209, 205, 210, 216, 15, 207, 9, 8, 148, 157, 
                  212, 11, 10, 13, 12, 6, 7, 18, 17, 5, 155, 151, 153, 147, 162, 154, 156, 3, 146, 152, 19, 161)
eu <- c(59, 71, 187, 81, 141, 145, 175, 70, 174, 179, 176, 80, 181, 184, 69, 189, 68, 177, 178, 55, 57, 183, 72, 188, 
        58, 182, 222, 180, 227, 228)
us <- c(160, 163, 170)
chile <- c(225, 225)
denmark <- c(141, 145, 175)
portugal <- c(55, 57, 183)
spain <- c(58, 182)
mirconesia <- c(9, 8, 148, 157, 212, 11, 10, 13, 12)
melanesia <- c(6, 7, 18, 17, 5)
polynesia <- c(155, 151, 153, 147, 162, 154, 156, 3, 146, 152, 19, 161)


## 160 = Hawaii, 163 = United Sates, 170 = Alaska, 135 = Mexico, 138 = Peru, 224 = Chile, 225 = Easter Island, 59 = Belgium,
## 71 = Bulgaria, 187 = Croatia, 81 = Cyprus, 141 = Faeroe Islands, 145 = Greenland, 175 = Denmark, 70 = Estonia,
## 174 = Finland, 179 = France, 176 = Germany, 80 = Greece, 181 = Ireland, 184 = Italy, 69 = Latvia, 189 = Lithuania,
## 68 = Malta, 177 = Netherlands, 178 = Poland, 55 = Azores, 57 = Madeira, 183 = Portugal, 72 = Romania, 188 = Slovenia,
## 58 = Canary Islands, 182 = Spain, 222 = Sweden, 180 = United Kingdom, 227 = Jersey, 228 = Guernsey, 209 = China,
## 205 = Myanmar, 210 = Japan, 216 = Indonesia, 15 = Philippines, 207 = Vietnam, 9 = Mirconesia, 8 = Palau, 148 = Line group,
## 157 = Phoenix grup, 212 = Kiribati, 11 = Marshall Islands, 10 = Nauru, 6 = Vanuatu, 7 = Solomon Islands, 18 = Fiji,
## 17 = Papau New Guinea, 5 = New Caledonia, 155 = Tonga, 151 = American Somoa, 153 = Cook Islands, 147 = French Polynesia, 
## 162 = New Zealand, 154 = Niue, 156 = Tokelau, 3 = Norfolk Island, 146 = Pitcairn, 152 = Samoa, 19 = Tuvalu, 161 = Wallis and Futuna,
## 13 = Northern Mariana Islands and Guam, 12 = Wake Island
  
 


