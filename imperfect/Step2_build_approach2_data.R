
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(countrycode)

# Directories
datadir <- "imperfect/data"

# Load data
final <- readRDS(file.path(datadir, "gaines_data_for_eez_analysis_imperfect.Rds"))


# Build data
################################################################################

# Function for percent difference
perc_diff <- function(x1, x2){
  out <- (x2-x1) / x1 * 100
}


# Global stats
#########################################

# # Global cumulative catch, profits, and p(above B/BMSY)
# gdata <- data %>% 
#   group_by(rcp, scenario) %>% 
#   summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
#             msy_tot2=sum(msy_eez[year==2100], na.rm=T),
#             c_tot=sum(harvest_eez, na.rm=T),
#             p_tot=sum(profit_eez, na.rm=T),
#             bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
#             bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
#   # Calculate cumulative catch and profits relative to no adaptation
#   ungroup() %>% 
#   group_by(rcp) %>% 
#   mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
#          p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
#          b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"]) %>% 
#   ungroup()
# 
# # Plot results
# p <- ggplot(gdata, aes(x=c_tot_rel, y=p_tot_rel, label=scenario, col=bbmsy_avg2)) +
#   geom_point(size=5) +
#   geom_text_repel() +
#   facet_wrap(~rcp, nrow=2) +
#   xlab("% difference in cumulative catch\n(relative to no adaptation)") + 
#   ylab("% difference in cumulative profits\n(relative to no adaptation)") + 
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=0) +
#   scale_color_gradientn(colours=brewer.pal(9, "RdBu"), 
#                         limits=c(0,2),
#                         breaks=seq(0,2,0.25),
#                         labels=seq(0,2,0.25),
#                         name="Mean B/BMSY") +
#   theme_bw()
# p
# 
# # Export results
# saveRDS(gdata, file.path(datadir, "gaines_global_level_results_approach2.Rds"))


# Country stats
#########################################

# Country-level cumulative catch, profits, and p(above B/BMSY)
cdata <- final %>% 
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
         b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])%>% 
  ungroup()

# Export data
saveRDS(cdata, file.path(datadir, "gaines_country_level_results_approach2_imperfect.Rds"))


# # Territory stats
# #########################################
# 
# # Territory-level cumulative catch, profits, and p(above B/BMSY)
# tdata <- data %>% 
#   group_by(sovereign_iso3, sovereign, country, rcp, scenario) %>% 
#   summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
#             msy_tot2=sum(msy_eez[year==2100], na.rm=T),
#             c_tot=sum(harvest_eez, na.rm=T),
#             p_tot=sum(profit_eez, na.rm=T),
#             bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
#             bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
#   # Calculate cumulative catch and profits relative to no adaptation
#   ungroup() %>% 
#   group_by(sovereign_iso3, sovereign, country, rcp) %>% 
#   mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
#          p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
#          b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])%>% 
#   ungroup()
# 
# # Export results
# saveRDS(tdata, file.path(datadir, "gaines_territory_level_results_approach2.Rds"))
# 
# 
# # Country-stock stats
# #########################################
# 
# # Country-species-level cumulative catch, profits, and p(above B/BMSY)
# csdata <- data %>% 
#   group_by(sovereign_iso3, sovereign, rcp, scenario, species) %>% 
#   summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
#             msy_tot2=sum(msy_eez[year==2100], na.rm=T),
#             c_tot=sum(harvest_eez, na.rm=T),
#             p_tot=sum(profit_eez, na.rm=T),
#             bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
#             bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
#   # Calculate cumulative catch and profits relative to no adaptation
#   ungroup() %>% 
#   group_by(sovereign_iso3, sovereign, rcp, species) %>% 
#   mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
#          p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
#          b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])%>% 
#   ungroup()
# 
# # Export data
# saveRDS(csdata, file.path(datadir, "gaines_country_stock_level_results_approach2.Rds"))
# 
# 
# # Territory-stock stats
# #########################################
# 
# # Country-species-level cumulative catch, profits, and p(above B/BMSY)
# tsdata <- data %>% 
#   group_by(sovereign_iso3, sovereign, country, rcp, scenario, species) %>% 
#   summarize(msy_tot1=sum(msy_eez[year==2012], na.rm=T),
#             msy_tot2=sum(msy_eez[year==2100], na.rm=T),
#             c_tot=sum(harvest_eez, na.rm=T),
#             p_tot=sum(profit_eez, na.rm=T),
#             bbmsy_avg2=mean(bbmsy[year==2100], na.rm=T),
#             bbmsy_good_prop=sum(!is.na(bbmsy)&bbmsy>=1)/sum(!is.na(bbmsy))) %>% 
#   # Calculate cumulative catch and profits relative to no adaptation
#   ungroup() %>% 
#   group_by(sovereign_iso3, sovereign, country, rcp, species) %>% 
#   mutate(c_tot_rel=perc_diff(c_tot[scenario=="No Adaptation"], c_tot),
#          p_tot_rel=perc_diff(p_tot[scenario=="No Adaptation"], p_tot),
#          b_prop_diff=bbmsy_good_prop-bbmsy_good_prop[scenario=="No Adaptation"])%>% 
#   ungroup()
# 
# # Export results
# saveRDS(tsdata, file.path(datadir, "gaines_territory_stock_level_results_approach2.Rds"))
# 
