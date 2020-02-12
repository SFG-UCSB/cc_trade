
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
library(RColorBrewer)

# Directories
datadir <- "data/gaines"

# Load data
data <- readRDS(file.path(datadir, "gaines_data_for_eez_analysis.Rds"))

# !!!!!!!!
# When you added functionality to specify end year, you did not re-run to 2100 to get new outputted file names


# Build data
################################################################################

# Year end
year_end <- 2100

# Time periods
yrs1 <- 2012:2021; length(yrs1)
yrs2 <- (year_end-9):year_end; length(yrs2)

# Function for percent difference
perc_diff <- function(x1, x2){
  out <- (x2-x1) / x1 * 100
}

# Global stats
#########################################

# Global percent difference in biomass, harvest, and profits in 2012 vs 2100
gdata <- data %>%
  # Reduce to only decades of interest
  filter(year %in% c(yrs1, yrs2)) %>% 
  # Calculate year totals
  ungroup() %>% 
  group_by(rcp, scenario, year) %>% 
  summarize(b=sum(biomass_eez, na.rm=T),
            c=sum(harvest_eez, na.rm=T),
            p=sum(profit_eez, na.rm=T),
            msy=sum(msy_eez, na.rm=T),
            bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_prop=sum(bbmsy>1 & !is.na(bbmsy)) / sum(!is.na(bbmsy))) %>% 
  # Calculate end point totals
  ungroup() %>% 
  group_by(rcp, scenario) %>% 
  summarize(b1=mean(b[year%in%yrs1], na.rm=T),
            b2=mean(b[year%in%yrs2], na.rm=T),
            c1=mean(c[year%in%yrs1], na.rm=T),
            c2=mean(c[year%in%yrs2], na.rm=T),
            p1=mean(p[year%in%yrs1], na.rm=T),
            p2=mean(p[year%in%yrs2], na.rm=T),
            msy_tot1=mean(msy[year%in%yrs1], na.rm=T),
            msy_tot2=mean(msy[year%in%yrs2], na.rm=T),
            bbmsy_avg1=mean(bbmsy_avg[year%in%yrs1], na.rm=T),
            bbmsy_avg2=mean(bbmsy_avg[year%in%yrs2], na.rm=T),
            bbmsy_prop1=mean(bbmsy_prop[year%in%yrs1], na.rm=T),
            bbmsy_prop2=mean(bbmsy_prop[year%in%yrs2], na.rm=T)) %>% 
  # Calculate totals relative to no adaptation
  ungroup() %>% 
  group_by(rcp) %>% 
  mutate(b_pdiff=perc_diff(b1[scenario=="No Adaptation"], b2),
         c_pdiff=perc_diff(c1[scenario=="No Adaptation"], c2),
         p_pdiff=perc_diff(p1[scenario=="No Adaptation"], p2),
         bbmsy_prop_diff=bbmsy_prop2-bbmsy_prop1[scenario=="No Adaptation"]) %>% 
  ungroup()


# Plot results
p <- ggplot(gdata, aes(x=c_pdiff, y=p_pdiff, label=scenario, col=bbmsy_avg2)) +
  geom_point(size=5) +
  geom_text_repel() +
  facet_wrap(~rcp, nrow=2) +
  xlab("% difference in catch\n(2100 versus today)") + 
  ylab("% difference in profits\n(2100 versus today)") + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(9, "Blues"), 
                       limits=c(0,1),
                       breaks=seq(0,1,0.2),
                       labels=seq(0,1,0.2),
                       name="Proportion of years\nwith B/BMSY > 1.0") +
  theme_bw()
p


# Export results
filename <- paste0("gaines_global_level_results_approach1_", year_end, ".Rds")
saveRDS(gdata, file.path(datadir, filename))


# Soveriegn country stats
#########################################

# Country-level percent difference in biomass, harvest, and profits in 2012 vs 2100
cdata <- data %>%
  # Reduce to only decades of interest
  filter(year %in% c(yrs1, yrs2)) %>% 
  # Calculate year totals
  group_by(sovereign_iso3, sovereign, rcp, scenario, year) %>% 
  summarize(b=sum(biomass_eez, na.rm=T),
            c=sum(harvest_eez, na.rm=T),
            p=sum(profit_eez, na.rm=T),
            msy=sum(msy_eez, na.rm=T),
            bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_prop=sum(bbmsy>1 & !is.na(bbmsy)) / sum(!is.na(bbmsy))) %>% 
  # Calculate end point totals
  group_by(sovereign_iso3, sovereign, rcp, scenario) %>% 
  summarize(b1=mean(b[year%in%yrs1], na.rm=T),
            b2=mean(b[year%in%yrs2], na.rm=T),
            c1=mean(c[year%in%yrs1], na.rm=T),
            c2=mean(c[year%in%yrs2], na.rm=T),
            p1=mean(p[year%in%yrs1], na.rm=T),
            p2=mean(p[year%in%yrs2], na.rm=T),
            msy_tot1=mean(msy[year%in%yrs1], na.rm=T),
            msy_tot2=mean(msy[year%in%yrs2], na.rm=T),
            bbmsy_avg1=mean(bbmsy_avg[year%in%yrs1], na.rm=T),
            bbmsy_avg2=mean(bbmsy_avg[year%in%yrs2], na.rm=T),
            bbmsy_prop1=mean(bbmsy_prop[year%in%yrs1], na.rm=T),
            bbmsy_prop2=mean(bbmsy_prop[year%in%yrs2], na.rm=T)) %>% 
  # Calculate totals relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, rcp) %>% 
  mutate(b_pdiff=perc_diff(b1[scenario=="No Adaptation"], b2),
         c_pdiff=perc_diff(c1[scenario=="No Adaptation"], c2),
         p_pdiff=perc_diff(p1[scenario=="No Adaptation"], p2),
         bbmsy_prop_diff=bbmsy_prop2-bbmsy_prop1[scenario=="No Adaptation"])

# Plot results
p <- ggplot(cdata, aes(x=c_pdiff, y=p_pdiff, fill=bbmsy_avg2, size=msy_tot1)) +
  geom_point(pch=21) +
  facet_grid(scenario ~ rcp) +
  xlab("% difference in catch\n(2100 versus today)") + 
  ylab("% difference in profits\n(2100 versus today)") + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(0,2),
                       breaks=seq(0,2,0.25),
                       labels=seq(0,2,0.25),
                       name="Mean B/BMSY in 2100") +
  theme_bw()
p

# Export results
filename <- paste0("gaines_country_level_results_approach1_", year_end, ".Rds")
saveRDS(cdata, file.path(datadir, filename))

# Territory stats
#########################################

# Territory-level percent difference in biomass, harvest, and profits in 2012 vs 2100
tdata <- data %>%
  # Reduce to only decades of interest
  filter(year %in% c(yrs1, yrs2)) %>% 
  # Calculate year totals
  group_by(sovereign_iso3, sovereign, country, rcp, scenario, year) %>% 
  summarize(b=sum(biomass_eez, na.rm=T),
            c=sum(harvest_eez, na.rm=T),
            p=sum(profit_eez, na.rm=T),
            msy=sum(msy_eez, na.rm=T),
            bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_prop=sum(bbmsy>1 & !is.na(bbmsy)) / sum(!is.na(bbmsy))) %>% 
  # Calculate end point totals
  group_by(sovereign_iso3, sovereign, country, rcp, scenario) %>% 
  summarize(b1=mean(b[year%in%yrs1], na.rm=T),
            b2=mean(b[year%in%yrs2], na.rm=T),
            c1=mean(c[year%in%yrs1], na.rm=T),
            c2=mean(c[year%in%yrs2], na.rm=T),
            p1=mean(p[year%in%yrs1], na.rm=T),
            p2=mean(p[year%in%yrs2], na.rm=T),
            msy_tot1=mean(msy[year%in%yrs1], na.rm=T),
            msy_tot2=mean(msy[year%in%yrs2], na.rm=T),
            bbmsy_avg1=mean(bbmsy_avg[year%in%yrs1], na.rm=T),
            bbmsy_avg2=mean(bbmsy_avg[year%in%yrs2], na.rm=T),
            bbmsy_prop1=mean(bbmsy_prop[year%in%yrs1], na.rm=T),
            bbmsy_prop2=mean(bbmsy_prop[year%in%yrs2], na.rm=T)) %>% 
  # Calculate totals relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, country, rcp) %>% 
  mutate(b_pdiff=perc_diff(b1[scenario=="No Adaptation"], b2),
         c_pdiff=perc_diff(c1[scenario=="No Adaptation"], c2),
         p_pdiff=perc_diff(p1[scenario=="No Adaptation"], p2),
         bbmsy_prop_diff=bbmsy_prop2-bbmsy_prop1[scenario=="No Adaptation"])

# Plot results
p <- ggplot(tdata, aes(x=c_pdiff, y=p_pdiff, fill=bbmsy_avg2, size=msy_tot1)) +
  geom_point(pch=21) +
  facet_grid(scenario ~ rcp) +
  xlab("% difference in catch\n(2100 versus today)") + 
  ylab("% difference in profits\n(2100 versus today)") + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(0,2),
                       breaks=seq(0,2,0.25),
                       labels=seq(0,2,0.25),
                       name="Mean B/BMSY in 2100") +
  theme_bw()
p

# Export results
filename <- paste0("gaines_territory_level_results_approach1_", year_end, ".Rds")
saveRDS(tdata, file.path(datadir, filename))

# EEZ stats
#########################################

# EEZ-level percent difference in biomass, harvest, and profits in 2012 vs 2100
edata <- data %>%
  # Reduce to only decades of interest
  filter(year %in% c(yrs1, yrs2)) %>% 
  # Calculate year totals
  group_by(sovereign_iso3, sovereign, country, eez, eez_id, rcp, scenario, year) %>% 
  summarize(b=sum(biomass_eez, na.rm=T),
            c=sum(harvest_eez, na.rm=T),
            p=sum(profit_eez, na.rm=T),
            msy=sum(msy_eez, na.rm=T),
            bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_prop=sum(bbmsy>1 & !is.na(bbmsy)) / sum(!is.na(bbmsy))) %>% 
  # Calculate end point totals
  group_by(sovereign_iso3, sovereign, country, eez, eez_id, rcp, scenario) %>% 
  summarize(b1=mean(b[year%in%yrs1], na.rm=T),
            b2=mean(b[year%in%yrs2], na.rm=T),
            c1=mean(c[year%in%yrs1], na.rm=T),
            c2=mean(c[year%in%yrs2], na.rm=T),
            p1=mean(p[year%in%yrs1], na.rm=T),
            p2=mean(p[year%in%yrs2], na.rm=T),
            msy_tot1=mean(msy[year%in%yrs1], na.rm=T),
            msy_tot2=mean(msy[year%in%yrs2], na.rm=T),
            bbmsy_avg1=mean(bbmsy_avg[year%in%yrs1], na.rm=T),
            bbmsy_avg2=mean(bbmsy_avg[year%in%yrs2], na.rm=T),
            bbmsy_prop1=mean(bbmsy_prop[year%in%yrs1], na.rm=T),
            bbmsy_prop2=mean(bbmsy_prop[year%in%yrs2], na.rm=T)) %>% 
  # Calculate totals relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, country, eez, eez_id, rcp) %>% 
  mutate(b_pdiff=perc_diff(b1[scenario=="No Adaptation"], b2),
         c_pdiff=perc_diff(c1[scenario=="No Adaptation"], c2),
         p_pdiff=perc_diff(p1[scenario=="No Adaptation"], p2),
         bbmsy_prop_diff=bbmsy_prop2-bbmsy_prop1[scenario=="No Adaptation"])

# Plot results
p <- ggplot(edata, aes(x=c_pdiff, y=p_pdiff, fill=bbmsy_avg2, size=msy_tot1)) +
  geom_point(pch=21) +
  facet_grid(scenario ~ rcp) +
  xlab("% difference in catch\n(2100 versus today)") + 
  ylab("% difference in profits\n(2100 versus today)") + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  xlim(-100,300) +
  ylim(-100,300) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(0,2),
                       breaks=seq(0,2,0.25),
                       labels=seq(0,2,0.25),
                       name="Mean B/BMSY in 2100") +
  theme_bw()
p

# Export results
filename <- paste0("gaines_eez_level_results_approach1_", year_end, ".Rds")
saveRDS(edata, file.path(datadir, filename))


# Country-stock stats
#########################################

# Country-species-level percent difference in biomass, harvest, and profits in 2012 vs 2100
csdata <- data %>%
  # Reduce to only decades of interest
  filter(year %in% c(yrs1, yrs2)) %>% 
  # Calculate year totals
  group_by(sovereign_iso3, sovereign, rcp, scenario, species, year) %>% 
  summarize(b=sum(biomass_eez, na.rm=T),
            c=sum(harvest_eez, na.rm=T),
            p=sum(profit_eez, na.rm=T),
            msy=sum(msy_eez, na.rm=T),
            bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_prop=sum(bbmsy>1 & !is.na(bbmsy)) / sum(!is.na(bbmsy))) %>% 
  # Calculate end point totals
  group_by(sovereign_iso3, sovereign, rcp, scenario, species) %>% 
  summarize(b1=mean(b[year%in%yrs1], na.rm=T),
            b2=mean(b[year%in%yrs2], na.rm=T),
            c1=mean(c[year%in%yrs1], na.rm=T),
            c2=mean(c[year%in%yrs2], na.rm=T),
            p1=mean(p[year%in%yrs1], na.rm=T),
            p2=mean(p[year%in%yrs2], na.rm=T),
            msy_tot1=mean(msy[year%in%yrs1], na.rm=T),
            msy_tot2=mean(msy[year%in%yrs2], na.rm=T),
            bbmsy_avg1=mean(bbmsy_avg[year%in%yrs1], na.rm=T),
            bbmsy_avg2=mean(bbmsy_avg[year%in%yrs2], na.rm=T),
            bbmsy_prop1=mean(bbmsy_prop[year%in%yrs1], na.rm=T),
            bbmsy_prop2=mean(bbmsy_prop[year%in%yrs2], na.rm=T)) %>% 
  # Calculate totals relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, rcp, species) %>% 
  mutate(b_pdiff=perc_diff(b1[scenario=="No Adaptation"], b2),
         c_pdiff=perc_diff(c1[scenario=="No Adaptation"], c2),
         p_pdiff=perc_diff(p1[scenario=="No Adaptation"], p2),
         bbmsy_prop_diff=bbmsy_prop2-bbmsy_prop1[scenario=="No Adaptation"])

# Export results
filename <- paste0("gaines_country_stock_level_results_approach1_", year_end, ".Rds")
saveRDS(csdata, file.path(datadir, filename))

# Territory-stock stats
#########################################

# Territory-species-level percent difference in biomass, harvest, and profits in 2012 vs 2100
tsdata <- data %>%
  # Reduce to only decades of interest
  filter(year %in% c(yrs1, yrs2)) %>% 
  # Calculate year totals
  group_by(sovereign_iso3, sovereign, country, rcp, scenario, species, year) %>% 
  summarize(b=sum(biomass_eez, na.rm=T),
            c=sum(harvest_eez, na.rm=T),
            p=sum(profit_eez, na.rm=T),
            msy=sum(msy_eez, na.rm=T),
            bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_prop=sum(bbmsy>1 & !is.na(bbmsy)) / sum(!is.na(bbmsy))) %>% 
  # Calculate end point totals
  group_by(sovereign_iso3, sovereign, country, rcp, scenario, species) %>% 
  summarize(b1=mean(b[year%in%yrs1], na.rm=T),
            b2=mean(b[year%in%yrs2], na.rm=T),
            c1=mean(c[year%in%yrs1], na.rm=T),
            c2=mean(c[year%in%yrs2], na.rm=T),
            p1=mean(p[year%in%yrs1], na.rm=T),
            p2=mean(p[year%in%yrs2], na.rm=T),
            msy_tot1=mean(msy[year%in%yrs1], na.rm=T),
            msy_tot2=mean(msy[year%in%yrs2], na.rm=T),
            bbmsy_avg1=mean(bbmsy_avg[year%in%yrs1], na.rm=T),
            bbmsy_avg2=mean(bbmsy_avg[year%in%yrs2], na.rm=T),
            bbmsy_prop1=mean(bbmsy_prop[year%in%yrs1], na.rm=T),
            bbmsy_prop2=mean(bbmsy_prop[year%in%yrs2], na.rm=T)) %>% 
  # Calculate totals relative to no adaptation
  ungroup() %>% 
  group_by(sovereign_iso3, sovereign, country, rcp, species) %>% 
  mutate(b_pdiff=perc_diff(b1[scenario=="No Adaptation"], b2),
         c_pdiff=perc_diff(c1[scenario=="No Adaptation"], c2),
         p_pdiff=perc_diff(p1[scenario=="No Adaptation"], p2),
         bbmsy_prop_diff=bbmsy_prop2-bbmsy_prop1[scenario=="No Adaptation"])

# Export results
filename <- paste0("gaines_territory_stock_level_results_approach1_", year_end, ".Rds")
saveRDS(tsdata, file.path(datadir, filename))


