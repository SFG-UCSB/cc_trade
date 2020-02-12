

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(ggplot2)
library(ggforce)
library(countrycode)

# Directories
datadir <- "data/gaines"
plotdir <- "figures"

# Load data
load(file.path(datadir, "gaines_eez_msy_time_series.Rdata"))


# MSY total
################################################################################

# Global MSY over time
msy <- msy_ts_g %>% 
  group_by(rcp, year) %>% 
  summarize(msy_mt=sum(msy, na.rm=T))

# Plot global MSY over time
p <- ggplot(msy, aes(x=year, y=msy_mt/1e6, col=rcp)) +
  geom_line() +
  xlab("") + ylab("MSY (millions mt)") +
  theme_bw()
p


# Total MSY by sovereign
################################################################################

# Sum MSY by sovereign nation
msy_country_ts <- msy_ts %>%
  group_by(sovereign_iso3, sovereign, rcp, year) %>% 
  summarize(msy_mt=sum(msy_eez, na.rm=T))

# Plot global MSY over time
p <- ggplot(msy_country_ts, aes(x=year, y=msy_mt/1e6, fill=sovereign)) +
  geom_area() +
  facet_wrap(~rcp, ncol=2) +
  guides(fill=FALSE) +
  xlab("") + ylab("MSY (millions mt)") +
  theme_bw()
p

# Export
save(msy_country_ts, file=file.path(datadir, "gaines_msy_time_series_by_country.Rdata"))



# Territory-level results
################################################################################

# Periods
yrs1 <- 2012:2021
yrs2 <- 2091:2100

# Calculate percent difference
perc_diff <- function(val1, val2){
  (val2-val1)/val1
}

# Classify stocks
key_eez <- msy_ts %>%
  # filter(sovereign=="United States") %>% 
  group_by(rcp, sovereign_iso3, sovereign, country, eez_id, eez, species) %>% 
  summarize(nyrs_present=sum(range_prop>0),
            nyrs_present1=sum(range_prop[year%in%yrs1]>0),
            nyrs_present2=sum(range_prop[year%in%yrs2]>0),
            # Change in global range
            range1=mean(range_tot_km2[year%in%yrs1]),
            range2=mean(range_tot_km2[year%in%yrs2]),
            range_perc=perc_diff(range1, range2),
            # Change in EEZ range
            range_eez1=mean(range_eez_km2[year%in%yrs1]),
            range_eez2=mean(range_eez_km2[year%in%yrs2]),
            range_eez_perc=perc_diff(range_eez1, range_eez2),
            # Change in MSY
            msy_avg1=mean(msy_eez[year%in%yrs1]),
            msy_avg2=mean(msy_eez[year%in%yrs2]),
            msy_perc=perc_diff(msy_avg1, msy_avg2)) %>% 
  mutate(msy_change=ifelse(abs(msy_perc)<0.5, "MSY same",
                           ifelse(msy_perc<0, "MSY down", "MSY up")),
         range_change=ifelse(abs(range_perc)<0.5, "Total range same",
                           ifelse(range_perc<0, "Total range down", "Total range up")),
         eez_range_change=ifelse(abs(range_eez_perc)<0.5, "EEZ range same",
                             ifelse(range_eez_perc<0, "EEZ range down", "EEZ range up")),
         type_long=ifelse(nyrs_present==0, "never", paste(msy_change, eez_range_change, range_change, sep="-")),
         type=ifelse(nyrs_present==0, "never",
                     ifelse(nyrs_present1>=5 & nyrs_present2<5, "leaving", 
                            ifelse(nyrs_present1<5 & nyrs_present2>=5, "entering", 
                                  ifelse(eez_range_change=="EEZ range same" & range_change=="Total range same", "same",
                                         ifelse(eez_range_change=="EEZ range down" & range_change%in%c("Total range down", "Total range same"), "shrinking", 
                                                ifelse(eez_range_change=="EEZ range up" & range_change%in%c("Total range up", "Total range same"), "expanding", "other"))))))) %>% 
  ungroup()

# Perform checks
# table(key_eez$type)
# freeR::complete(key_eez)

# Split long strings into two lines
swr <-  Vectorize(function(string, nwrap=20){
  paste(strwrap(string, width=nwrap), collapse="\n")
})

# MSY time series by classication
msy_ts1 <- msy_ts %>%
  ungroup() %>% 
  # Filter temporarily
  # filter(sovereign=="United States") %>% 
  # Add classification
  left_join(select(key_eez, rcp, country, species, type, msy_change, range_change), by=c("rcp", "country", "species")) %>% 
  # Sum by type
  filter(type!="never") %>% 
  group_by(rcp, sovereign_iso3, sovereign, country, eez, eez_id, type, year) %>% 
  summarize(msy=sum(msy_eez, na.rm=T)/1000) %>% 
  ungroup() %>% 
  mutate(type=factor(type, levels=rev(c("other", "same", "expanding", "entering", "shrinking", "leaving"))),
         country_wrap=swr(country))

# Export
saveRDS(msy_ts1, file=file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))


# Plot
################################################################################

# ggplot2 multi-page PDF
# devtools::install_github("guiastrennec/ggplus")
library(ggplus)

# Plot all
p <- ggplot(filter(msy_ts1, rcp=="RCP85"), aes(x=year, y=msy, fill=type)) +
  geom_area() +
  scale_fill_manual(name="Stock type",
                    labels=rev(c("Other", "Stable", "Expanding", "Entering", "Shrinking", "Leaving")),
                    values = rev(c("grey30", "grey80", "blue", "navy", "red", "darkred"))) +
  # facet_wrap_paginate(~ country, nrow=4, ncol=4, scales="free") +
  xlab("") + ylab("MSY (1000s mt)") +
  theme_bw()

# Creat multi-page PDF
pdf(file.path(plotdir, "AppendixX_territory_level_msy_time_series.pdf"), width=8.5, height=11)
gg10 <- facet_multiple(plot=p, facets="country_wrap", ncol = 4, nrow = 5, scales="free")
dev.off()







# Country-level
################################################################################

msy_cntry <- msy_ts %>%
  group_by(rcp, sovereign_iso3, sovereign, species, year) %>% 


# Classify stocks
key_cntry <- msy_ts %>%
  # filter(sovereign=="United States") %>% 
  group_by(rcp, sovereign_iso3, sovereign, species) %>% 
  summarize(nyrs_present=sum(range_prop>0),
            nyrs_present1=sum(range_prop[year%in%yrs1]>0),
            nyrs_present2=sum(range_prop[year%in%yrs2]>0),
            # Change in global range
            range1=mean(range_tot_km2[year%in%yrs1]),
            range2=mean(range_tot_km2[year%in%yrs2]),
            range_perc=perc_diff(range1, range2),
            # Change in EEZ range
            range_eez1=mean(range_eez_km2[year%in%yrs1]),
            range_eez2=mean(range_eez_km2[year%in%yrs2]),
            range_eez_perc=perc_diff(range_eez1, range_eez2),
            # Change in MSY
            msy_avg1=mean(msy_eez[year%in%yrs1]),
            msy_avg2=mean(msy_eez[year%in%yrs2]),
            msy_perc=perc_diff(msy_avg1, msy_avg2)) %>% 
  mutate(msy_change=ifelse(abs(msy_perc)<0.5, "MSY same",
                           ifelse(msy_perc<0, "MSY down", "MSY up")),
         range_change=ifelse(abs(range_perc)<0.5, "Total range same",
                             ifelse(range_perc<0, "Total range down", "Total range up")),
         eez_range_change=ifelse(abs(range_eez_perc)<0.5, "EEZ range same",
                                 ifelse(range_eez_perc<0, "EEZ range down", "EEZ range up")),
         type_long=ifelse(nyrs_present==0, "never", paste(msy_change, eez_range_change, range_change, sep="-")),
         type=ifelse(nyrs_present==0, "never",
                     ifelse(nyrs_present1>=5 & nyrs_present2<5, "leaving", 
                            ifelse(nyrs_present1<5 & nyrs_present2>=5, "entering", 
                                   ifelse(eez_range_change=="EEZ range same" & range_change=="Total range same", "same",
                                          ifelse(eez_range_change=="EEZ range down" & range_change%in%c("Total range down", "Total range same"), "shrinking", 
                                                 ifelse(eez_range_change=="EEZ range up" & range_change%in%c("Total range up", "Total range same"), "expanding", "other"))))))) %>% 
  ungroup()

# Perform checks
# table(key_eez$type)
# freeR::complete(key_eez)

# Split long strings into two lines
swr <-  Vectorize(function(string, nwrap=20){
  paste(strwrap(string, width=nwrap), collapse="\n")
})

# MSY time series by classication
msy_ts1 <- msy_ts %>%
  ungroup() %>% 
  # Filter temporarily
  # filter(sovereign=="United States") %>% 
  # Add classification
  left_join(select(key_eez, rcp, country, species, type, msy_change, range_change), by=c("rcp", "country", "species")) %>% 
  # Sum by type
  filter(type!="never") %>% 
  group_by(rcp, sovereign_iso3, sovereign, country, eez, eez_id, type, year) %>% 
  summarize(msy=sum(msy_eez, na.rm=T)/1000) %>% 
  ungroup() %>% 
  mutate(type=factor(type, levels=rev(c("other", "same", "expanding", "entering", "shrinking", "leaving"))),
         country_wrap=swr(country))

# Export
saveRDS(msy_ts1, file=file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))




