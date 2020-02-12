

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(ggplot2)
library(countrycode)
library(RColorBrewer)

# Directories
datadir <- "data/gaines"
plotdir <- "figures/paper"
eezdir <- "data/eezs/processed"

# Load data
tmsy <- readRDS(file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))


# Build data
################################################################################

# Calculate percent change in mean MSY from beginning to end
yrs1 <- 2012:2021; length(yrs1)
yrs2 <- 2091:2100; length(yrs2)
data <- tmsy %>% 
  ungroup() %>% 
  # Sum MSY by EEZ and year
  group_by(rcp, sovereign, sovereign_iso3, year) %>%
  summarize(msy=sum(msy)) %>% 
  # Average over periods
  group_by(rcp, sovereign, sovereign_iso3) %>% 
  summarize(msy_avg1=mean(msy[year%in%yrs1]),
            msy_avg2=mean(msy[year%in%yrs2]),
            msy_perc=(msy_avg2-msy_avg1)/msy_avg1*100,
            msy_perc=pmin(300, msy_perc)) %>% 
  # Remove outrageous values
  filter(msy_perc <= 100 & msy_perc>=-100)

# Plot distribution of MSY change percentages
hist(data$msy_perc, breaks=seq(-100,100,10))

# In RCP 4.5 (least severe), how many countries experience dramatic losses?
stat1 <- filter(data, rcp=="RCP45" & msy_perc <=-50 & !sovereign_iso3 %in% c("Disputed", "Joint"))
stat2 <- filter(data, rcp=="RCP85" & msy_perc <=-50 & !sovereign_iso3 %in% c("Disputed", "Joint"))

# How many countries experience wins across all scenarios?
stat3 <- data %>% 
  filter(rcp!="RCP26" & !sovereign_iso3 %in% c("Disputed", "Joint")) %>% 
  group_by(sovereign) %>% 
  summarize(npos=sum(msy_perc>0),
            msy_perc_avg=mean(msy_perc, na.rm=T)) %>% 
  arrange(desc(npos))

sum(stat3$npos==3)

# EEZ level stats
sov_interest <- c("Norway", "Antarctica", "Fiji", "Portugal", "Finland")
edata <- tmsy %>% 
  ungroup() %>% 
  # Sum MSY by EEZ and year
  group_by(rcp, sovereign, sovereign_iso3, country, eez, eez_id, year) %>%
  summarize(msy=sum(msy)) %>% 
  # Average over periods
  group_by(rcp, sovereign, sovereign_iso3, country, eez, eez_id) %>% 
  summarize(msy_avg1=mean(msy[year%in%yrs1]),
            msy_avg2=mean(msy[year%in%yrs2]),
            msy_perc=(msy_avg2-msy_avg1)/msy_avg1*100,
            msy_perc=pmin(300, msy_perc)) %>% 
  # Remove outrageous values
  filter(msy_perc <= 100 & msy_perc>=-100) %>% 
  # Filter to countries of interest
  filter(sovereign %in% sov_interest) %>% 
  arrange(sovereign)

