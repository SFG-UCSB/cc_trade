

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(ggplot2)
library(countrycode)
library(maptools)
library(FAOSTAT)

# Directories
datadir <- "data/gaines"
faodir <- "data/fao"
plotdir <- "figures"

# Load data
data_orig <- readRDS(file.path(datadir, "gaines_data_for_eez_analysis.Rds"))
fao_orig <- read.csv(file.path(faodir, "1950_2016_fao_global_capture_production_formatted.csv"), as.is=T)

# Build data
################################################################################

# Format FAO data
fao <- fao_orig %>% 
  filter(type=="marine" & units=="tonnes") %>% 
  group_by(country, year) %>% 
  summarize(catch=sum(catch, na.rm=T)) %>% 
  mutate(iso3=countrycode(country, "country.name", "iso3c")) %>% 
  filter(year==2012)

# Sample size
data <- data_orig %>% 
  filter(rcp=="RCP26" & scenario=="No Adaptation" & year==2012 & range_prop>0) %>% 
  group_by(sovereign_iso3, sovereign) %>% 
  summarize(n_spp=n_distinct(species),
            c_tot=sum(harvest_eez, na.rm=T)) %>% 
  left_join(fao, by=c("sovereign_iso3"="iso3")) %>% 
  mutate(c_perc=c_tot/catch,
         c_perc=ifelse(is.infinite(c_perc), NA, c_perc),
         c_perc=ifelse(c_perc>1, 1, c_perc))

# Ranges
range(data$n_spp)
hist(data$n_spp)
range(data$c_perc, na.rm=T)
hist(data$c_perc)

# Add data to world map
# Build spatial sample size data
data(wrld_simpl) # from maptools
world_orig <- wrld_simpl@data
world <- world_orig %>% 
  mutate(ISO3=as.character(ISO3)) %>% 
  left_join(data, by=c("ISO3"="sovereign_iso3")) %>% 
  mutate(nspp_bin=cut(n_spp, breaks=seq(0,650,50)),
         nspp_col=brewer.pal(9, "Blues")[nspp_bin],
         nspp_col=ifelse(is.na(nspp_col), "grey80", nspp_col))
wrld_simpl@data <- world


# Plot data
################################################################################

# Setup figure
figname <- "FigS1_country_sample_size.png"
png(file.path(plotdir, figname), width=4.5, height=4.5, units="in", res=600)
par(mfrow=c(2,1), mar=c(0,0,0.4,0), oma=c(0,0,0.3,0), mgp=c(2,0.8,0))

# Plot data
plot(wrld_simpl, col=wrld_simpl$nspp_col, border="grey30", lwd=0.3)
title("Number of stocks", line=0, xpd=NA, cex.main=0.9)

# Plot data
plot(wrld_simpl, col=wrld_simpl$nspp_col, border="grey30", lwd=0.3)
title("Percentage of catch", line=0, xpd=NA, cex.main=0.9)

# Off
dev.off()

