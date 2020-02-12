

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(ggplot2)
library(rworldmap)
library(countrycode)
library(maptools)
library(sf)
library(rnaturalearth)


# Directories
datadir <- "data/gaines"
plotdir <- "figures/scripps"
eezdir <- "data/eezs/processed"

# Load data
tmsy <- readRDS(file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))

# Load EEZ shapefile
eezs <- readRDS(file.path(eezdir, "world_eezs_v8.Rds"))

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")


# Build data
################################################################################

# Calculate percent change in mean MSY from beginning to end
yrs1 <- 2012:2021; length(yrs1)
yrs2 <- 2091:2100; length(yrs2)
data <- tmsy %>% 
  ungroup() %>% 
  # Sum MSY by EEZ and year
  group_by(rcp, sovereign, sovereign_iso3, country, eez_id, eez, year) %>%
  summarize(msy=sum(msy)) %>% 
  # Average over periods
  group_by(rcp, sovereign, sovereign_iso3, country, eez_id, eez) %>% 
  summarize(msy_avg1=mean(msy[year%in%yrs1]),
            msy_avg2=mean(msy[year%in%yrs2]),
            msy_perc=(msy_avg2-msy_avg1)/msy_avg1*100,
            msy_perc=pmin(300, msy_perc)) %>% 
  # Remove outrageous values
  filter(msy_perc <= 100 & msy_perc>=-100)

# Plot distribution of MSY change percentages
hist(data$msy_perc, breaks=seq(-100,100,10))

# Add data to EEZ shapefile
eezs1 <- eezs %>%
  left_join(data, by="eez") %>% 
  filter(!is.na(rcp)) %>% 
  mutate(rcp=plyr::revalue(rcp, c("RCP26"="RCP 2.6",
                             "RCP45"="RCP 4.5",
                             "RCP60"="RCP 6.0",
                             "RCP85"="RCP 8.5")))



# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10), 
                  strip.text=element_text(size=10),
                  legend.position = "bottom",
                  # panel.grid.major = element_blank(), 
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Subset one
sdata <- filter(eezs1, rcp=="RCP 2.6")

# Plot one
g <- ggplot(sdata) +
  geom_sf(aes(fill=msy_perc), lwd=0.05, col="white") +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       name="Percent change in MSY\n(2100 relative to today") +
  theme_bw() + my_theme
g


# Export
ggsave(file.path(plotdir, "Fig3_msy_perc_change_map_one.png"), width=6.5, height=4, units="in", dpi=600)



# Plot all
################################################################################

# Plot all
g <- ggplot(eezs1) +
  geom_sf(aes(fill=msy_perc), lwd=0.05, col="white") +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  geom_hline(yintercept = 0, linetype="dotted", color="grey30", size=0.1) +
  facet_wrap(~ rcp, nrow=2) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,50),
                       name="Percent change in MSY\n(2100 relative to today)") +
  theme_bw() + my_theme

# Export
ggsave(plot=g, file.path(plotdir, "Fig3_msy_perc_change_map_all.png"), width=6.5, height=4.5, units="in", dpi=600)

