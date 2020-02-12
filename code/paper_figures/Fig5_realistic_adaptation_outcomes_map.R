

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
datadir <- "imperfect/data/"
plotdir <- "figures/paper"
eezdir <- "data/eezs/processed"

# Load data
data_orig <- readRDS(file.path(datadir, "gaines_country_level_results_approach1_imperfect.Rds"))

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf") %>% 
  mutate(sovereign_iso3=countrycode(sovereignt, "country.name", "iso3c"))


# Build data
################################################################################

# Create data
my_iso <- sort(unique(data_orig$sovereign_iso3))
world_isos <- sort(unique(world$sovereign_iso3))
extra_world_isos <- world_isos[!world_isos%in%my_iso]

# Extra
edata <- expand.grid(rcp=c("RCP 4.5", "RCP 6.0", "RCP 8.5"), sovereign_iso3=extra_world_isos, outcome=c("Catch", "Profits")) %>% 
  mutate(perc=NA)

# Build data
data <- data_orig %>% 
  ungroup() %>% 
  # Subset relevant scenarios
  filter(scenario=="Imperfect Full Adaptation 5 yr" & rcp!="RCP26") %>% 
  # Format scenario labels
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5")) %>% 
  # Add percent change in MSY and cap percent differences
  mutate(c_pdiff=ifelse(abs(c_pdiff)>300, sign(c_pdiff)*300, c_pdiff),
         # Cap percent difference in profits [-100%, 300%]
         p_pdiff=ifelse(p_pdiff < -100, -100, p_pdiff), 
         p_pdiff=ifelse(p_pdiff > 300, 300, p_pdiff),
         # Calculate percent difference in MSY: cap at [-100%, 100%]
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff)) %>% 
  # Select columns and reshape
  select(rcp, sovereign_iso3, p_pdiff, c_pdiff) %>% 
  gather(key="outcome", value="perc", 3:4) %>% 
  mutate(outcome=recode(outcome, "p_pdiff"="Profits", "c_pdiff"="Catch")) %>% 
  # Add extra isos
  rbind(edata)


# Add data to EEZ shapefile
world1 <- world %>%
  left_join(data, by=c("sovereign_iso3"="sovereign_iso3")) %>% 
  filter(!is.na(outcome) & !is.na(rcp))



# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10), 
                  strip.text=element_text(size=10),
                  plot.title=element_text(size=10),
                  legend.position = "bottom",
                  # panel.grid.major = element_blank(), 
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot all
g <- ggplot(world1) +
  geom_sf(aes(fill=perc), lwd=0.05, col="grey30") +
  geom_hline(aes(yintercept = 0), linetype="dotted", color="grey30", size=0.1) +
  facet_grid(rcp ~ outcome) +
  # scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
  #                      limits=c(-100,300),
  #                      breaks=seq(-100,300,50),
  #                      name="Percent change\n(2100 relative to today)") +
  scale_fill_gradient2(low=brewer.pal(9, "RdBu")[1],
                       high=brewer.pal(9, "RdBu")[9],
                       mid=brewer.pal(9, "RdBu")[5],
                       midpoint=0,
                       limits=c(-100,250),
                       name="Percent change\n(2100 relative to today)", na.value="grey80") +
  ylab("") +
  theme_bw() + my_theme
g

# Export
ggsave(plot=g, file.path(plotdir, "Fig5_realistic_adaptation_outcomes_map.tiff"), width=6.5, height=5.5, units="in", dpi=600)

