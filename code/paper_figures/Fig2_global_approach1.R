
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)

# Read data
datadir1 <- "data/gaines"
datadir2 <- "imperfect/data"
plotdir <- "figures/paper"

# Load data
gdata1 <- readRDS(file.path(datadir1, "gaines_global_level_results_approach1.Rds"))
gdata2 <- readRDS(file.path(datadir2, "gaines_global_level_results_approach1_imperfect.Rds"))


# Build data
################################################################################

# Subset data
gdata1_format <- gdata1 %>% 
  ungroup() %>% 
  filter(!grepl("Imperfect", scenario))
gdata2_format <- gdata2 %>% 
  ungroup %>% 
  filter(grepl("Imperfect", scenario) & scenario!="Imperfect Full Adaptation 15 yr")

# Combine and format data
data <- rbind(gdata1_format, gdata2_format) %>% 
  filter(rcp!="RCP26") %>% 
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5"),
         scenario=recode(scenario,
                         "No Adaptation"="BAU",
                         "Full Adaptation"="Full",
                         "Imperfect Full Adaptation 5 yr"="Realistic (5 yr)",
                         "Imperfect Full Adaptation 10 yr"="Realistic (10 yr)",
                         "Imperfect Full Adaptation 20 yr"="Realistic (20 yr)"))


# Plot figure
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10), 
                  strip.text=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position = "bottom")

# Plot data
g <- ggplot(data, aes(x=c_pdiff, y=p_pdiff, color=bbmsy_prop2, label=scenario)) +
  facet_wrap(~rcp, nrow=1) +
  # Lines
  geom_hline(yintercept=0, col="grey50", size=0.3) +
  geom_vline(xintercept=0, col="grey50", size=0.3) +
  # Points
  geom_point(pch=16, size=4) +
  # Labels
  geom_text_repel(col="black", size=2, point.padding = 1, segment.size=0.4) +
  # Axis titles
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  # Legends
  scale_color_gradientn(name="Proportion of stocks\nabove BMSY in 2100",
                        colours=brewer.pal(9, "RdBu"), 
                        limits=c(0,1),
                        breaks=seq(0,1,0.25),
                        labels=seq(0,1,0.25)) +
  theme_bw() + my_theme
g

# Export
ggsave(plot=g, file.path(plotdir, "Fig2_global_adaptation_approach1.tiff"), width=6.5, height=3.5, units="in", dpi=600)
