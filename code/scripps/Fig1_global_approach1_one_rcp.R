

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
datadir <- "rshiny/data" 
plotdir <- "figures/scripps"

# Load data
gdata1 <- readRDS(file.path(datadir, "gaines_global_level_results_approach1.Rds"))


# Parameters
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=12),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12), 
                  strip.text=element_text(size=12),
                  plot.title=element_text(size=14),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Legend titles
bbmsy_avg_title1 <- "Mean B/BMSY\nof stocks in 2100"
bbmsy_prop_title1 <- "Proportion of stocks\nwith B/BMSY > 1.0 in 2100"


# One RCP
################################################################################

gdata1_sub <- filter(gdata1, rcp=="RCP 2.6")

# Plot data
g <- ggplot(gdata1_sub, aes(x=c_pdiff, y=p_pdiff, fill=bbmsy_avg2, size=bbmsy_prop2, label=scenario_short)) +
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  geom_point(pch=21) +
  geom_point(aes(x=0, y=0), fill="grey90", pch=21, size=5) +
  geom_text_repel(col="black", size=4, point.padding = 1) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  scale_size_continuous(name=bbmsy_prop_title1) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(0,2),
                       breaks=seq(0,2,0.25),
                       labels=seq(0,2,0.25),
                       name=bbmsy_avg_title1) +
  theme_bw() + my_theme
g

# Export
ggsave(file.path(plotdir, "Fig1_global_approach1_one_rcp.png"), width=6.5, height=4, units="in", dpi=600)


# All RCPs
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
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(gdata1, aes(x=c_pdiff, y=p_pdiff, fill=bbmsy_avg2, size=bbmsy_prop2, label=scenario_short)) +
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  geom_point(pch=21) +
  geom_point(aes(x=0, y=0), fill="grey90", pch=21, size=5) +
  facet_wrap(~rcp, ncol=2) +
  geom_text_repel(col="black", size=3, point.padding = 0.6) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  scale_size_continuous(name=bbmsy_prop_title1) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(0,2),
                       breaks=seq(0,2,0.25),
                       labels=seq(0,2,0.25),
                       name=bbmsy_avg_title1) +
  theme_bw() + my_theme
g

# Export
ggsave(file.path(plotdir, "Fig2_global_approach1_all_rcps.png"), width=6.5, height=4, units="in", dpi=600)


