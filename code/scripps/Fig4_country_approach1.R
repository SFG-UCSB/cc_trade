

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
data <- readRDS(file.path(datadir, "gaines_territory_level_results_approach1.Rds"))


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

# One RCP
data1 <- data %>% 
  filter(rcp=="RCP 2.6" & scenario=="Full Adaptation") %>% 
  mutate(c_pdiff=ifelse(abs(c_pdiff)>300, sign(c_pdiff)*300, c_pdiff),
         p_pdiff=ifelse(abs(p_pdiff)>300, sign(p_pdiff)*300, p_pdiff), 
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff))
  

# Plot data
g <- ggplot(data1, aes(x=c_pdiff, y=p_pdiff, size=msy_tot1/1e6, fill=msy_pdiff)) +
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  geom_point(pch=21) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       labels=seq(-100,100,25),
                       name="% change in MSY\n(2100 relative to today)") +
  scale_size_continuous(name="MSY (millions mt)") +
  theme_bw() + my_theme
g

# Export
ggsave(file.path(plotdir, "Fig4_country_approach1_one_rcp.png"), width=6.5, height=4, units="in", dpi=600)


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

# Format
data2 <- data %>% 
  filter(scenario=="Full Adaptation") %>% 
  mutate(c_pdiff=ifelse(abs(c_pdiff)>300, sign(c_pdiff)*300, c_pdiff),
         p_pdiff=ifelse(abs(p_pdiff)>300, sign(p_pdiff)*300, p_pdiff), 
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff))

# Plot data
g <- ggplot(data2, aes(x=c_pdiff, y=p_pdiff, size=msy_tot1/1e6, fill=msy_pdiff)) +
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  geom_point(pch=21) +
  facet_wrap(~ rcp, ncol=2) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       labels=seq(-100,100,25),
                       name="% change in MSY\n(2100 relative to today)") +
  scale_size_continuous(name="MSY (millions mt)") +
  theme_bw() + my_theme
g

# Export
ggsave(file.path(plotdir, "Fig5_country_approach1_all_rcps.png"), width=6.5, height=4, units="in", dpi=600)


