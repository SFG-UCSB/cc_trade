
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)

# Directories
datadir <- "data"
plotdir <- "figures/edf_targets"
codedir <- "code"

# Read projection function
source(file.path(codedir, "proj_fish.R"))

# Read EDF target geographies
edf_key <- read.csv(file.path(datadir, "EDF_target_countries.csv"), as.is=T)


# Loop through target geographies
################################################################################

# EDF target geographies
geos <- sort(unique(edf_key$group))

# Loop through geographies and project outcomes
for(i in 1:length(geos)){
  
  # Get ISOs
  geo <- geos[i]
  isos <- edf_key$iso3[edf_key$group==geo]
  
  # Plot figure
  proj_fish(iso3s=isos, ref_year=2100, plotdir=plotdir, figname=geo)
  
}

