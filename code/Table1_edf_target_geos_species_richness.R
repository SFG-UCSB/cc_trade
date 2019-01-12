
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(countrycode)

# Directories
datadir <- "data"
plotdir <- "figures/edf_targets"
codedir <- "code"
tabledir <- "tables"

# Read projection function
source(file.path(codedir, "proj_fish.R"))

# Read EDF target geographies
edf_key <- read.csv(file.path(datadir, "EDF_target_countries.csv"), as.is=T)


# Build data
################################################################################

# Build table
spp <- gaines_for_eez %>% 
  filter(sovereign_iso3 %in% edf_key$iso3 & range_prop>0) %>% 
  left_join(select(edf_key, -country), by=c("sovereign_iso3"="iso3")) %>% 
  group_by(group) %>% 
  summarize(n_spp=n_distinct(species)) %>% 
  arrange(desc(n_spp))

# Number of total species
n_distinct(gaines_for_eez$species)

# Export
write.csv(spp, file.path(tabledir, "Table1_n_species_by_edf_geo.csv"), row.names=F)


# Build data
################################################################################

# Setup figure
spp <- arrange(spp, n_spp)
png(file.path(plotdir, "FigS1_edf_geo_nspp.png"), width=5, height=3, units="in", res=600)
par(mar=c(3,7,0.5,0.8), mgp=c(1.8,0.5,0))
barplot(spp$n_spp, names=spp$group, las=1, cex.ax=0.9, cex.names=0.9, horiz=T,
        xlab="Number of species", xlim=c(0,600), border=F, col="grey60")
dev.off()


