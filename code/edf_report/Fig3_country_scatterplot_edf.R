
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)
library(countrycode)
library(RColorBrewer)

# Directories
datadir <- "data"
plotdir <- "figures/edf_targets"
codedir <- "code"

# Read data
gaines_for_eez <- readRDS("data/gaines/gaines_data_for_eez_analysis.Rds")

# Read EDF target geographies
edf_key <- read.csv(file.path(datadir, "EDF_target_countries.csv"), as.is=T)


# Build data
################################################################################

# Reference year
ref_year <- 2100

# Calculate 2012 and final year totals
data <- gaines_for_eez %>% 
  filter(year %in%c(2012, ref_year)) %>% 
  group_by(sovereign_iso3, rcp, scenario, year) %>% 
  summarize(b_tot=sum(biomass_eez, na.rm=T),
            h_tot=sum(harvest_eez, na.rm=T),
            p_tot=sum(profit_eez, na.rm=T))

# Initial year values
tot2012 <- data %>%
  filter(rcp == "RCP26" & scenario == "No Adaptation" & year == 2012) %>% 
  ungroup() %>% 
  select(-c(rcp, scenario, year)) %>% 
  rename(b_2012=b_tot, h_2012=h_tot, p_2012=p_tot)

# Calculate proportional change
data1 <- data %>% 
  # Reduce to final year values
  filter(year==ref_year) %>% 
  select(-year) %>% 
  rename(b_final=b_tot, h_final=h_tot, p_final=p_tot) %>% 
  # Add initial year values
  left_join(tot2012, by="sovereign_iso3") %>% 
  # Compute proportion change
  mutate(b_prop=(b_final - b_2012)/b_2012,
         h_prop=(h_final - h_2012)/h_2012,
         p_prop=(p_final - p_2012)/p_2012) %>% 
  # Mark whether EDF country
  mutate(edf=sovereign_iso3 %in% edf_key$iso3)
  

# Plot data
################################################################################

# Add profit change colors/weights
range(data1$p_prop, na.rm=T)
bins <- seq(-3.5, 3.5, 0.25)
colors <- freeR::colorpal(brewer.pal(11, "RdBu"), length(bins))
data1 <- data1 %>% 
  mutate(p_prop_bin=cut(p_prop, breaks=bins), 
         p_prop_col=colors[p_prop_bin])

# Scenarios
rcps <- sort(unique(data1$rcp))
scenarios <- c("No Adaptation", "Range Shift Only", 
               "Productivity Only", "Full Adaptation")
  
# Loop through RCPs
for(i in 1:length(rcps)){
  
  # Setup figure
  figname <- paste0("Fig3_country_scatterplot_edf_", tolower(rcps[i]), ".png")
  png(file.path(plotdir, figname), height=6.5, width=6.5, units="in", res=600)
  par(mfrow=c(2,2), mar=c(4,4,1,0.5), mgp=c(2.5,0.8,0))
  
  # Loop through adaptation scenarios
  for(j in 1:length(scenarios)){
    
    # Subset data
    sdata <- data1 %>% 
      filter(rcp==rcps[i] & scenario==scenarios[j])
    edata <- filter(sdata, edf==T)
    
    # Plot data
    plot(h_prop ~ b_prop, sdata, subset=edf==F, bty="n", las=1,
         xlab="Difference in biomass", ylab="Difference in harvest",
         main=scenarios[j], bg="grey95", pch=21, cex=0.9, cex.axis=0.9, col="grey30",
         xlim=c(-1, 2.5), ylim=c(-1,1.5))
    points(x=edata$b_prop, y=edata$h_prop, bg=edata$p_prop_col, pch=21, cex=1.5)
    lines(x=c(-1,2.5), y=c(0,0), lty=3)
    lines(x=c(0,0), y=c(-1,1.5), lty=3)
    
  }
  
  # Off
  dev.off()
  
}
