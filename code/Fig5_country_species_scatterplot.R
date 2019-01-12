
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

# Species key
spp_key <- unique(select(gaines_for_eez, species, comm_name))

# Build data
################################################################################

# Reference year
ref_year <- 2100

# Calculate 2012 and final year totals
data <- gaines_for_eez %>% 
  filter(sovereign_iso3 %in% edf_key$iso3 & year %in%c(2012, ref_year)) %>% 
  group_by(sovereign_iso3, rcp, species, scenario, year) %>% 
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
  left_join(tot2012, by=c("sovereign_iso3", "species")) %>% 
  # Compute proportion change
  mutate(b_prop=(b_final - b_2012)/b_2012,
         h_prop=(h_final - h_2012)/h_2012,
         p_prop=(p_final - p_2012)/p_2012) %>% 
  # Remove stocks without proportional change
  filter(!is.na(b_prop) & !is.infinite(b_prop),
         !is.na(h_prop) & !is.infinite(h_prop),
         !is.na(p_prop) & !is.infinite(p_prop)) %>% 
  # Revalue propotional change
  mutate(b_prop_adj=ifelse(b_prop>=3,3,b_prop),
         h_prop_adj=ifelse(h_prop>=3,3,h_prop),
         p_prop_adj=ifelse(p_prop>=3,3,p_prop))
  

# Plot data
################################################################################

# Add profit change colors/weights
# range(data1$p_prop_adj, na.rm=T)
# bins <- seq(-3.5, 3.5, 0.25)
# colors <- freeR::colorpal(brewer.pal(11, "RdBu"), length(bins))
# data1 <- data1 %>%
#   mutate(p_prop_bin=cut(p_prop, breaks=bins),
#          p_prop_col=colors[p_prop_bin])

# MSY 2012 data frame
msy12 <- gaines_for_eez %>% 
  select(species, d)

# Stock status data frame
status <- gaines_for_eez %>% 
  filter(year=="2012" & scenario=="No Adaptation") %>% 
  select(species, bbmsy, ffmsy) %>% 
  unique() %>% 
  mutate(status_b=ifelse(bbmsy<1, "overfished", "not overfished"),
         status_f=ifelse(ffmsy>=1, "overfishing", "not overfishing"),
         status=ifelse(status_b=="not overfished" & status_f=="not overfishing", "healthy", NA),
         status=ifelse(status_b=="not overfished" & status_f=="overfishing", "emerging", status),
         status=ifelse(status_b=="overfished" & status_f=="not overfishing", "recovering", status),
         status=ifelse(status_b=="overfished" & status_f=="overfishing", "overfished", status),
         status_col=revalue(status, c("healthy"="blue",
                                      "emerging"="orange",
                                      "recovering"="green",
                                      "overfished"="purple")))

# MSY in 2012
msy12 <- gaines_for_eez %>% 
  filter(year=="2012" & scenario=="No Adaptation" & rcp=="RCP26") %>% 
  group_by(species, sovereign_iso3) %>% 
  summarize(msy=sum(msy2012*range_prop),
            neezs=n())

# Add status to data
data2 <- data1 %>% 
  left_join(status, by=c("species")) %>% 
  left_join(msy12, by=c("species", "sovereign_iso3"))

# Scenarios
rcps <- "RCP85"
# rcps <- sort(unique(data1$rcp))
scenarios <- c("No Adaptation", "Range Shift Only", 
               "Productivity Only", "Full Adaptation")

# Loop through simple countries
countries <- unique(edf_key$group)
countries <- countries[!countries %in% c("European Union", "Pacific Islands")]
i <- j <- k <- 1
for(k in 1:length(countries)){
  
  # Country
  cntry <- countries[k]
  cntry_format <- gsub(" ", "_", tolower(cntry))
  iso3 <- edf_key$iso3[edf_key$country==cntry]

  # Loop through RCPs
  for(i in 1:length(rcps)){
    
    # Setup figure
    figname <- paste0("Fig5_", cntry_format, "_scatterplot_", tolower(rcps[i]), ".png")
    png(file.path(plotdir, figname), height=6.5, width=6.5, units="in", res=600)
    par(mfrow=c(2,2), mar=c(4,4,1,0.5), mgp=c(2.5,0.8,0))
    
    # Loop through adaptation scenarios
    for(j in 1:length(scenarios)){
      
      # Subset data
      sdata <- data2 %>% 
        filter(sovereign_iso3==iso3 & rcp==rcps[i] & scenario==scenarios[j]) %>% 
        left_join(spp_key, by="species")

      # Plot data
      plot(h_prop_adj ~ b_prop_adj, sdata, bty="n", las=1,
           xlab="Difference in biomass", ylab="Difference in harvest",
           main=scenarios[j], bg=freeR::tcolor(status_col, 0.5), pch=21, cex.axis=0.9, col="grey30",
           xlim=c(-1, 3), ylim=c(-1,3), cex=scales::rescale(sdata$msy, to=c(1, 4.5)))
      lines(x=c(-1,3), y=c(0,0), lty=3)
      lines(x=c(0,0), y=c(-1,3), lty=3)
      if(j==1){text(x=3, y=-1, pos=2, labels=paste(nrow(sdata), "species"), cex=0.8)}
      
      # Label large stocks
      lg_sdata <- sdata %>% 
        arrange(desc(msy)) %>% 
        ungroup() %>% 
        dplyr::slice(1:5)
      text(x=lg_sdata$b_prop_adj, y=lg_sdata$h_prop_adj, 
           labels=lg_sdata$comm_name, font=2, cex=0.8, xpd=NA)
      
    }
    
    # Off
    dev.off()
    
  }
  
}

# Full adaptation only
################################################################################

# Remove levereage?
remove_biggest <- T

# Loop through simple countries
countries <- unique(edf_key$group)
countries <- countries[!countries %in% c("European Union", "Pacific Islands")]
i <- j <- k <- 1
for(k in 1:length(countries)){
  
  # Country
  cntry <- countries[k]
  cntry_format <- gsub(" ", "_", tolower(cntry))
  iso3 <- edf_key$iso3[edf_key$country==cntry]
  
  # Loop through RCPs
  for(i in 1:length(rcps)){
    
    # Setup figure
    if(remove_biggest==T){
      figname <- paste0("Fig5_", cntry_format, "_scatterplot_", tolower(rcps[i]), "_full_adapt_largest_out.png")
    }else{
      figname <- paste0("Fig5_", cntry_format, "_scatterplot_", tolower(rcps[i]), "_full_adapt.png")
    }
    png(file.path(plotdir, figname), height=4.5, width=4.5, units="in", res=600)
    par(mfrow=c(1,1), mar=c(3.5,3.5,1,0.5), mgp=c(2,0.8,0), xpd=NA)
      
    # Subset data
    if(remove_biggest==T){
      sdata <- data2 %>% 
        filter(sovereign_iso3==iso3 & rcp==rcps[i] & scenario=="Full Adaptation") %>% 
        left_join(spp_key, by="species") %>% 
        ungroup() %>% 
        filter(msy!=max(msy, na.rm=T))
    }else{
      sdata <- data2 %>% 
        filter(sovereign_iso3==iso3 & rcp==rcps[i] & scenario=="Full Adaptation") %>% 
        left_join(spp_key, by="species")
    }
    
    # Plot data
    plot(h_prop_adj ~ b_prop_adj, sdata, bty="n", las=1,
         xlab="Difference in biomass", ylab="Difference in harvest",
         main=paste(cntry, rcps[i], sep=" - "), bg=freeR::tcolor(status_col, 0.5), pch=21, cex.axis=0.9, col="grey30",
         xlim=c(-1, 3), ylim=c(-1,3), cex=scales::rescale(sdata$msy, to=c(1, 4.5)))
    lines(x=c(-1,3), y=c(0,0), lty=3)
    lines(x=c(0,0), y=c(-1,3), lty=3)
    if(j==1){text(x=3, y=-1, pos=2, labels=paste(nrow(sdata), "species"), cex=0.8)}
    
    # Label large stocks
    lg_sdata <- sdata %>% 
      arrange(desc(msy)) %>% 
      ungroup() %>% 
      dplyr::slice(1:5)
    text(x=lg_sdata$b_prop_adj, y=lg_sdata$h_prop_adj, 
         labels=lg_sdata$comm_name, font=2, cex=0.8, xpd=NA)
    
    # Off
    dev.off()
    
  }
  
}
