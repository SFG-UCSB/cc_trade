

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
library(ggrepel)

# Directories
datadir <- "data/gaines"
plotdir <- "figures/country_level"

# Load data
cdata <- readRDS(file.path(datadir, "gaines_territory_level_results_approach2.Rds"))


# Function
################################################################################

# Function to plot outcomes relative to today for a single country under all RCPs
country <- "French Polynesia"
plot_approach2 <- function(country, plotdir=NA){
  
  # Subset data
  cntry <- country
  sdata <- cdata %>% 
    ungroup() %>% 
    filter(country==cntry & scenario!="No Adaptation") %>% 
    mutate(rcp=revalue(rcp, c("RCP26"="RCP 2.6",
                              "RCP45"="RCP 4.5",
                              "RCP60"="RCP 6.0",
                              "RCP85"="RCP 8.5")))
  
  # Plot data
  p <- ggplot(sdata, aes(x=c_tot_rel, y=p_tot_rel, label=scenario)) +
    geom_point(aes(fill=bbmsy_good_prop), size=5, shape=21) +
    geom_text_repel(point.padding=1, size=2) +
    facet_wrap(.~rcp, nrow=2) +
    theme_bw() +
    xlab("Percent difference in cumulative catch\n(compared to no adaptation scenario)") +
    ylab("Percent difference in cumulative profits\n(compared to no adaptation scenario)") +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    # xlim(c(-50, 25)) +
    # ylim(c(-100,50)) +
    expand_limits(x=c(-10,10), y=c(-10,10)) +
    scale_fill_gradientn(colours=brewer.pal(9, "Blues"), 
                         limits=c(0,1),
                         breaks=seq(0,1,0.2),
                         # labels=seq(0,1,0.2),
                         name="Proportion of years\nwith B/BMSY > 1.0") +
    # scale_fill_continuous(name="Proportion of years\nwith B/BMSY > 1.0") +
    theme(legend.position="right")
  print(p)
  
  # Print to file if asked
  if(!is.na(plotdir)){
    filename <- paste0(gsub(" ", "_", tolower(cntry)), "_future_vs_no_adaptation.png")
    ggsave(plot=p, filename=file.path(plotdir, filename), width=6, height=5, units="in", dpi=600)
  }
  
}

# Apply function
################################################################################

plot_approach2("French Polynesia", plotdir)




