

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(countrycode)
library(RColorBrewer)
library(fields)

# Directories
datadir <- "data/gaines"
plotdir <- "figures"

# Load data
gdata <- readRDS(file.path(datadir, "gaines_global_level_results_approach1.Rds"))



# Plot data
################################################################################

# Format results
gdata <- gdata %>% 
  mutate(rcp_label=revalue(rcp, c("RCP26"="RCP 2.6", 
                                   "RCP45"="RCP 4.5", 
                                   "RCP60"="RCP 6.0", 
                                   "RCP85"="RCP 8.5")),
         scenario_label=revalue(scenario, c("No Adaptation"="BAU",
                                            "Range Shift Only"="Range Shift",
                                            "Imperfect Productivity Only"="Imperfect\nProductivity",
                                            "Productivity Only"="Productivity",
                                            "Imperfect Full Adaptation"="Imperfect Full",
                                            "Full Adaptation"="Full"))) %>% 
  filter(!scenario %in% c("Imperfect Productivity Only", "Imperfect Full Adaptation") & rcp %in% c("RCP26", "RCP85"))


# Setup theme
my_theme <- theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  strip.text=element_text(size=16),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot results
range(gdata$bbmsy_avg2)
p <- ggplot(gdata, aes(x=c_pdiff, y=p_pdiff, label=scenario_label, fill=bbmsy_avg2, size=bbmsy_prop2)) +
  geom_point(pch=21) +
  geom_text_repel(col="black", size=4, point.padding = 1) +
  facet_wrap(~rcp_label, ncol=2) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                        limits=c(0.25,1.75),
                        breaks=seq(0.25,1.75,0.25),
                        name="Mean B/BMSY\nin 2100") +
  scale_size_continuous(breaks=seq(0,1,0.25), 
                        name="Proportion of stocks\nwith B/BMSY > 1.0 in 2100") +
  theme_bw() + my_theme
p

ggsave(file.path(plotdir, "Fig1_global_scatterplots_approach1_iffo.png"), width=8, height=4, units="in", dpi=600, scale=1.3)


