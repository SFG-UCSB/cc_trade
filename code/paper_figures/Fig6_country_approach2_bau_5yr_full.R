

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
datadir <- "imperfect/data/"
plotdir <- "figures/paper"

# Load data
data_orig <- readRDS(file.path(datadir, "gaines_country_level_results_approach2_imperfect.Rds"))


# Plot figure
################################################################################

# Scenarios to plot
scenarios_plot <- c("Imperfect Full Adaptation 5 yr", "Full Adaptation")

# Format data
data <- data_orig %>% 
  ungroup() %>% 
  # Subset relevant scenarios
  filter(scenario %in% scenarios_plot & rcp!="RCP26") %>% 
  filter(p_tot_rel > 0) %>% 
  # Format scenario labels
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5"),
         scenario=recode(scenario,
                         "Full Adaptation"="Full Adaptation", 
                         "Imperfect Full Adaptation 5 yr"="Realistic Adaptation (5 yr)"),
         scenario=factor(scenario, levels=c("Realistic Adaptation (5 yr)", "Full Adaptation"))) %>% 
  # Add percent change in MSY and cap percent differences
  mutate(c_tot_rel=ifelse(abs(c_tot_rel)>300, sign(c_tot_rel)*300, c_tot_rel),
         p_tot_rel=ifelse(abs(p_tot_rel)>300, sign(p_tot_rel)*300, p_tot_rel), 
         # Calculate percent difference in MSY: cap at [-100%, 100%]
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff)) %>% 
  # Add direction of percent change for making tabulations easy
  mutate(c_sign=ifelse(c_tot_rel<0, "neg", "pos"))


# Calculate stats
stats <- data %>% 
  group_by(rcp, scenario, c_sign) %>% 
  summarize(n=n()) %>%
  group_by(rcp, scenario) %>% 
  mutate(ntot=sum(n),
         p=n/ntot, 
         plabel=paste0(round(p*100,0), "%")) %>% 
  mutate(xpos=ifelse(c_sign=="neg", -30, 100),
         ypos=15,
         align=ifelse(c_sign=="neg", 0, 1),
         nudge_x=ifelse(c_sign=="neg", -5, 5))


# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8), 
                  strip.text=element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
p <- ggplot(data) +
  geom_point(pch=21, mapping=aes(x=c_tot_rel, y=p_tot_rel, fill=msy_pdiff, size=msy_tot1/1e6)) +
  facet_grid(scenario ~ rcp) +
  # Labels
  xlab("% difference in cumulative catch\n(relative to business-as-usual)") + 
  ylab("% difference in cumulative profits\n(relative to business-as-usual)") + 
  # Reference lines
  geom_hline(yintercept=0, col="black", size=0.4) +
  geom_vline(xintercept=0, col="black", size=0.4) +
  # Add labels
  geom_text(data=stats, mapping=aes(x=xpos, y=ypos, hjust=align, label=plabel), col="grey50", size=2) +
  # Legends
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       labels=seq(-100,100,25),
                       name="Percent change in MSY\n(2100 relative to today)") +
  scale_size_continuous(name="MSY (millions mt)") +
  # Themes
  theme_bw() + my_theme
p

# Export
ggsave(file.path(plotdir, "Fig6_country_approach2_none_partial_full.tiff"), width=6.5, height=3.5, units="in", dpi=600)


