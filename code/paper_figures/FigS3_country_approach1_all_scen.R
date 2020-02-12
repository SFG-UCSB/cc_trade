

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
datadir1 <- "imperfect/data/"
datadir2 <- "data/gaines"
plotdir <- "figures/paper"

# Load data
data1_orig <- readRDS(file.path(datadir1, "gaines_country_level_results_approach1_imperfect.Rds"))
data2_orig <- readRDS(file.path(datadir2, "gaines_country_level_results_approach1.Rds"))

# Merge data
data_orig <- rbind(data1_orig %>% ungroup(), 
                   data2_orig %>%  filter(scenario %in% c("Range Shift Only", "Productivity Only")) %>%  ungroup())


# Plot figure
################################################################################

# Format data
data <- data_orig %>% 
  ungroup() %>% 
  # Subset relevant scenarios
  filter(rcp!="RCP26" & scenario!="Imperfect Full Adaptation 15 yr") %>% 
  # Format scenario labels
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5"),
         scenario=recode(scenario,
                         "No Adaptation"="Business-As-Usual", 
                         "Full Adaptation"="Full", 
                         "Imperfect Full Adaptation 5 yr"="Realistic (5 yr)",
                         "Imperfect Full Adaptation 10 yr"="Realistic (10 yr)",
                         "Imperfect Full Adaptation 20 yr"="Realistic (20 yr)"),
         scenario=factor(scenario, levels=c("Business-As-Usual",
                                            "Range Shift Only", "Productivity Only",
                                            "Realistic (20 yr)", "Realistic (10 yr)", "Realistic (5 yr)", 
                                            "Full"))) %>% 
  # Add percent change in MSY and cap percent differences
  mutate(c_pdiff=ifelse(abs(c_pdiff)>300, sign(c_pdiff)*300, c_pdiff),
         # Cap percent difference in profits [-100%, 300%]
         p_pdiff=ifelse(p_pdiff < -100, -100, p_pdiff), 
         p_pdiff=ifelse(p_pdiff > 300, 300, p_pdiff),
         # Calculate percent difference in MSY: cap at [-100%, 100%]
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff)) %>% 
  # Add direction of percent change for making tabulations easy
  mutate(c_pdiff_sign=ifelse(c_pdiff<0, "neg", "pos"), 
         p_pdiff_sign=ifelse(p_pdiff<0, "neg", "pos"),
         type=paste0(c_pdiff_sign, "-", p_pdiff_sign))


# Calculate stats
stats <- data %>% 
  group_by(rcp, scenario, type) %>% 
  summarize(n=n()) %>%
  filter(type!="NA-NA") %>% 
  group_by(rcp, scenario) %>% 
  mutate(ntot=sum(n),
         p=n/ntot, 
         plabel=paste0(round(p*100,0), "%")) %>% 
  mutate(xpos=ifelse(grepl("neg-", type), -100, 200),
         ypos=ifelse(grepl("-neg", type), -20, 20),
         align=ifelse(grepl("neg-", type), 0, 1))


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
  geom_point(pch=21, mapping=aes(x=c_pdiff, y=p_pdiff, fill=msy_pdiff, size=msy_tot1/1e6)) +
  facet_grid(scenario ~ rcp) +
  # Labels
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
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
ggsave(file.path(plotdir, "FigS3_country_approach1_all_scen.tiff"), width=6.5, height=8.5, units="in", dpi=600)


