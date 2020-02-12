

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
data_orig <- readRDS(file.path(datadir, "gaines_country_level_results_approach1_imperfect.Rds"))


# Plot figure
################################################################################

# Scenarios to plot
scenarios_plot <- c("No Adaptation", "Imperfect Full Adaptation 5 yr", "Full Adaptation")

# Format data
data <- data_orig %>% 
  ungroup() %>% 
  # Subset relevant scenarios
  filter(scenario %in% scenarios_plot & rcp!="RCP26") %>% 
  # Format scenario labels
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5"),
         scenario=recode(scenario,
                         "No Adaptation"="Business-As-Usual", 
                         "Full Adaptation"="Full Adaptation", 
                         "Imperfect Full Adaptation 5 yr"="Realistic Adaptation (5 yr)"),
         scenario=factor(scenario, levels=c("Business-As-Usual", "Realistic Adaptation (5 yr)", "Full Adaptation"))) %>% 
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
         msy_pdiff_sign=ifelse(msy_pdiff<0, "neg", "pos"),
         type=paste0(c_pdiff_sign, "-", p_pdiff_sign))

# Stats
stats <- data %>% 
  filter(!is.na(msy_pdiff)) %>% 
  group_by(rcp, scenario) %>% 
  summarize(n=n(),
            n_msy_neg=sum(msy_pdiff<0),
            n_msy_neg_but_pos=sum(msy_pdiff<0 & c_pdiff>0 & p_pdiff>0),
            prop_msy_neg_but_pos=n_msy_neg_but_pos/n_msy_neg*100,
            n_msy_pos=sum(msy_pdiff>0),
            n_msy_pos_but_neg=sum(msy_pdiff>0 & c_pdiff<0 & p_pdiff<0),
            prop_msy_pos_but_neg=n_msy_pos_but_neg/n_msy_pos*100,)



# Build data for figure
################################################################################

# Build proportions
stats1 <- data %>% 
  # Remove countries without MSY trend
  filter(!is.na(msy_pdiff)) %>% 
  # Count number of outcomes in each RCP-scenario-MSY trend group
  group_by(rcp, scenario, msy_pdiff_sign, type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Calculate proportion of outcomes in each RCP-scenario-MSY trend group
  group_by(rcp, scenario, msy_pdiff_sign) %>% 
  mutate(n_sign=sum(n),
         prop=n/n_sign) %>% 
  ungroup() %>% 
  # Format labels
  mutate(type=recode(type, 
                     "neg-neg"="Lower catch and profits",
                     "neg-pos"="Lower catch but higher profits",
                     "pos-neg"="Higher catch but lower profits",
                     "pos-pos"="Higher catch and profits"),
         type=factor(type, levels=c("Lower catch and profits",
                                    "Lower catch but higher profits",
                                    "Higher catch but lower profits",
                                    "Higher catch and profits")),
         msy_pdiff_sign=recode(msy_pdiff_sign, 
                               "neg"="MSY lower in 2100\n(87, 98, and 134 countries)", 
                               "pos"="MSY higher in 2100\n(69, 58, and 22 countries)"),
         msy_pdiff_sign=factor(msy_pdiff_sign, c("MSY lower in 2100\n(87, 98, and 134 countries)",
                                                 "MSY higher in 2100\n(69, 58, and 22 countries)")))

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8), 
                  strip.text=element_text(size=7),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(stats1, aes(x=rcp, y=prop, fill=type)) +
  facet_grid(scenario ~ msy_pdiff_sign) +
  geom_bar(stat="identity") +
  # geom_text(aes(x=rcp, y=1, label=n_sign), size=3) +
  labs(x="Emissions scenario", y="Proportion of countries") +
  scale_fill_manual(name="Outcomes in\n2100 relative to today", 
                    values=brewer.pal(4, "RdBu")) +
  theme_bw() + my_theme
g

# Export plot
ggsave(file.path(plotdir, "Fig4_outcomes_by_msy_trend.tiff"), width=5.5, height=4.5, units="in", dpi=600)





