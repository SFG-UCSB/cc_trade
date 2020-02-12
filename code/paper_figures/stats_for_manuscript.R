

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
                         "Imperfect Full Adaptation 5 yr"="Good Adaptation (5 yr)"),
         scenario=factor(scenario, levels=c("Business-As-Usual", "Good Adaptation (5 yr)", "Full Adaptation"))) %>% 
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

# Stats
stats1 <- data %>% 
  filter(!is.na(msy_pdiff)) %>% 
  group_by(rcp, scenario, msy_pdiff_sign, type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(type=recode(type, 
                     "neg-neg"="Both decrease",
                     "neg-pos"="Catch decrease, profits increase",
                     "pos-neg"="Catch increase, profits decrease",
                     "pos-pos"="Both increase"
                     ),
         msy_pdiff_sign=recode(msy_pdiff_sign, 
                               "neg"="MSY decrease", 
                               "pos"="MSY increase"))


# Plot data
g <- ggplot(stats1, aes(x=rcp, y=n, fill=type)) +
  facet_grid(scenario ~ msy_pdiff_sign) +
  geom_bar(stat="identity") +
  labs(x="Emissions scenario", y="Proportion of countries") +
  scale_fill_discrete(name="Outcomes") +
  theme_bw()
g







