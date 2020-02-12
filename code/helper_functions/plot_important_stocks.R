
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
datadir <- "data/fao"

# Load data
data <- read.csv(file.path(datadir, "1950_2016_fao_global_capture_production_formatted.csv"), as.is=T)


# Function
################################################################################


plot_top10_stocks <- function(country){
  
  # Build data
  sdata <- data %>% 
    filter(country=="French Polynesia" & year%in%2012:2016 & type=="marine") %>%
    group_by(species) %>% 
    summarize(catch_mt=mean(catch, na.rm=T)/1000) %>% 
    arrange(desc(catch_mt)) %>% 
    mutate(species=as.factor(species), 
           csum=cumsum(catch_mt)/sum(catch_mt)) %>% 
    slice(1:min(which(csum>0.975)))
  
  # Plot data
  p <- ggplot(sdata, aes(reorder(species, catch_mt), catch_mt)) + 
    geom_col() +
    theme_bw() +
    xlab("") + ylab("Catch (1000s mt)") +
    coord_flip() +
    title("Species representing ~97.5% of total catch")
  p
  
  # Save plot
  
}
