

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/gaines"
eezdir <- "data/eezs/raw"

# Load data
tmsy <- readRDS(file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))

# Read country centroids
# https://www.arcgis.com/home/item.html?id=f4d8f9131fe6411f8da592208fbe5ddc
centroids <- read.csv(file.path(eezdir, "country_centroids_all.csv"),
                      header=T, sep="\t", as.is=T) %>% 
  setNames(tolower(colnames(.))) %>% 
  mutate(iso3=countrycode(short_name, "country.name", "iso3c")) %>% 
  select(short_name, full_name, iso3, iso3136, fips10, lat, long) %>% 
  rename(lat_dd=lat, long_dd=long, iso2=iso3136)


# Build data
################################################################################

# Years
yrs1 <- 2012:2021; length(yrs1)

# Build global data
g_msy <- tmsy %>% 
  # Remove RCP 2.6
  filter(rcp!="RCP26") %>%
  # MSY by sovereign country
  group_by(rcp, year) %>% 
  summarize(msy_mt=sum(msy)) %>% 
  # MSY rolling means and percent differences
  ungroup() %>% 
  group_by(rcp) %>% 
  mutate(msy_mt_yrs1=mean(msy_mt[year%in%yrs1]),
         msy_mt_10yr=zoo::rollmean(msy_mt, 10, align="right", fill=NA), 
         msy_mt_pdiff=(msy_mt_10yr-msy_mt_yrs1)/msy_mt_yrs1*100) %>% 
  # Remove empty years
  ungroup() %>% 
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5", 
                    "RCP60"="RCP 6.0", 
                    "RCP85"="RCP 8.5"))

# Final global MSY change
g_final <- g_msy %>% 
  filter(year==2100) %>% 
  mutate(msy_label=paste0(freeR::roundf(msy_mt_pdiff,1), "%"))

# Build country data
sov_msy <- tmsy %>% 
  # Remove RCP 2.6
  filter(rcp!="RCP26") %>%
  # MSY by sovereign country
  group_by(rcp, sovereign, sovereign_iso3, year) %>% 
  summarize(msy_mt=sum(msy)) %>% 
  # MSY rolling means and percent differences
  ungroup() %>% 
  group_by(rcp, sovereign, sovereign_iso3) %>% 
  mutate(msy_mt_yrs1=mean(msy_mt[year%in%yrs1]),
         msy_mt_10yr=zoo::rollmean(msy_mt, 10, align="right", fill=NA), 
         msy_mt_pdiff=(msy_mt_10yr-msy_mt_yrs1)/msy_mt_yrs1*100) %>% 
  # Remove empty years
  ungroup() %>% 
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5", 
                    "RCP60"="RCP 6.0", 
                    "RCP85"="RCP 8.5")) %>% 
  filter(!is.na(msy_mt_pdiff)) %>% 
  # Add latitude
  left_join(select(centroids, iso3, lat_dd), by=c("sovereign_iso3"="iso3")) %>% 
  mutate(lat_dd_abs=abs(lat_dd))

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  axis.text.x = element_text(angle = 90, hjust = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(sov_msy, aes(x=year, y=msy_mt_pdiff, group=sovereign, col=lat_dd_abs)) +
  facet_wrap(~rcp, ncol=3) +
  # Plot country lines
  geom_line(alpha=0.3) +
  # Plot global lines
  geom_line(g_msy, mapping=aes(x=year, y=msy_mt_pdiff, group=NA), col="black") +
  # Plot reference line and text
  geom_hline(yintercept=0, col="grey30", lwd=0.6, linetype="dotted") +
  geom_text(data=g_final, mapping=aes(x=2100, y=msy_mt_pdiff, label=msy_label, group=NA), 
            col="black", hjust=0) +
  # Labels, legend, theme
  labs(x="", y="Percent difference in MSY\n(relative to today)") +
  scale_color_gradientn(name="Latitude\n(° absolute)", 
                        colors=RColorBrewer::brewer.pal(9,"RdBu"), na.value="grey30") +
  scale_x_continuous(limits=c(2020, 2110), breaks=seq(2020,2100,20)) +
  theme_bw() + my_theme
g


  
  
  
  

