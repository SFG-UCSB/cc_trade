

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(ggplot2)
library(rworldmap)
library(countrycode)
library(maptools)
library(sf)
library(rnaturalearth)
library(grid)
library(gridExtra)


# Directories
datadir <- "data/gaines"
plotdir <- "figures/paper"
eezdir <- "data/eezs/processed"
eezdir2 <- "data/eezs/raw"


# Load data
tmsy <- readRDS(file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))

# Load EEZ shapefile
eezs <- readRDS(file.path(eezdir, "world_eezs_v8.Rds"))

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")

# Read country centroids
# https://www.arcgis.com/home/item.html?id=f4d8f9131fe6411f8da592208fbe5ddc
centroids <- read.csv(file.path(eezdir2, "country_centroids_all.csv"),
                      header=T, sep="\t", as.is=T) %>% 
  setNames(tolower(colnames(.))) %>% 
  mutate(iso3=countrycode(short_name, "country.name", "iso3c")) %>% 
  select(short_name, full_name, iso3, iso3136, fips10, lat, long) %>% 
  rename(lat_dd=lat, long_dd=long, iso2=iso3136)


# Build map data
################################################################################

# Calculate percent change in mean MSY from beginning to end
yrs1 <- 2012:2021; length(yrs1)
yrs2 <- 2091:2100; length(yrs2)
data <- tmsy %>% 
  ungroup() %>% 
  # Sum MSY by EEZ and year
  group_by(rcp, sovereign, sovereign_iso3, country, eez_id, eez, year) %>%
  summarize(msy=sum(msy)) %>% 
  # Average over periods
  group_by(rcp, sovereign, sovereign_iso3, country, eez_id, eez) %>% 
  summarize(msy_avg1=mean(msy[year%in%yrs1]),
            msy_avg2=mean(msy[year%in%yrs2]),
            msy_perc=(msy_avg2-msy_avg1)/msy_avg1*100,
            msy_perc=pmin(300, msy_perc)) %>% 
  # Remove outrageous values
  filter(msy_perc <= 100 & msy_perc>=-100)

# Plot distribution of MSY change percentages
hist(data$msy_perc, breaks=seq(-100,100,10))

# Add data to EEZ shapefile
eezs1 <- eezs %>%
  left_join(data, by="eez") %>% 
  filter(!is.na(rcp) & rcp!="RCP26") %>% 
  mutate(rcp=plyr::revalue(rcp, c("RCP45"="RCP 4.5",
                                  "RCP60"="RCP 6.0",
                                  "RCP85"="RCP 8.5")))

# Export data for Anna Cabre <annanusca@gmail.com>
data_out <- data %>% 
  ungroup() %>% 
  filter(!is.na(rcp) & rcp!="RCP26") %>% 
  mutate(rcp=plyr::revalue(rcp, c("RCP45"="RCP 4.5",
                                  "RCP60"="RCP 6.0",
                                  "RCP85"="RCP 8.5")))
write.csv(data_out, file="~/Desktop/Fig1_data.csv", row.names=F)

# Build time series data
################################################################################

# EEZ centroids
eez_centers <- eezs %>% 
  sf::st_transform("+proj=moll") %>% 
  sf::st_centroid() %>% 
  sf::st_transform("+init=epsg:4326") %>% 
  sf::st_coordinates() %>% 
  as.tibble() %>% 
  rename(long_dd=X, lat_dd=Y) %>% 
  mutate(eez=eezs$eez,
         lat_dd_abs=abs(lat_dd)) 

# Add lats to results
data1 <- data %>% 
  left_join(eez_centers, by="eez") %>% 
  filter(rcp!="RCP26") %>% 
  ungroup() %>% 
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5", 
                    "RCP60"="RCP 6.0", 
                    "RCP85"="RCP 8.5"))
  

# Theme
my_theme0 <- theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10), 
                   strip.text=element_text(size=10),
                   plot.title=element_text(size=10),
                   panel.grid.major = element_line(colour = 'transparent'),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot
g <- ggplot(data1, aes(x=lat_dd_abs, y=msy_perc)) +
  geom_point(color="grey40", size=0.8) +
  facet_wrap(~rcp) +
  geom_hline(yintercept = 0, linetype="dotted", color="grey10") +
  geom_smooth(method='lm', formula= y ~ x, color="black") +
  labs(x="Latitude\n(absolute degrees)", y="Percent change in MSY\n(2100 relative to today)") +
  theme_bw() + my_theme0
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS2_msy_latitude_correlation.tiff"), 
       width=6.5, height=3, units="in", dpi=600)


# Build time series data
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

# Plot data
################################################################################

# Theme
my_theme1 <- theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10), 
                   strip.text=element_text(size=10),
                   plot.title=element_text(size=10),
                   legend.position = "bottom",
                   # axis.text.x = element_text(angle = 90, hjust = 0.5),
                   # panel.grid.major = element_blank(), 
                   panel.grid.major = element_line(colour = 'transparent'),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Theme
my_theme2 <- theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   plot.title=element_text(size=12),
                   # axis.text.x = element_text(angle = 90, hjust = 0.5),
                   legend.position = "bottom",
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot maps
g1 <- ggplot(eezs1) +
  geom_sf(aes(fill=msy_perc), lwd=0.05, col="white", label_axes="--EN") +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  geom_hline(aes(yintercept = 0), linetype="dotted", color="grey30", size=0.1) +
  facet_wrap(~ rcp, ncol=1) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,50),
                       name="Percent change in MSY\n(2100 relative to today)") +
  # Add invisible x-axis to line up with other panels
  scale_x_continuous(breaks=c(-90, 90), labels=rep("a",2)) +
  ylab("") +
  theme_bw() + my_theme1

# Plot data
g2 <- ggplot(sov_msy, aes(x=year, y=msy_mt_pdiff, group=sovereign, col=lat_dd_abs)) +
  facet_wrap(~rcp, ncol=1) +
  # Plot country lines
  geom_line(alpha=0.3) +
  # Plot global lines
  geom_line(g_msy, mapping=aes(x=year, y=msy_mt_pdiff, group=NA), col="black") +
  # Plot reference line and text
  geom_hline(yintercept=0, col="grey30", lwd=0.6, linetype="dotted") +
  geom_text(data=g_final, mapping=aes(x=2100, y=msy_mt_pdiff-5, label=msy_label, group=NA), 
            col="black", hjust=0, size=3) +
  # Labels, legend, theme
  labs(x="", y="Percent change in MSY\n(relative to today)") +
  scale_color_gradientn(name="Latitude\n(° absolute)", 
                        colors=RColorBrewer::brewer.pal(9,"RdBu"), na.value="grey30") +
  scale_x_continuous(limits=c(2020, 2110), breaks=seq(2020,2100,20)) +
  theme_bw() + my_theme1

# Arrange
g <- grid.arrange(g1, g2, ncol=2)


# Export
ggsave(plot=g, file.path(plotdir, "Fig1_msy_trends_final.tiff"), 
       width=6.5, height=6.5, units="in", dpi=600)

