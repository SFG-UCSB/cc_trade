
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(plyr)
library(dplyr)
library(raster)
library(ggplot2)

# Animate
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(gifski)

# Directories
datadir <- "dist_shift/data"
plotdir <- "dist_shift/animations"

# Read data
load(file.path(datadir, "upsides_species_dist_shifts.Rdata"))

# World layer
world <- map_data("world")

# Build animation
################################################################################

# Function to plot animation
# species <- "Engraulis ringens"; scenario <- 85
plot_animation(species="Engraulis ringens", scenario=26, plotdir=plotdir)
plot_animation <- function(species, scenario, plotdir){
  
  # Subset data
  spp <- species
  spp_format <- gsub(" ", "_", tolower(spp))
  if(scenario==26){data <- filter(rcp26, species==spp); rcp <- "RCP 2.6"}
  if(scenario==45){data <- filter(rcp45, species==spp); rcp <- "RCP 4.5"}
  if(scenario==60){data <- filter(rcp60, species==spp); rcp <- "RCP 6.0"}
  if(scenario==85){data <- filter(rcp85, species==spp); rcp <- "RCP 8.5"}
  sdata <- filter(data, year==2015)

  # Calculate lazy centroids
  # centroids <- data %>%
  #   group_by(year) %>% 
  #   summarize(lat_dd=mean(lat_dd),
  #             long_dd=mean(long_dd))
  
  # Centroid animation
  # p <- ggplot(centroids, aes(x=long_dd, y=lat_dd)) +
  #   geom_point() + 
  #   transition_states(year) 
  # p
  
  # Raster animation
  # p <- ggplot(data, aes(x=long_dd, y=lat_dd)) + 
  #   geom_raster(aes(fill=prop)) + 
  #   transition_states(year) 
  # p
  
  # # Plot one year
  # p <- ggplot(sdata, aes(x=long_dd, y=lat_dd)) +
  #   geom_raster(aes(fill=prop)) +
  #   coord_fixed(ratio = 1) +
  #   labs(x="", y="") +
  #   xlim(range(data$long_dd)) +
  #   ylim(range(data$lat_dd)) +
  #   theme_bw() +
  #   # scale_fill_distiller(palette = "Spectral", values=seq(0,1,0.25),
  #   #                      guide_legend(title = "Probability \nof occurence")) +
  #   guides(fill = guide_legend(reverse=TRUE), title="Probability \nof occurence") +
  #   geom_map(data=world, map=world, aes(x=long, y = lat, map_id=region), fill="grey80")
  # p

  # Build title
  a_title <- paste0(spp, "\n", rcp, ": {closest_state}")
  
  # Plot animation
  p <- ggplot(data, aes(x=long_dd, y=lat_dd)) +
    geom_raster(aes(fill=prop)) +
    # scale_fill_continuous(palette = "Spectral",
    #                      guide_legend(title = "Probability \nof occurence")) +
    guides(fill = guide_legend(reverse=TRUE), title="Probability \nof occurence") +
    coord_fixed(ratio = 1) +
    labs(x="", y="") +
    xlim(range(data$long_dd)) +
    ylim(range(data$lat_dd)) +
    theme_bw() +
    geom_map(data=world, map=world, aes(x=long, y = lat, map_id=region), fill="grey80") +
    transition_states(year, state_length = 0.7, transition_length = 0) + 
    ggtitle(a_title)
  print(p)
  
  # Calculate plot dimensions from aspect ratio
  # 1. Convert to raster
  # 2. Get extent and calculate aspect ratio
  # 3. Scale longest axis to 6 inches
  # ras <- data %>% 
  #   select(long_dd, lat_dd, prop) %>% 
  #   rasterFromXYZ()
  # extent(ras)[1]
  # xw <- extent(ras)[2] - extent(ras)[1]
  # yw <- extent(ras)[4] - extent(ras)[3]
  # aratio <- c(xw, yw)
  # aratio1 <- aratio/max(aratio)
  # aratio6 <- aratio1 * 6
  
  # Save animation
  anim_save(filename=file.path(plotdir, paste0(spp_format, "_", scenario, ".gif")), dpi=600)
  
  # Save animation using magick
  # magick::image_write(animate(p, width = aratio6[1], height = aratio6[2], res=600, units="in"), "test.gif")
  
}

# Range size over time
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot range range size over time
species <- "Engraulis ringens"; scenario <- 85
plot_range_ts <- function(species, scenario, plotdir){
  
  # Subset data
  spp <- species
  spp_format <- gsub(" ", "_", tolower(spp))
  if(scenario==26){data <- filter(rcp26, species==spp); rcp <- "RCP 2.6"}
  if(scenario==45){data <- filter(rcp45, species==spp); rcp <- "RCP 4.5"}
  if(scenario==60){data <- filter(rcp60, species==spp); rcp <- "RCP 6.0"}
  if(scenario==85){data <- filter(rcp85, species==spp); rcp <- "RCP 8.5"}

  # Calculate range size
  range_km2_ts <- data %>% 
    group_by(year) %>% 
    summarise(area_km2=sum(area_km2))
  
  # Plot range size over time
  g <- ggplot(range_km2_ts, aes(x=year, y=area_km2/1000)) +
    geom_line() + 
    xlab("") + ylab("Range (1000s sqkm)") + 
    theme_bw() + my_theme
  g
  ggsave(file.path(plotdir, paste0(spp_format, "_range_ts.png")), width=4, height=2)
  
}

# plot_range_ts(species="Engraulis ringens", scenario=85, plotdir=plotdir)

