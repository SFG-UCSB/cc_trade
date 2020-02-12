
# To do list:
# Finish building species key
# Finish adding species to formatted data frames

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

# Directories
datadir <- "dist_shift/data"

# Read data
load(file.path(datadir, "UpdateResults_11_2017.RData"))

# Formatting notes
################################################################################

# Notes from Tracey
# 1) Ignore "RCP45_" in the field year -- results are fine, it was a labeling mistake; 
# 2) table SpxCid_rcp45 has information for the initial year 2012 (not added to the other tables to avoide repetition); 
# 3) EEZ_HS_ids table can be used to link EEZ ids to country/sovereign names; 
# 4) cell numbers correspond to raster/matrix with 180 rows and 360 columns

# Info
# Years: 2015-2100 (5 year intervals), 2012 data in RCP 4.5 but not others
# Species: 781 species
# Grid: 1°x1° cells, 180 rows x 360 cols


# Build species key
################################################################################

# Read FAO species key
fao_spp_orig <- import(file.path(datadir, "ASFIS_sp", "ASFIS 6 languages_2018.xlsx"))
fao_spp <- fao_spp_orig %>% 
  setNames(tolower(colnames(.)))  %>% 
  select(isscaap:english_name) %>% 
  rename(alpha3="3a_code", taxa_code=taxocode, species=scientific_name, comm_name=english_name)
  
# Unique species
spp <- unique(SpxCid_rcp45$SciName)

# Build key
spp_key <- fao_spp %>%
  filter(species %in% spp)

# Build key to relate cell numbers to lat/longs
################################################################################

# Projection
wgs84 <- "+init=epsg:4326"

# Build raster
world_ras <- raster(vals=1:(180*360),
                     nrows=180, ncols=360, 
                     xmn=-180, xmx=180,
                     ymn=-90, ymx=90,
                     crs=CRS(wgs84))

# Convert matrix to make sure values proceed by row
world_mat <- as.matrix(world_ras)

# Flatten raster into a key that relates cell numbers to lat/longs
cell_key <- cbind(1:ncell(world_ras), coordinates(world_ras)) %>% 
  as.data.frame() %>% 
  rename(cell=V1, long_dd=x, lat_dd=y)


# Format data
################################################################################

# Format 2012 data (in RCP 4.5)
# Don't add lat/long because this will get added when added to scenarios
d2012 <- SpxCid_rcp45 %>% 
  # Reduce to 2012
  filter(year=="C2012") %>% 
  # Rename columns
  rename(species=SciName, species_id=SpeciesID, cell=cells, prop=prop_marine) %>% 
  # Add scenario and format year
  mutate(rcp="Present",
         species=as.character(species),
         year=gsub("C", "", year), # occurs in RCP4.5 output for 2012
         year=as.numeric(year)) %>% 
  # Arrange columns
  select(rcp, species, species_id, year, cell, area_km2, prop)

# Function to format data
# data <- SpxCid_rcp26; rcp <- "RCP 2.6"
format_data <- function(data, rcp){
  out <- data %>% 
    # Rename columns
    rename(species=SciName, species_id=SpeciesID, cell=cells, prop=prop_marine) %>% 
    # Add scenario and format year
    mutate(rcp=rcp,
           species=as.character(species),
           year=as.character(year),
           year=gsub("RCP45_", "", year), # occurs in all outputs
           year=gsub("C", "", year), # occurs in RCP4.5 output for 2012
           year=as.numeric(year)) %>% 
    # Remove 2012 then add back in
    filter(year>2012) %>% 
    rbind.fill(d2012) %>% 
    # Add lat/longs
    left_join(cell_key, by="cell") %>% 
    # Arrange columns
    select(rcp, species, species_id, year, cell, long_dd, lat_dd, area_km2, prop) %>% 
    arrange(year, species, cell)
  return(out)
}

# Format data
rcp26 <- format_data(SpxCid_rcp26, "RCP 2.6")
rcp45 <- format_data(SpxCid_rcp45, "RCP 4.5")
rcp60 <- format_data(SpxCid_rcp60, "RCP 6.0")
rcp85 <- format_data(SpxCid_rcp85, "RCP 8.5")

# Check completeness
# freeR::complete(rcp26)
# freeR::complete(rcp45)
# freeR::complete(rcp60)
# freeR::complete(rcp85)


# Export data
################################################################################

# Export data
save(rcp26, rcp45, rcp60, rcp85,
     file=file.path(datadir, "upsides_species_dist_shifts.Rdata"))


# Subset and visualize a distribution
################################################################################

# Subset a species and year
sdata <- rcp85 %>% 
  filter(species=="Engraulis ringens" & year==2100)

# Plot distribution
world <- map_data("world")
p <- ggplot()
p <- p + geom_raster(data = sdata, aes(x=long_dd, y = lat_dd, fill=prop)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(x="", y="") +
  xlim(-180,180) + ylim(-90,90) +
  geom_map(data=world, map=world, aes(x=long, y = lat, map_id=region), fill="grey80")
p


