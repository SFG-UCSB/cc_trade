
# Setup
################################################################################

# Packages
library(dplyr)
library(countrycode)

# Directories
datadir <- "data"
plotdir <- "figures/edf_targets"

# Countries
################################################################################

# Countries
countries <- c("United States", "Mexico", "Peru", "Chile", "China", 
               "Myanmar", "Japan", "Indonesia", "Philippines", "Vietnam")

               
# European Union countries               
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
                  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
                  "Slovenia", "Spain", "Sweden", "United Kingdom")
 
# Pacific Island countries
pi_countries <- c("Fiji", "Kiribati", "Marshall Islands", "Federated States of Micronesia", 
                  "Nauru", "Palau", "Samoa", 
                  "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")

# Merge and export countries
################################################################################

# Merge countries
data <- data.frame(group=c(countries, 
                           rep("European Union", length(eu_countries)),
                           rep("Pacific Islands", length(pi_countries))),
                   country=c(countries, eu_countries, pi_countries), stringsAsFactors=F) %>% 
  mutate(iso3=countrycode(country, "country.name", "iso3c")) %>% 
  select(group, iso3, country)

# Export
write.csv(data, file.path(datadir, "EDF_target_countries.csv"), row.names=F)


# Plot countries
################################################################################

# Packages
library(ggplot2)
library(rworldmap)
map.world <- map_data(map="world")


# Add ISO3 and EDF to world map
world <- map_data(map="world")
world <- world %>% 
  mutate(iso3=countrycode(region, "country.name", "iso3c"),
         edf=iso3 %in% data$iso3)


# Plot EDF target geographies
p <- (ggplot()
  + theme_bw()
  + theme(legend.position="none")
  + geom_map(data=world, map=world, aes(map_id=region, fill=edf), color="white", size=0.05)
  + scale_fill_manual(values=c("grey70", "grey20")) 
  + coord_equal() + xlim(-180,180) + ylim(-90,90))
p

# Export
ggsave(p, filename=file.path(plotdir, "Fig1_EDF_target_geo_map.png"), 
       dpi = 300, width = 6.5, height = 3.5, units = "in")



