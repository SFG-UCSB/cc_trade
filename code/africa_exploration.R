## Tracey Mangin
## January 22, 2018
## Africa exploration

library(janitor)
library(countrycode)
library(tidyverse)

boundary_gfw <- read_csv("~/Box/SFG Centralized Resources/Projects/cc_trade/data/full_dataset.csv")

## countries of interest (West Africa)
wa_nations <- c("Morocco", "Western Sahara", "Mauritania", "Senegal", "The Gambia", "Guinea-Bissau", "Guinea",
                "Sierra Leone", "Liberia", "Cote d'Ivoire", "Ghana", "Togo", "Benin", "Nigeria", "Equatorial Guinea",
                "Gabon", "Republic of the Congo", "Democratic Republic of the Congo", "Angola", "Namibia")

wa_df <- data.frame(country = wa_nations) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c"))

gfw_wa <- boundary_gfw %>%
  filter(closest_eez_boundary_territory %in% wa_df$iso3) %>%
  group_by(closest_eez_boundary_territory, inside_eez, year, flag) %>%
  summarise(fishing_hrs = sum(fishing_hours)) %>%
  ungroup() %>%
  mutate(flag_nation = countrycode(flag, "iso3c", "country.name"))