## Tracey Mangin
## January 22, 2018
## Africa exploration

library(janitor)
library(countrycode)
library(scales)
library("wesanderson")
names(wes_palettes)library(tidyverse)

pathstart <- "~/Box/SFG Centralized Resources/Projects/cc_trade/"

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


## First, let's look at fishing within EEZs

gfw_wa_eez <- gfw_wa %>%
  filter(inside_eez == TRUE) %>%
  mutate(flag_region = ifelse(flag %in% wa_df$iso3, "West Africa", "Other")) %>%
  group_by(closest_eez_boundary_territory, year, flag_region) %>%
  summarise(sum_fishing_hrs = sum(fishing_hrs)) %>%
  ungroup() %>%
  group_by(closest_eez_boundary_territory, year) %>%
  mutate(total_hrs = sum(sum_fishing_hrs)) %>%
  ungroup() %>%
  mutate(rel_hrs = sum_fishing_hrs / total_hrs,
         grp = paste(closest_eez_boundary_territory, flag_region))

pal2 <- wes_palette("Darjeeling1", 5, type = "discrete")


fishing_hrs1 <- ggplot(gfw_wa_eez, aes(x = closest_eez_boundary_territory, y = sum_fishing_hrs / 1000, fill = flag_region)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey") +
  facet_wrap(~ year, ncol = 2) +
  scale_fill_manual(values = c("#5BBCD6", "#00A08A")) +
  xlab("EEZ") +
  ylab("Fishing hours (thousands)") +
  guides(fill = guide_legend(title = "Flag region")) +
  ggtitle("Fishing hours within EEZs from GFW data") +
  theme_minimal()

ggsave(filename = paste0(pathstart, "outputs/figures/prelim/wa_effort_in_eez.pdf"), fishing_hrs1, width = 11, height = 11, units = "in")


# fishing_hrs2017 <- ggplot(gfw_wa_eez %>% filter(year == 2017), aes(x = reorder(closest_eez_boundary_territory, total_hrs), y = sum_fishing_hrs / 1000, fill = flag_region)) +
#   geom_bar(stat = "identity") +
#   geom_hline(yintercept = 0, size = 0.5, color = "grey") +
#   # facet_wrap(closest_eez_boundary_territory, scales = "free") +
#   scale_fill_manual(values = c("#5BBCD6", "#00A08A")) +
#   xlab("EEZ") +
#   ylab("Fishing hours (thousands)") +
#   guides(fill = guide_legend(title = "Flag region")) +
#   ggtitle("Fishing hours within EEZs from GFW data: 2017") +
#   theme_minimal()
# 
# ggsave(filename = paste0(pathstart, "outputs/figures/prelim/wa_effort_in_eez_2017.pdf"), fishing_hrs2017, width = 11, height = 11, units = "in")

## make figure with both relative and total hours
gfw_wa_eez2 <- gfw_wa_eez %>%
  filter(year == 2017) %>%
  select(closest_eez_boundary_territory, flag_region, sum_fishing_hrs, rel_hrs) %>%
  mutate(sum_fishing_hrs = sum_fishing_hrs / 1000) %>%
  gather(indicator, value, sum_fishing_hrs:rel_hrs) %>%
  mutate(grp = paste(closest_eez_boundary_territory, flag_region),
         total_hrs = gfw_wa_eez$total_hrs[match(grp, gfw_wa_eez$grp)],
         indicator = ifelse(indicator == "sum_fishing_hrs", "Fishing hours", "Relative fishing hours"))

wa_factor_df <- gfw_wa_eez %>%
  filter(flag_region == "Other",
         year == 2017) %>%
  arrange(total_hrs)

gfw_wa_eez2$closest_eez_boundary_territory <- factor(gfw_wa_eez2$closest_eez_boundary_territory, levels = wa_factor_df$closest_eez_boundary_territory)  
  
  
fishing_2017 <- ggplot(gfw_wa_eez2, aes(x = closest_eez_boundary_territory, y = value, fill = flag_region)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey") +
  facet_wrap(~ indicator, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#5BBCD6", "#00A08A")) +
  xlab("EEZ") +
  ylab("Fishing hours (relative; thousands)") +
  guides(fill = guide_legend(title = "Flag region")) +
  ggtitle("Fishing hours within EEZs from GFW data: 2017") +
  theme_minimal()

ggsave(filename = paste0(pathstart, "outputs/figures/prelim/effort_in_wa_eez_2017.pdf"), fishing_2017, width = 11, height = 8.5, units = "in")

## Now look at inside vs outside
gfw_wa_all <- gfw_wa %>%
  mutate(flag_region = ifelse(flag %in% wa_df$iso3, "West Africa", "Other")) %>%
  group_by(closest_eez_boundary_territory, year, flag_region, inside_eez) %>%
  summarise(sum_fishing_hrs = sum(fishing_hrs)) %>%
  ungroup() %>%
  group_by(closest_eez_boundary_territory, inside_eez, year) %>%
  mutate(total_hrs = sum(sum_fishing_hrs)) %>%
  ungroup() %>%
  mutate(rel_hrs = sum_fishing_hrs / total_hrs)

## make figure with both relative and total hours
gfw_wa_out <- gfw_wa_all %>%
  filter(year == 2017, inside_eez == FALSE) %>%
  select(closest_eez_boundary_territory, flag_region, sum_fishing_hrs, rel_hrs) %>%
  mutate(sum_fishing_hrs = sum_fishing_hrs / 1000) %>%
  gather(indicator, value, sum_fishing_hrs:rel_hrs) %>%
  mutate(indicator = ifelse(indicator == "sum_fishing_hrs", "Fishing hours", "Relative fishing hours"),
         location = "Outside EEZ") 

gfw_wa_all2 <- gfw_wa_eez2 %>%
  select(closest_eez_boundary_territory, flag_region, indicator, value) %>%
  mutate(location = "Inside EEZ") %>%
  rbind(gfw_wa_out)

gfw_wa_all2$closest_eez_boundary_territory <- factor(gfw_wa_all2$closest_eez_boundary_territory, levels = wa_factor_df$closest_eez_boundary_territory)  

fishing_2017_all <- ggplot(gfw_wa_all2, aes(x = closest_eez_boundary_territory, y = value, fill = flag_region)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey") +
  facet_grid(indicator ~ location, scales = "free") +
  scale_fill_manual(values = c("#5BBCD6", "#00A08A")) +
  xlab("EEZ") +
  ylab("Fishing hours (relative; thousands)") +
  guides(fill = guide_legend(title = "Flag region")) +
  ggtitle("Fishing hours within and outside of African EEZs on west coast from GFW data: 2017") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        title = element_text(size = 14))

ggsave(filename = paste0(pathstart, "outputs/figures/prelim/effort_wa_2017.pdf"), fishing_2017_all, width = 11, height = 11, units = "in")



