## Tracey Mangin
## January 22, 2019
## How much do prices vary for each species?

library(janitor)
library(tidyverse)

upsides <- read_csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv")

upsides0 <- upsides %>%
  filter(Year == 2012,
         IdLevel != "Neis")

## extract relevant info
input_df <- upsides0 %>%
  select(IdOrig, Country, SciName, SpeciesCat, RegionFAO, Dbase, CatchShare, Biomass, Profits, BvBmsy, FvFmsy, MSY, Price, g, k, phi) %>%
  clean_names(case = "snake") %>%
  rename(BvBmsy = bv_bmsy,
         FvFmsy = fv_fmsy)

## get more specific
prices_df <- input_df %>%
  select(id_orig, country, sci_name, price)

prices_df2 <- prices_df %>%
  arrange(sci_name) %>%
  group_by(sci_name) %>%
  mutate(avg_price = mean(price),
         n_stock = n()) %>%
  ungroup() %>%
  mutate(p_avg = ifelse(price == avg_price, 1, 0)) %>%
  group_by(sci_name) %>%
  mutate(n_avg = sum(p_avg)) %>%
  ungroup() %>%
  mutate(p_same = ifelse(n_avg == n_stock, 1, 0)) %>%
  ## filter for species that have different price values for stocks
  filter(p_same == 0)

## how much do the prices vary?
# price_fig <- ggplot(prices_df2, aes(x = sci_name, y = price)) +
#   geom_jitter(stat = "identity", position = "jitter") +
#   facet_wrap(~ sci_name, scales = "free_y")
  
  

  
