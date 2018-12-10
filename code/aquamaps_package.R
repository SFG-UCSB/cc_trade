

# AquaMaps packae
# devtools::install_github("raquamaps/raquamaps")  
# devtools::install_github("raquamaps/aquamapsdata")
# https://github.com/raquamaps/raquamaps
# https://github.com/raquamaps/aquamapsdata
library(aquamaps)
library(aquamapsdata)
library(purrr)

# Download Database for first time
# download_db(force = TRUE)

my_db <- aquamapsdata:::src_sqlite_aquamapsdata()

my_db %>% tbl("nativemaps")
my_db %>% tbl("hcaf")
my_db %>% tbl("hspen")
my_db %>% tbl("occ")
my_db %>% tbl("taxa")
