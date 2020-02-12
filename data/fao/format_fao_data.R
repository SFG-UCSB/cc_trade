
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(countrycode)

# Directories
datadir <- "data/fao"
plotdir <- "figures"

# Load data
data_orig <- read.csv(file.path(datadir, "1950_2016_fao_global_capture_production.csv"), as.is=T, na.strings=c("..."))


# Format data
################################################################################

# Notes
# -	= zero 
# 0	= more than zero but less than half the unit used 
# F =	FAO estimate from available sources of information
# ... = data not available or unobtainable
# nei = not elsewhere included
# Quantity = tonnes (10^0)
# Value = USD in thousands (10^3)

# Format data
data_long <- data_orig %>% 
  rename(country=Country..Country., species=Species..ASFIS.species., 
         fao_area=Fishing.area..FAO.major.fishing.area., units=Unit..Unit.)

# Format data more
data_wide <- melt(data_long, id.vars=c("country", "species", "fao_area", "units"), variable.name="year", value.name="catch")

# Format data more
data <- data_wide %>% 
  mutate(year=as.numeric(gsub("X", "", year)),
         note=ifelse(grepl("F", catch), "estimate", NA),
         note=ifelse(catch=="0 0", "more than zero but small", note),
         note=ifelse(catch %in% c("0", "-"), "true zero", note),
         catch=gsub(" F", "", catch),
         catch=revalue(catch, c("-"="0",
                                "0 0"="0")),
         catch=as.numeric(catch),
         type=ifelse(grepl("Inland", fao_area), "inland", "marine"),
         units=tolower(units))

# Inspect marine landings over time
marine <- data %>% 
  filter(type=="marine" & units=="tonnes") %>% 
  group_by(year) %>% 
  summarize(catch=sum(catch, na.rm=T))

# Plot data
plot(catch/1E6 ~ year, marine, type="l", bty="n", las=2, 
     xlim=c(1950, 2020), ylim=c(0,100), xlab="", ylab="Catch (millions mt)")


# Export data
################################################################################

# Export data
write.csv(data, file.path(datadir, "1950_2016_fao_global_capture_production_formatted.csv"), row.names=F)


