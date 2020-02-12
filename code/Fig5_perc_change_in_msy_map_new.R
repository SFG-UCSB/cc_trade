

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(ggplot2)
library(rworldmap)
library(countrycode)
library(maptools)
library(sf)


# Directories
datadir <- "data/gaines"
plotdir <- "figures"
eezdir <- "data/World_EEZ_v8_20140228_LR"

# Load data
tmsy <- readRDS(file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))

# Load EEZ shapefile
eezs <- sf::st_read(dsn=eezdir, layer="World_EEZ_v8_2014")


# Build data
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
  filter(msy_perc <= 100 & msy_perc>=-100) #%>% 
  # Reshape
  # ungroup() %>%
  # select(eez_id, country, rcp, msy_perc) %>%
  # tidyr::spread(key=rcp, value=msy_perc)

# Plot distribution of MSY change percentages
hist(data$msy_perc, breaks=seq(-100,100,10))

# Add data to EEZ shapefile
eezs1 <- eezs %>%
  left_join(data, by=c("EEZ_ID"="eez_id")) %>% 
  filter(!is.na(rcp))

hist(eezs1$msy_perc)


p <- ggplot(filter(eezs1, rcp=="RCP85")) +
  geom_sf(aes(fill=msy_perc), col="white") + 
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       name="% change in MSY") +
  theme_bw()
p



# Plot
p <- ggplot(eezs1) +
  geom_sf(aes(fill=msy_perc), col="white") +
  facet_wrap(~ rcp, nrow=2) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       name="% change in MSY")
ggsave(plot=p, filename="map.png", path=plotdir, width=6.5, height=6.5, units="in", dpi=300)





p









# Stats for manuscript
################################################################################

rcp26 <- data %>% 
  filter(rcp=="RCP26") %>% 
  arrange(msy_perc)

rcp85 <- data %>% 
  filter(rcp=="RCP85") %>% 
  arrange(msy_perc) %>% 
  filter(msy_perc < -5)

rcp85wa <- data %>% 
  filter(rcp=="RCP85" & continent=="Africa") %>% 
  arrange(msy_perc) %>% 
  filter(msy_perc < -85) %>% 
  filter(!sovereign%in%c("Sudan", "Eritrea", "Djibouti"))

winners <- data %>% 
  group_by(continent, sovereign, sovereign_iso3) %>% 
  summarize(nwin=sum(msy_perc>0),
            perc_avg=mean(msy_perc)) %>% 
  arrange(desc(nwin), desc(perc_avg))

rcp2685win <- data %>% 
  filter(rcp%in%c("RCP26", "RCP85") & sovereign%in%c("Finland", "Canada", "Antarctica")) %>% 
  arrange(rcp, msy_perc)

maur <- data %>% 
  filter(sovereign%in%c("Mauritania")) %>% 
  arrange(rcp, msy_perc)



# Plot data
################################################################################

# Range percent difference
range(data$msy_perc)

# Params
rcps <- sort(unique(data$rcp))
rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))

# Setup figure
lw <- 0.07
figname <- "Fig5_perc_change_in_msy_map.png"
png(file.path(plotdir, figname), width=6.5, height=3.1, units="in", res=600)
layout(matrix(data=c(1,2,5,
                     3,4,6), ncol=3, byrow=T), widths=c((1-lw)/2, (1-lw)/2, lw))
par(mar=c(0,0,0,0), oma=c(0,0,0.5,0), mgp=c(2,0.8,0))
# par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(0,0,0.3,0), mgp=c(2,0.8,0))

# Loop through RCPs
i <- 1
# for(i in 1){
for(i in 1:length(rcps)){
    
    # Subset data
    rcp1 <- rcps[i]
    sdata <- data %>% 
      ungroup() %>% 
      filter(rcp==rcp1)
    
    # Build spatial sample size data
    data(wrld_simpl) # from maptools
    world_orig <- wrld_simpl@data
    world <- world_orig %>% 
      mutate(ISO3=as.character(ISO3)) %>% 
      left_join(sdata, by=c("ISO3"="sovereign_iso3")) %>% 
      mutate(perc_bin=cut(msy_perc, breaks=seq(-100,100,10)),
             perc_col=freeR::colorpal(brewer.pal(11, "RdBu"), nlevels(perc_bin))[perc_bin],
             perc_col=ifelse(is.na(perc_col), "grey80", perc_col),
             perc_col=ifelse(msy_perc==0 & !is.na(msy_perc), "white", perc_col))
    wrld_simpl@data <- world
    
    # Plot data
    plot(wrld_simpl, col=wrld_simpl$perc_col, border="grey30", lwd=0.3)
    title(rcp_names[i], line=-0.2, xpd=NA, cex.main=1)
    
    # Add equator
    lines(x=c(-180,180), y=c(0,0), lty=3, lwd=0.6, col="grey10")
   
  
}

# Add legend
nbins <- length(seq(-100,100,10)-1)
colors <- freeR::colorpal(brewer.pal(11, "RdBu"), nbins)
plot(1:10, 1:10, bty="n", type="n", xaxt="n", yaxt="n", xlab="", ylab="")
colorbar.plot(x=0, y=2.3, adj.x=0, adj.y=0, horiz=F, col=colors,
              strip=seq(-100,100, length.out=nbins),
              strip.width=0.3, strip.length=2.4, xpd=NA)
text(x=2.2, y=c(2.45, mean(c(2.45,9)), 9), pos=4, labels=c("-100%", "0%", "100%"), cex=0.7)
text(x=-0.5, y=9.7, pos=4, labels="% change\nin MSY", font=2, cex=0.75, xpd=NA)

legend(x=-0.3, y=2, bty="n", fill="grey80", border=NA, legend="No data", cex=0.7, xpd=NA)

# Off
dev.off()



