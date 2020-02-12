

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

# Directories
datadir <- "data/gaines"
plotdir <- "figures"

# Load data
load(file.path(datadir, "gaines_country_level_catch_by_shift_type.Rdata"))

# Packages
map.world <- map_data(map="world")

# Add ISO3 to world map
world <- map_data(map="world")
world <- world %>% 
  mutate(iso3=countrycode(region, "country.name", "iso3c"))


# Build data
################################################################################

# Calculate proportion of 2012 catch coming from "leaving" stocks
data <- results %>% 
  filter(year==2012 & type!="never") %>% 
  group_by(rcp, sovereign_iso3, sovereign, scenario) %>%
  summarize(c_tot=sum(catch),
            c_shift=ifelse("leaving" %in% type, catch[type=="leaving"], 0),
            c_perc=c_shift/c_tot*100)


# Plot data
################################################################################

# Params
rcps <- sort(unique(data$rcp))
rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
scenarios <- c("No Adaptation", "Full Adaptation")

# Setup figure
figname <- "Fig6_initial_catch_shift_map.png"
png(file.path(plotdir, figname), width=6.5, height=6.5, units="in", res=600)
par(mfrow=c(4,2), mar=c(0,0,0,0), oma=c(1,1,2,2), mgp=c(2,0.8,0))

# Loop through RCPs
i <- 4; j <- 1
# for(i in 1){
for(i in 1:length(rcps)){
  
  # Loop through scenarios
  for(j in 1:length(scenarios)){
    
    # Subset data
    rcp1 <- rcps[i]
    scen <- scenarios[j]
    sdata <- data %>% 
      ungroup() %>% 
      filter(rcp==rcp1 & scenario==scen)
    
    # Build spatial sample size data
    data(wrld_simpl) # from maptools
    world_orig <- wrld_simpl@data
    world <- world_orig %>% 
      mutate(ISO3=as.character(ISO3)) %>% 
      left_join(sdata, by=c("ISO3"="sovereign_iso3")) %>% 
      mutate(perc_bin=cut(c_perc, breaks=seq(0,100,10)),
             perc_col=freeR::colorpal(brewer.pal(9, "Reds"),10)[perc_bin],
             perc_col=ifelse(is.na(perc_col), "grey80", perc_col),
             perc_col=ifelse(c_perc==0 & !is.na(c_perc), "white", perc_col))
    wrld_simpl@data <- world
    
    # Plot data
    plot(wrld_simpl, col=wrld_simpl$perc_col, border="grey30", lwd=0.3)
    if(i==1){title(scen, line=0.5, xpd=NA, cex.main=1.4)}
    if(j==1){mtext(rcp_names[i], side=2, adj=0.5, font=2, cex=1, line=-0.5)}
    
    # Add equator
    lines(x=c(-180,180), y=c(0,0), lty=3, lwd=0.6, col="grey10")
    
    # Add legend
    labels <- rev(paste0(seq(0,90,10), "-", seq(10,100,10), "%"))
    if(i==1 & j==2){
      legend(x=-180, y=45, fill=c(rev(freeR::colorpal(brewer.pal(9, "Reds"),10)), "white", "grey30"), 
             legend=c(labels, "0%", "no data"), cex=0.6, xpd=NA, title="% of catch shifting", border=NA, lty=0.5)
    } 
    
  }
  
}

# Off
dev.off()


# GGPLOT2 method
################################################################################

# # Add data to world map
# s_world <- world %>% 
#   left_join(select(sdata, sovereign_iso3, c_perc), by=c("iso3"="sovereign_iso3"))
# 
# # Plot data
# p <- (ggplot()
#       + theme_bw()
#       # + theme(legend.position="none")
#       + geom_map(data=s_world, map=s_world, aes(map_id=region, fill=c_perc), color="grey50", size=0.05)
#       + scale_fill_gradientn("% of 2012 catch\nfrom leaving species", 
#                              colours=brewer.pal(9, "Reds"),
#                              na.value="grey80", breaks=seq(0,100,20), labels=seq(0,100,20)) 
#       + coord_equal() + xlim(-180,180) + ylim(-90,90))
# p


# Plot data
################################################################################

# # Params
# rcps <- sort(unique(data$rcp))
# rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
# scenarios <- c("No Adaptation", "Range Shift Only", "Productivity Only", "Full Adaptation")
# 
# # Setup figure
# figname <- "AppendixC_initial_catch_shift_map.pdf"
# pdf(file.path(plotdir, figname), width=11, height=8.5)
# par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(2,1,4,2), mgp=c(2,0.8,0))
# 
# # Loop through RCPs
# i <- 4; j <- 1
# # for(i in 1){
# for(i in 1:length(rcps)){
#   
#   # Loop through scenarios
#   for(j in 1:length(scenarios)){
#     
#     # Subset data
#     rcp1 <- rcps[i]
#     scen <- scenarios[j]
#     sdata <- data %>% 
#       ungroup() %>% 
#       filter(rcp==rcp1 & scenario==scen)
#     
#     # Build spatial sample size data
#     data(wrld_simpl) # from maptools
#     world_orig <- wrld_simpl@data
#     world <- world_orig %>% 
#       mutate(ISO3=as.character(ISO3)) %>% 
#       left_join(sdata, by=c("ISO3"="sovereign_iso3")) %>% 
#       mutate(perc_bin=cut(c_perc, breaks=seq(0,100,10)),
#              perc_col=brewer.pal(9, "Reds")[perc_bin],
#              perc_col=ifelse(is.na(perc_col), "grey80", perc_col),
#              perc_col=ifelse(c_perc==0 & !is.na(c_perc), "white", perc_col))
#     wrld_simpl@data <- world
#     
#     
#     # Plot data
#     plot(wrld_simpl, col=wrld_simpl$perc_col, border="grey30", lwd=0.3)
#     title(scen, line=-3, xpd=NA)
#     
#   }
#   
#   # Add title
#   mtext(rcp_names[i], outer=T, side=3, adj=0.5, line=-1, cex=1.5, xpd=NA, font=2)
#   
# }
# 
# 
# 
# # Off
# dev.off()


