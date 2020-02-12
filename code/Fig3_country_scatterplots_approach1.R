

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

# Directories
datadir <- "data/gaines"
plotdir <- "figures"

# Load data
cdata_orig <- readRDS(file.path(datadir, "gaines_country_level_results_approach1.Rds"))


# Plot data
################################################################################

# Postive outcomes under no adaptation
pos_no_adapt <- cdata %>% 
  filter(scenario=="No Adaptation" & c_pdiff>0 & p_pdiff>0)


# Plot data
################################################################################

# Format results
cdata <- cdata_orig %>% 
  ungroup() %>% 
  mutate(rcp_label=revalue(rcp, c("RCP26"="RCP 2.6", 
                                  "RCP45"="RCP 4.5", 
                                  "RCP60"="RCP 6.0", 
                                  "RCP85"="RCP 8.5")),
         scenario=factor(scenario, levels=c("No Adaptation", "Range Shift Only", "Imperfect Productivity Only",
                                            "Productivity Only", "Imperfect Full Adaptation", "Full Adaptation")),
         scenario_label=revalue(scenario, c("No Adaptation"="No Adaptation",
                                            "Range Shift Only"="Range Shift",
                                            "Imperfect Productivity Only"="Imperfect Productivity",
                                            "Productivity Only"="Productivity",
                                            "Imperfect Full Adaptation"="Imperfect Full",
                                            "Full Adaptation"="Full Adaptation")))

# Plot results
p <- ggplot(cdata, aes(x=c_pdiff, y=p_pdiff, fill=bbmsy_avg2, size=msy_tot1/1000)) +
  geom_point(pch=21) +
  facet_grid(scenario_label ~ rcp_label) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(0,2),
                       breaks=seq(0,2,0.25),
                       labels=seq(0,2,0.25),
                       name="Mean B/BMSY/nin 2100") +
  scale_size_continuous(name="MSY (1000s mt) in 2100") +
  theme_bw()
p
ggsave(file.path(plotdir, "Fig3_country_scatterplots_approach1.png"), width=6.5, height=5, units="in", dpi=600, scale=1.3)



# Plot data (base plot)
################################################################################

# # Format outliers
# range(cdata$c_pdiff, na.rm=T)
# range(cdata$p_pdiff, na.rm=T)
# 
# # Params
# rcps <- sort(unique(cdata$rcp))
# rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
# scenarios <- c("No Adaptation", "Range Shift Only", "Productivity Only", "Full Adaptation")
# scenario_names <- c("No Adaptation", "Range Shift Only", "Productivity Shift Only", "Full Adaptation")
# 
# # # Proportion colors
# # range(cdata$bbmsy_avg2)
# # bins <- seq(0,2,0.2)
# # bin_cols <- freeR::colorpal(brewer.pal(11, "RdBu"), length(bins)-1)
# # cdata$bbmsy_col <- bin_cols[cut(cdata$bbmsy_avg2, breaks=bins)]
# 
# # Proportion colors
# range(cdata$bbmsy_prop2)
# bins <- seq(0,1,0.1)
# bin_cols <- freeR::colorpal(brewer.pal(9, "RdBu"), length(bins)-1)
# cdata$bbmsy_col <- bin_cols[cut(cdata$bbmsy_prop2, breaks=bins)]
# 
# # Setup figure
# figname <- "Fig3_country_scatterplots_approach1.png"
# png(file.path(plotdir, figname), width=6.5, height=6.5, units="in", res=600)
# par(mfrow=c(4,4), mar=c(1,1,1,1), oma=c(3,3.5,1,0), mgp=c(2,0.8,0))
# 
# # Loop through RCP-scenarios
# i <- j <- 1
# for(j in 1:length(scenarios)){
#   for(i in 1:length(rcps)){
#     
#     # Subset data
#     rcp1 <- rcps[i]
#     scen <- scenarios[j]
#     scen_name <- scenario_names[j]
#     sdata <- filter(cdata, scenario==scen & rcp==rcp1 & !is.na(c_pdiff))
#     
#     # Plot data
#     plot(p_pdiff ~ c_pdiff, sdata, bty="n", las=1, cex.axis=0.8,
#          pch=21, bg=bbmsy_col, cex=scales::rescale(msy_tot2, to=c(1,4.5)),
#          main="", xpd=NA, xaxt="n", yaxt="n",
#          xlab="", ylab="", xlim=c(-100, 100), ylim=c(-250,250))
#     axis(1, at=seq(-100,100,50), labels=j==4, cex.axis=0.8, las=1)
#     axis(2, at=seq(-250,250,50), labels=i==1, cex.axis=0.8, las=1)
#     if(j==1){title(rcp_names[i], cex.main=1, line=1.2, xpd=NA)}
#     
#     # Add zero lines
#     lines(x=c(0,0), y=c(-250,250))
#     lines(x=c(-100,100), y=c(0,0))
#     
#     # Add quadrant percents
#     # Top-left, clockwise
#     n1 <- sum(sdata$p_pdiff>0 & sdata$c_pdiff<0) / nrow(sdata) * 100
#     n2 <- sum(sdata$p_pdiff>0 & sdata$c_pdiff>0) / nrow(sdata) * 100
#     n3 <- sum(sdata$p_pdiff<0 & sdata$c_pdiff>0) / nrow(sdata) * 100
#     n4 <- sum(sdata$p_pdiff<0 & sdata$c_pdiff<0) / nrow(sdata) * 100
#     text(x=-100, y=240, pos=4, offset=0, labels=paste0(round(n1),"%"), col="grey20", cex=0.75)
#     text(x=100, y=240, pos=2, offset=0, labels=paste0(round(n2),"%"), col="grey20", cex=0.75)
#     text(x=100, y=-240, pos=2, offset=0, labels=paste0(round(n3),"%"), col="grey20", cex=0.75)
#     text(x=-100, y=-240, pos=4, offset=0, labels=paste0(round(n4),"%"), col="grey20", cex=0.75)
#     
#     # Add scenario label
#     if(i==1){
#       text(x=-110, y=290, pos=4, labels=scen_name, font=2, cex=0.9, xpd=NA)
#     }
#     
#   }
# }
# 
# # Add axis labels
# mtext("Percent difference in catch\n(2100 vs today)", outer=T, side=1, line=1.8, cex=0.65)
# mtext("Percent difference in profits\n(2100 vs today)", outer=T, side=2, line=1.4, cex=0.65)
# 
# # Off
# dev.off()
