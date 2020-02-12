

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
data <- readRDS(file.path(datadir, "gaines_country_stock_level_results.Rds"))


# Plot data
################################################################################

# Format outliers
csdata <- data %>% 
  ungroup() %>% 
  mutate(p_tot_rel_plot=pmin(300, p_tot_rel, na.rm=T),
         p_tot_rel_plot=pmax(-100, p_tot_rel_plot, na.rm=T),
         c_tot_rel_plot=pmin(300, c_tot_rel, na.rm=T)) %>% 
  filter(c_tot>0)
range(csdata$p_tot_rel_plot)
range(csdata$c_tot_rel_plot)
# summary(csdata$c_tot_rel[is.finite(csdata$c_tot_rel)], na.rm=T)
# hist(csdata$c_tot_rel[is.finite(csdata$c_tot_rel)], breaks=seq(-100,100000,10), xlim=c(0,1000))

# B/BMSY colors
hist(csdata$bbmsy_avg2, xlim=c(0,6), breaks=c(seq(0,20,0.1), 2E15))
bbmsy_bins <- c(seq(0,1.9,0.1), Inf)
csdata <- csdata %>% 
  mutate(bbmsy_bin=cut(bbmsy_avg2, breaks=bbmsy_bins),
         bbmsy_col=freeR::colorpal(brewer.pal(11, "RdBu"), nlevels(bbmsy_bin))[bbmsy_bin])

# Params
rcps <- sort(unique(csdata$rcp))
rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
scenarios <- c("Range Shift Only", "Productivity Only", "Full Adaptation")
cntrys <- sort(unique(csdata$sovereign))

# Setup figure
figname <- "AppendixA_country_stock_scatterplots.pdf"
pdf(file.path(plotdir, figname), width=8.5, height=11)
par(mfrow=c(4,3), mar=c(2,2,1,1), oma=c(4,4,4,2))

# Loop through countries
i <- j <- k <- 1
# for(i in 1:3){
for(i in 1:length(cntrys)){
  
  # Loop through RCPs
  for(j in 1:length(rcps)){
    
    # Loop through scenarios
    for(k in 1:length(scenarios)){
      
      # Subset data
      cntry <- cntrys[i]
      rcp1 <- rcps[j]
      scen <- scenarios[k]
      sdata <- csdata %>% 
        filter(sovereign==cntry & rcp==rcp1 & scenario==scen) %>% 
        arrange(desc(msy_tot2))
      
      # Plot data
      plot(p_tot_rel_plot ~ c_tot_rel_plot, sdata, bty="n", cex.axis=1, col="grey70",
           pch=21, main="", bg=freeR::tcolor(bbmsy_col,0.7), xpd=NA, yaxt="n", cex=scales::rescale(msy_tot2, to=c(1,4.5)),
           xlab="", ylab="", xlim=c(-100, 300), ylim=c(-100,300))
      axis(2, at=seq(-100,300,100), labels=c("-100", "0", "100", "200", "300"), cex.axis=1)
      if(j==1){title(scenarios[k], cex.main=1.1, line=0.3)}
      
      # Add labels
      top5 <- slice(sdata, 1)
      text(x=top5$c_tot_rel_plot, y=top5$p_tot_rel_plot, labels=gsub(" ", "\n", top5$species), font=2, xpd=NA)
      
      # Add zero lines
      lines(x=c(0,0), y=c(-100,300))
      lines(x=c(-100,300), y=c(0,0))
      
    }
    
  }
  
  # Title: Country (XXX species)
  n_spp <- n_distinct(sdata$species)
  title_text <- paste0(cntry, " (", n_spp, " stocks)")
  
  # Add axis labels
  mtext(title_text, outer=T, side=3, line=0.5, cex=0.9, font=2)
  mtext("% difference in cumulative catch\n(compared to no adaptation scenario)", outer=T, side=1, line=1.5, cex=0.75)
  mtext("% difference in cumulative profits\n(compared to no adaptation scenario)", outer=T, side=2, line=0.7, cex=0.75)
  
}

# Dev
dev.off()
