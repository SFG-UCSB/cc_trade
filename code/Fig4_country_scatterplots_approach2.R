

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
data <- readRDS(file.path(datadir, "gaines_country_level_results.Rds"))


# Plot data
################################################################################

# Format outliers
cdata <- data %>% 
  ungroup() %>% 
  mutate(p_tot_rel_plot=pmin(300, p_tot_rel, na.rm=T),
         p_tot_rel_plot=pmax(-100, p_tot_rel_plot, na.rm=T))
range(cdata$p_tot_rel_plot)

# Params
rcps <- sort(unique(cdata$rcp))
rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
scenarios <- c("Range Shift Only", "Productivity Only", "Full Adaptation")
scenario_names <- c("Range Shift Only", "Productivity Shift Only", "Full Adaptation")


# Proportion colors
range(cdata$b_prop_diff)
bins <- seq(-0.8,0.8,0.1)
bin_cols <- freeR::colorpal(brewer.pal(11, "RdBu"), length(bins)-1)
cdata$b_prop_col <- bin_cols[cut(cdata$b_prop_diff, breaks=bins)]

# Setup figure
figname <- "Fig4_country_scatterplots_approach2.png"
png(file.path(plotdir, figname), width=6.5, height=5, units="in", res=600)
par(mfrow=c(3,4), mar=c(1,1,1,1), oma=c(3,3.8,1,0), mgp=c(2,0.8,0))

# Loop through RCP-scenarios
i <- j <- 1
for(j in 1:length(scenarios)){
  for(i in 1:length(rcps)){
    
    # Subset data
    rcp1 <- rcps[i]
    scen <- scenarios[j]
    scen_name <- scenario_names[j]
    sdata <- filter(cdata, scenario==scen & rcp==rcp1 & !is.na(c_tot_rel))
    
    # Plot data
    plot(p_tot_rel_plot ~ c_tot_rel, sdata, bty="n", las=1, cex.axis=0.8,
         pch=21, bg=b_prop_col, cex=scales::rescale(msy_tot2, to=c(1,4.5)),
         main="", xpd=NA, xaxt="n", yaxt="n",
         xlab="", ylab="", xlim=c(-50, 100), ylim=c(-100,300))
    axis(1, at=seq(-50,100,50), labels=j==3, cex.axis=0.8, las=1)
    if(i==1){
      axis(2, at=seq(-100,300,100), labels=c("≤-100", "0", "100", "200", "≥300"), cex.axis=0.8, las=1)
    }else{
      axis(2, at=seq(-100,300,100), labels=F, cex.axis=0.8, las=1)
    }
    if(j==1){title(rcp_names[i], cex.main=1, line=1.2, xpd=NA)}
    
    # Add zero lines
    lines(x=c(0,0), y=c(-100,300))
    lines(x=c(-50,100), y=c(0,0))
    
    # Add quadrant percents
    # Top-left, clockwise
    n1 <- sum(sdata$p_tot_rel>0 & sdata$c_tot_rel<0) / nrow(sdata) * 100
    n2 <- sum(sdata$p_tot_rel>0 & sdata$c_tot_rel>0) / nrow(sdata) * 100
    n3 <- sum(sdata$p_tot_rel<0 & sdata$c_tot_rel>0) / nrow(sdata) * 100
    n4 <- sum(sdata$p_tot_rel<0 & sdata$c_tot_rel<0) / nrow(sdata) * 100
    text(x=-50, y=290, pos=4, offset=0, labels=paste0(round(n1),"%"), col="grey20", cex=0.75)
    text(x=100, y=290, pos=2, offset=0, labels=paste0(round(n2),"%"), col="grey20", cex=0.75)
    text(x=100, y=-90, pos=2, offset=0, labels=paste0(round(n3),"%"), col="grey20", cex=0.75)
    text(x=-50, y=-90, pos=4, offset=0, labels=paste0(round(n4),"%"), col="grey20", cex=0.75)
    
    # Add scenario label
    if(i==1){
      text(x=-60, y=340, pos=4, labels=scen_name, font=2, cex=0.9, xpd=NA)
    }
    
  }
}

# Add axis labels
mtext("Percent difference in cumulative catch\n(compared to no adaptation scenario)", outer=T, side=1, line=1.8, cex=0.65)
mtext("Percent difference in cumulative profits\n(compared to no adaptation scenario)", outer=T, side=2, line=1.8, cex=0.65)

# Off
dev.off()





