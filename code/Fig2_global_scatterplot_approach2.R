

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
data <- readRDS(file.path(datadir, "gaines_global_level_results.Rds"))



# Plot data
################################################################################

# Format outliers
range(data$p_tot_rel)
range(data$c_tot_rel)
data <- data %>% 
  mutate(scenario_name=revalue(scenario, c("Full Adaptation"="Full\nAdaptation",
                                           "No Adaptation"="No\nAdaptation",
                                           "Range Shift Only"="Range\nShift Only",
                                           "Productivity Only"="Productivity\nOnly")))

# Params
rcps <- sort(unique(data$rcp))
rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))

# Proportion colors
range(data$b_prop_diff)
bins <- seq(-0.8,0.8,0.1)
bin_cols <- freeR::colorpal(brewer.pal(11, "RdBu"), length(bins)-1)
data$b_prop_col <- bin_cols[cut(data$b_prop_diff, breaks=bins)]
data$b_prop_col[data$scenario=="No Adaptation"] <- "grey80"

# Setup figure
figname <- "Fig2_global_scatterplots_approach2.png"
png(file.path(plotdir, figname), width=4.5, height=4.5, units="in", res=600)
par(mfrow=c(2,2), mar=c(2,1.5,1,0.5), oma=c(2,2.5,0,2), mgp=c(2,0.8,0))

# Loop through RCP-scenarios
i <- 1
for(i in 1:length(rcps)){
    
    # Subset data
    rcp1 <- rcps[i]
    sdata <- filter(data, rcp==rcp1)
    
    # Plot data
    plot(p_tot_rel ~ c_tot_rel, sdata, type="n", bty="n", las=1, cex.axis=0.7,
         pch=21, bg=b_prop_col, cex=scales::rescale(msy_tot2, to=c(4,4.5)),
         xpd=NA, main=rcp_names[i], cex.main=0.9, col=NA, 
         xlab="", ylab="", xlim=c(-20, 40), ylim=c(-20,120))

    # Add zero lines
    lines(x=c(0,0), y=c(-20,120), lty=3, col="grey50")
    lines(x=c(-20,40), y=c(0,0), lty=3, col="grey50")
    
    # Add points
    points(x=sdata$c_tot_rel, y=sdata$p_tot_rel, pch=21, bg=freeR::tcolor(sdata$b_prop_col, 0.8), cex=4, col=NA)
    
    # Label scenarios
    text(sdata$c_tot_rel, sdata$p_tot_rel, label=sdata$scenario_name, cex=0.6, font=2)
    
    # Add legend
    if(i==4){
      nbins <- length(bins)-1
      colors <- freeR::colorpal(brewer.pal(11, "RdBu"), nbins)
      colorbar.plot(x=20, y=-20, adj.x=0, adj.y=0, horiz=F, col=colors,
                    strip=seq(-40,40, length.out=nbins),
                    strip.width=0.1, strip.length=0.4, xpd=NA)
      text(x=20, y=41, pos=4, offset=0, labels=expression(bold("Change in % of\nstocks with B/B"["MSY"]*">1")), cex=0.6, xpd=NA)
      text(x=22, y=c(-78, mean(c(-78,-33)), -33), pos=4, offset=0, labels=c("-60", "0%", "60%"), cex=0.7)
    }
    
}


# Add axis labels
mtext("% difference in cumulative catch\n(compared to no adaptation scenario)", outer=T, side=1, line=0.7, cex=0.65)
mtext("% difference in cumulative profits\n(compared to no adaptation scenario)", outer=T, side=2, line=0.7, cex=0.65)

# Off
dev.off()





