

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
load(file.path(datadir, "gaines_country_level_catch_by_shift_type.Rdata"))


# Plot data
################################################################################

# Format results
results <- results %>% 
  ungroup() %>% 
  filter(sovereign_iso3!="Other" & type!="never" & scenario=="No Adaptation") %>% 
  mutate(catch=catch/1000,
         msy=msy/1000,
         type=factor(type, levels=c("constant", "leaving", "entering")))

# Params
rcps <- sort(unique(results$rcp))
rcp_names <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
cntrys <- sort(unique(results$sovereign))

# Setup figure
figname <- "AppendixC_country_msy_by_stock_type.pdf"
pdf(file.path(plotdir, figname), width=8.5, height=11)
par(mfrow=c(4,4), mar=c(2,2,3,1), oma=c(4,4,4,2), mgp=c(2,0.8,0))

# Loop through countries
i <- j <- 1
# for(i in 1:8){
for(i in 1:length(cntrys)){
  
  # Country data and ymax
  cntry <- cntrys[i]
  cdata <- results %>% 
    filter(sovereign==cntry)
  cmax <- max(cdata$msy, na.rm=T)
  ybin <- ifelse(cmax<100, 20, 50)
  ymax <- freeR::ceiling1(cmax, ybin)
  
  # Loop through RCPs
  for(j in 1:length(rcps)){
      
    # Subset data
    rcp1 <- rcps[j]
    sdata <- results %>% 
      filter(sovereign==cntry & rcp==rcp1) %>% 
      arrange(year, type)
    
    # Reshape to wide
    sdata_wide <- sdata %>%
      select(year, type, msy) %>% 
      tidyr::spread(key=type, value=msy, drop=F, fill=0)
    sdata_wide_mat <- t(as.matrix(select(sdata_wide, -year)))
    
    # Barplot
    yrs <- 2012:2100
    yrs_label <- c(2012, seq(2020,2100,10))
    y_labels <- ifelse(yrs%in%yrs_label, yrs, NA)
    bp <- barplot(sdata_wide_mat, col=c("grey70", "red", "blue"), border=F, las=2, names.arg=y_labels, ylim=c(0, ymax))
    if(i%in%seq(1,200,4)){title(rcp_names[j], cex.main=1.2, line=1.8)}
    if(j==1){title(cntry, cex.main=1.1, line=0.5, adj=0.05)}

    # Print number of shifting species
    skey <- filter(key, sovereign==cntry & rcp==rcp1 & type!="never")
    n_spp <- nrow(skey)
    n_leave <- sum(skey$type=="leaving")
    c_tot_t0 <- sum(sdata_wide_mat[,1])
    c_shift_t0 <- sdata_wide_mat[2,1]
    p_shift_t0 <- c_shift_t0/c_tot_t0*100
    shift_text <- paste0(n_leave, " of ", n_spp, " leaving\n", freeR::roundf(p_shift_t0,1), "% of MSY in 2012")
    text(x=bp[length(2012:2100)], y=ymax-ymax*0.075, pos=2, labels=shift_text , cex=0.8, col="red", xpd=NA)
    
  }

  # Add axis labels
  mtext("MSY (1000s of metric tons)", outer=T, side=2, line=0.9, cex=0.9)
  
}

# Dev
dev.off()



