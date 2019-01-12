
# Read data
gaines_for_eez <- readRDS("data/gaines/gaines_data_for_eez_analysis.Rds")

# Project stuff
# iso3s <- "USA"; ref_year <- 2100
proj_fish <- function(iso3s, ref_year, plotdir, figname){
  
  # Calculate 2012 and final year totals
  ref_year <- 2100
  proj_tot <- gaines_for_eez %>% 
    filter(sovereign_iso3 %in% iso3s & year %in%c(2012, ref_year)) %>% 
    group_by(rcp, scenario, year) %>% 
    summarize(b_tot=sum(biomass_eez, na.rm=T),
              h_tot=sum(harvest_eez, na.rm=T),
              p_tot=sum(profit_eez, na.rm=T))
  
  # Initial year values
  tot2012 <- proj_tot %>%
    filter(rcp == "RCP26" & scenario == "No Adaptation" & year == 2012) 
  
  # Calculate proportional change: final year to 2012
  proj_tot1 <- proj_tot %>% 
    group_by(rcp, scenario) %>% 
    summarize(b_prop=(b_tot[year==ref_year] - tot2012$b_tot)/tot2012$b_tot,
              h_prop=(h_tot[year==ref_year] - tot2012$h_tot)/tot2012$h_tot,
              p_prop=(p_tot[year==ref_year] - tot2012$p_tot)/tot2012$p_tot) %>%
    ungroup() %>% 
    tidyr::gather(indicator, prop, b_prop:p_prop) %>% 
    mutate(indicator=revalue(indicator, c("b_prop"="Biomass", "h_prop"="Harvest", "p_prop"="Profit")),
           rcp=revalue(rcp, c("RCP26"="RCP 2.6", "RCP45"="RCP 4.5", "RCP60"="RCP 6.0", "RCP85"="RCP 8.5")),
           scenario=factor(scenario, levels=c("No Adaptation", "Range Shift Only", "Productivity Only", "Full Adaptation")))
  
  # Setup theme
  plottheme <- theme(axis.title = element_text(size = 22),
                     legend.text = element_text(size = 20),
                     legend.title = element_text(size = 20),
                     legend.key = element_blank(), 
                     plot.title = element_text(size = 22, hjust = 0.5),
                     axis.text = element_text(size = 18),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x = element_line(color = "black", size = 1),
                     axis.line.y = element_line(color = "black", size = 1),
                     strip.text.x = element_text(size = 18),
                     legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid"))
  
  # Plot horizontal barplot
  p <- ggplot(data = proj_tot1, aes(x = rcp, y = prop, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~indicator, ncol = 1) +
    coord_flip() +
    ylab("Difference: 2100 vs. Today") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(name = "Scenario",
                      # values = c("Full Adaptation" = "#009E73", "Productivity Only" = "#56B4E9", "Range Shift Only" = "#E69F00", "No Adaptation" = "#D55E00"),
                      values = c("Full Adaptation" = "#0072B2", "Productivity Only" = "#009E73", "Range Shift Only" = "#56B4E9", "No Adaptation" = "#D55E00"),
                      breaks = c("Full Adaptation", "Productivity Only", "Range Shift Only", "No Adaptation"),
                      labels = c("Full Adaptation", "Productivity Adaptation", "Range Shift Adaptation", "No Adaptation")) +
    plottheme +
    theme(axis.title.y=element_blank())
  
  # Save figure
  ggsave(p, filename=file.path(plotdir, paste0(figname, ".png")), dpi = 300, width = 11, height = 8.5, units = "in")
  
}
  
  
  
  
  
  
  
  
  