
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/gaines/raw"
plotdir <- "figures/paper"

# Load data
outcomes_orig <- readRDS(file.path(datadir, "global_cc_1nation_manuscript_2019Feb12.rds"))

# Build data
################################################################################

# Build initial condition dataset
data <- outcomes_orig %>% 
  filter(year==2012 & scenario=="No Adaptation" & cc_presence=="no_climate_change" & discount_rate==0) %>% 
  mutate(b_status=ifelse(BvBMSY<1, "overfished", "not overfished"),
         f_status=ifelse(FvFMSY>1, "overfishing", "no overfishing"), 
         status=paste(b_status, "and", f_status),
         status=ifelse(BvBMSY>=0.8 & BvBMSY<=1.2 & FvFMSY <= 1.2 & FvFMSY >= 0.8, "optimal", status)) 

table(data$status) / nrow(data)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  legend.text=element_text(size=8),
                  legend.title = element_text(size=10),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Rect coords
rect <- tibble(xmin=0.8, xmax=1.2, ymin=0.8, ymax=1.2)

# Plot data
g <- ggplot() +
  # Plot points
  geom_point(data, mapping=aes(x=BvBMSY, y=FvFMSY, size=MSY2012/1e6), color="darkgreen") +
  # Optimal management rectangele
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey80", alpha=0.6) +
  # Plot lines
  geom_hline(yintercept = 1, linetype="dotted", color="grey40") +
  geom_vline(xintercept = 1, linetype="dotted", color="grey40") +
  # Small things
  labs(x=expression("B/B"["MSY"]), y=expression("F/F"["MSY"]), title="2012 stock status") +
  scale_size_continuous(name="Maximum sustainable yield\n(MSY, millions of mt)") +
  theme_bw() + my_theme +
  theme(legend.position=c(0.7,0.8))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS1_initial_condition.tiff"), 
       width=4.5, height=4.5, units="in", dpi=600)





