### Output
library(targets)
library(tarchetypes)
library(tidyverse)
library(lubridate)
library(vegan)
library(ggvegan)
library(patchwork)
library(broom)

tar_option_set(packages = c("dataDownloader", "tidyverse", "lubridate", "readxl", "traitstrap", "vegan", "ggvegan", "broom", "patchwork", "ape", "nlme", "lme4", "broom.mixed", "MuMIn", "ggpubr", "ggfortify", "ggpattern"))

## Main figures

# Fig 1 NMDS
tar_load(Fig_1_CommunityOrdination)
ggsave("output/Fig_1_NMDS.png", Fig_1_CommunityOrdination, dpi = 300, height = 2.5, width = 6)

# Fig 2 Trait mean
tar_load(Fig_2_trait_mean)
ggsave("output/Fig_2_trait_mean.png", Fig_2_trait_mean, dpi = 300, height = 6, width = 8)

# Fig 3 Specific - fixed mean
tar_load(Fig_3_specific_fixed)
ggsave("output/Fig_3_specific_fixed.png", Fig_3_specific_fixed, dpi = 300, height = 6, width = 8)

# Fig 4 C-fluxes
tar_load(Fig_4_Fluxes)
ggsave("output/Fig_4_Fluxes.png", Fig_4_Fluxes, dpi = 300, height = 6, width = 15)
ggsave("output/Fig_4_Fluxes.png", Fig_4_Fluxes, height = 8, width = 15)

## Appendix

# Fig S1 Climate
tar_load(Fig_S1_ClimatePlot)
ggsave("output/Fig_S1_climate.png", Fig_S1_ClimatePlot, dpi = 300, height = 6, width = 8)

# Fig S2 Microclimate
tar_load(Fig_S2_Microclimate)
ggsave("output/Fig_S2_micrclimate.png", Fig_S2_Microclimate, dpi = 300, height = 3, width = 6)

# Fig S3 Change in community metrics
tar_load(Fig_S3_metric_change)
ggsave("output/Fig_S3_comm_metrics.png", Fig_S3_metric_change, dpi = 300, height = 8, width = 6)

# Fig S4 Canopy height
tar_load(Fig_S4_CanopyHeight)
ggsave("output/Fig_S4_CanopyHeight.png", Fig_S4_CanopyHeight, dpi = 300, height = 4, width = 5)

# Fig S5 PCA
tar_load(Fig_5_pca_plot)
ggsave("output/Fig_S5_pca.png", Fig_5_pca_plot, dpi = 300, height = 7.3, width = 6)

# Fig S7 Mean fluxes
tar_load(Fig_S7_Mean_fluxes)
ggsave("output/Fig_S7_mean_fluxes.jpg", Fig_S7_Mean_fluxes, height = 3.5, width = 10)

# Fig S8 Effect size fluxes
tar_load(Fig_S8_Effect_size)
ggsave("output/Fig_S8_Effect_size.jpg", Fig_S8_Effect_size, height = 10, width = 13)