### Output
library(targets)
library(tarchetypes)
library(tidyverse)
library(vegan)
library(ggvegan)
library(patchwork)

tar_option_set(packages = c("dataDownloader", "tidyverse", "lubridate", "readxl", "traitstrap", "vegan", "ggvegan", "broom", "patchwork", "ape", "nlme", "lme4", "broom.mixed", "MuMIn", "ggpubr", "ggfortify", "ggpattern"))

## Main figures

# Fig 1 NMDS
tar_load(Fig_1_CommunityOrdination)
ggsave("output/Fig_1_NMDS.png", Fig_1_CommunityOrdination, dpi = 300, height = 2.5, width = 6)


## Appendix

# Fig S2 PCA
tar_load(Fig_2_pca_plot)
ggsave("output/Fig_S2_pca.png", Fig_2_pca_plot, dpi = 300, height = 7.3, width = 6)

# Fig S3 Change in community metrics
tar_load(Fig_S3_metric_change)
ggsave("output/Fig_S3_comm_metrics.png", Fig_S3_metric_change, dpi = 300, height = 8, width = 6)

# Fig S4 Canopy height
tar_load(Fig_S4_CanopyHeight)
ggsave("output/Fig_S4_CanopyHeight.png", Fig_S4_CanopyHeight, dpi = 300, height = 4, width = 5)


