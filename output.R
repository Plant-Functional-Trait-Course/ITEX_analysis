### Output
library(targets)
library(tarchetypes)
library(usethis)
library(tidyverse)
library(lubridate)
library(vegan)
library(ggvegan)
library(patchwork)
library(broom)
library(tagger)
library(glue)
library(Hmisc)
library(ggpattern)

tar_option_set(packages = c("dataDownloader", "tidyverse", "lubridate", "readxl", "traitstrap", "vegan", "ggvegan", "broom", "patchwork", "ape", "nlme", "lme4", "broom.mixed", "MuMIn", "ggpubr", "ggfortify", "ggpattern"))

## Main figures

# Fig 1 Species PCA
tar_load(Fig_1_Community_PCA)
ggsave("output/Fig_1_Community_PCA.png", Fig_1_Community_PCA, dpi = 600, height = 12, width = 10)


# Fig 2 Trait PCA
tar_load(Fig_2_Trait_PCA)
ggsave("output/Fig_2_Trait_PCA.png", Fig_2_Trait_PCA, dpi = 600, height = 12, width = 10)


# Fig 3 Specific - fixed mean
tar_load(Fig_3_specific_fixed)
ggsave("output/Fig_3_Specific_fixed.png", Fig_3_specific_fixed, dpi = 600, height = 7, width = 10)

# Fig 4 C-fluxes
tar_load(Fig_4_Fluxes)
ggsave("output/Fig_4_Fluxes.png", Fig_4_Fluxes, dpi = 600, height = 10, width = 10)


## Appendix

# Fig S1 Climate
tar_load(Fig_S1_ClimatePlot)
ggsave("output/Fig_S1_climate.png", Fig_S1_ClimatePlot, dpi = 600, height = 6, width = 8)

# Fig S2 Microclimate
tar_load(Fig_S2_Microclimate)
ggsave("output/Fig_S2_Microclimate.png", Fig_S2_Microclimate, dpi = 600, height = 4, width = 6)

# Fig S3 Canopy height
tar_load(Fig_S3_CanopyHeight)
ggsave("output/Fig_S3_CanopyHeight.png", Fig_S3_CanopyHeight, dpi = 600, height = 4, width = 5)

# Fig S4 Change in community metrics
tar_load(Fig_S4_metric_change)
ggsave("output/Fig_S4_Comm_metrics.png", Fig_S4_metric_change, dpi = 600, height = 8, width = 7)

# Fig S5 Trait mean
tar_load(Fig_S5_trait_mean)
ggsave("output/Fig_S5_trait_mean.png", Fig_S5_trait_mean, dpi = 600, height = 7, width = 10)

# Table S3
tar_load(Anova_Trait_Tidy)
Anova_Trait_Tidy |>
  mutate(Trait = recode(Trait,
                        "SLA_cm2_g" = "SLA",
                        "Leaf_Area_cm2" = "Leaf Area",
                        "Leaf_Thickness_mm" = "Leaf Thickness",
                        "N_percent" = "Leaf N",
                        "C_percent" = "Leaf C",
                        "P_Ave" = "Leaf P",
                        "CN_ratio" = "C:N",
                        "dC13_percent" = "d13C",
                        "dN15_percent" = "d15N",
                        "Dry_Mass_g" = "Dry Mass",
                        "Plant_Height_cm" = "Plant Height"),
         Trait = factor(Trait, levels = c("Plant Height", "Dry Mass", "Leaf Area", "Leaf Thickness", "SLA", "LDMC", "Leaf C", "Leaf N", "Leaf P", "C:N", "d13C", "d15N")),
         sumsq = round(sumsq, 2),
         meansq = round(meansq, 2),
         statistic = round(statistic, 2),
         p.value = round(p.value, 3)) |>
  arrange(Trait) %>%
  writexl::write_xlsx(., "output/Anova_Trait_Tidy.xlsx")

# Table S5
tar_load(itv_importance)
writexl::write_xlsx(itv_importance, "output/itv_importance.xlsx")

# Fig S6 Variance partitioning
tar_load(Fig_S6_varpart)
ggsave("output/Fig_S6_Varpart.png", Fig_S6_varpart, dpi = 600, height = 8, width = 8)

# Fig S7 Mean fluxes
tar_load(Fig_S7_Mean_fluxes)
ggsave("output/Fig_S7_mean_fluxes.png", Fig_S7_Mean_fluxes, dpi = 600, height = 8, width = 8)

# Fig S8 Effect size fluxes
tar_load(Fig_S8_Effect_size)
ggsave("output/Fig_S8_Effect_size.png", Fig_S8_Effect_size, dpi = 600, height = 10, width = 13)
