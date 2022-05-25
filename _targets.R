#######################
### targets plan ###
#######################

### LOAD LIBRARIES
library("targets")
library("tarchetypes")
library(tidyverse)
# install.packages("remotes")
#remotes::install_github("Between-the-Fjords/dataDownloader")
tar_option_set(packages = c("dataDownloader", "tidyverse", "lubridate", "readxl", "traitstrap", "vegan", "ggvegan", "broom", "patchwork", "ape", "nlme", "lme4", "broom.mixed", "MuMIn", "ggpubr", "ggfortify", "ggpattern"))


#theme_set(theme_bw(base_size = 15))


### SOURCE FUNCTIONS
# Plans
source("R/DownloadAndImport.R")
source("R/AnalysisPlan.R")
source("R/Figure_Plan.R")
source("R/FluxPlan.R")

# Functions
source("R/Functions/Community_analyses.R")
source("R/Functions/CommunityFigures.R")
source("R/Functions/Trait_analyses.R")
source("R/Functions/TraitFigures.R")
source("R/Functions/inter_intra_anova.R")
source("R/Functions/Climate_analyses.R")
source("R/Functions/Flux_analyses.R")
source("R/Functions/Flux_figures.R")

# render ms
ManuscriptPlan <- list(

  #render manuscript
  tar_render(name = ms, path = "manuscript.Rmd")


)

### COMBINE TARGETS PLANS
combined_plan <- c(
  DataDownloadPlan,
  DataImportPlan,
  AnalysisPlan,
  FigurePlan,
  FluxPlan,
  ManuscriptPlan
)

#return combined plan
combined_plan
