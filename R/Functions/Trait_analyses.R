#### TRAIT DATA ANALYSIS ####

#### TRAIT BOOTSTRAPPING ####

make_bootstrapping <- function(Community, Traits){

  #prepare community data
  comm <- Community %>%
    filter(FunctionalGroup != "lichen", FunctionalGroup != "moss", FunctionalGroup != "liverwort", FunctionalGroup != "fungi") %>%
    select(-FunctionalGroup) %>%
    filter(Taxon != "equisetum arvense", Taxon != "equisetum scirpoides") %>%
    filter(Year == 2015) %>%
    mutate(Site_trt = paste0(Site, Treatment))

  #prepare trait data
  trait <- Traits %>%
    select(Treatment, Site, PlotID, Taxon, Trait, Value) %>%
    filter(!is.na(Value)) %>%
    mutate(Site_trt = paste0(Site, Treatment)) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
    filter(Trait != "Wet_Mass_g")

  #prepare trait data without intraspecific variation
  trait.null <- Traits %>%
    select(Treatment, Site, PlotID, Taxon, Trait, Value) %>%
    filter(!is.na(Value)) %>%
    filter(Treatment == "CTL") %>%
    filter(Trait != "Wet_Mass_g") %>%
    group_by(Taxon, Trait) %>%
    summarise(Value = mean(as.numeric(Value), na.rm = T)) %>%
    right_join(trait, by = c("Taxon", "Trait")) %>%
    select(-Value.y, "Value" = Value.x) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

  #set seed for bootstrapping repeatability
  set.seed(2525)
  trait_imp <- trait_impute(comm = comm,
                            traits = trait,
                            scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                            global = F,
                            taxon_col = "Taxon",
                            trait_col = "Trait",
                            value_col = "Value",
                            abundance_col = "Abundance",
                            min_n_in_sample = 2
  )

  trait_imp_null <- trait_impute(comm = comm,
                                 traits = trait.null,
                                 scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                                 global = F,
                                 taxon_col = "Taxon",
                                 trait_col = "Trait",
                                 value_col = "Value",
                                 abundance_col = "Abundance",
                                 min_n_in_sample = 2
  )

  #check trait coverage
  trait_imp %>%
    #filter(Trait == "C_percent") %>%
    autoplot(.) +
    theme(axis.text.x = element_text(angle = 90))

  fortify(trait_imp) |>
    ungroup() |>
    #filter(Trait == "CN_ratio") |>
    complete(.id, level, Trait, fill = list(s = 0)) |>
    filter(level == "PlotID") |>
    group_by(Trait) |>
    summarise(q = quantile(s, prob = 0.25))

  # For the size related traits (dry mass, ...), 90 % (prob = 0.1) of the plots have at least 90 % coverage at the plot level. For nutrient traits, 75 % (prob = 0.25) have at least 76.2 % coverage.

  trait_imp_null %>%
    autoplot(.) +
    theme(axis.text.x = element_text(angle = 90))

  #do the bootstrapping
  CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
  CWM_notiv <- trait_np_bootstrap(trait_imp_null, nrep = 100, sample_size = 200)

  CWM_mean <- trait_summarise_boot_moments(CWM) %>%
    select(Site:mean)

  CWM_notiv_mean <- trait_summarise_boot_moments(CWM_notiv) %>%
    select(Site:mean) %>%
    rename("mean_noitv" = "mean")

  traitMean <- CWM_mean %>%
    left_join(CWM_notiv_mean) %>%
    select(-n) %>%
    mutate(Year = 2015)

  #prepare bootstrapped trait data for analyses
  traitMean <- traitMean %>% ungroup() %>%
    mutate(Trait = plyr::mapvalues(Trait, from = c("P_percent", "dC13_permil", "dN15_permil"), to = c("P_Ave", "dC13_percent", "dN15_percent"))) %>%
    mutate(Treatment = substr(Site_trt, 3, 6)) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

  return(traitMean)

}


make_trait_pca <- function(Trait_Mean, habitat){

  # make data wide
  trait_pca <- Trait_Mean %>%
    select(-mean_noitv) %>%
    pivot_wider(names_from = Trait, values_from = mean)

  # select traits
  trait_pca_data <- trait_pca %>%
    select("C_percent":"SLA_cm2_g")

  # meta data
  trait_pca_info <- trait_pca %>%
    select(Site:Treatment)

  # make pca
  res_pca <- rda(trait_pca_data, center = TRUE, scale = TRUE)

  if(habitat == "CH"){

    out <- bind_cols(trait_pca_info, fortify(res_pca, axes = 1:5) |>
                       filter(Score == "sites"))
    tr <- fortify(res_pca, axes = 1:5) |>
      filter(Score == "species")

  } else {

    out <- bind_cols(trait_pca_info, fortify(res_pca) |>
                       filter(Score == "sites"))
    tr <- fortify(res_pca) |>
      filter(Score == "species")
  }

  return(list(out, tr, res_pca))

}





#### Intraspecific vs. interspecific variation ####


Intra_vs_Inter <- function(Traits, Trait_Mean){

  #prepare trait data
  trait <- Traits %>%
    select(Treatment, Site, PlotID, Taxon, Trait, Value) %>%
    filter(!is.na(Value)) %>%
    mutate(Site_trt = paste0(Site, Treatment)) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
    filter(Trait != "Wet_Mass_g")

  var_res <- data.frame()

  for(i in unique(trait$Trait)){
    v <- varcomp(lme(Value~1, random=~1|Taxon, data=trait %>% filter(Trait == i), na.action = na.omit), 1)[c(1,2)]

    v$trait <- i

    v <- unlist(v)

    var_res <- bind_rows(var_res, v)

  }

  var_split <- Trait_Mean %>%
    group_by(Trait) %>%
    do(test = trait.flex.anova(~Site * Treatment, mean, mean_noitv, data = .))

  var_split_exp <- data.frame(RelSumSq.Turnover = 1000, RelSumSq.Intraspec. = 1000, RelSumSq.Covariation = 1000, RelSumSq.Total = 1000, trait = "E", level = "F")

  for(i in 1:nrow(var_split)){
    out <- as.data.frame(var_split$test[[i]][2])
    out$trait <- as.factor(rep(var_split[i,1], 5))
    out$level <- rownames(out)
    var_split_exp <- rbind(var_split_exp, out)
  }

  return(var_split_exp)

}


Intra_vs_Inter_var_split <- function(Var_Split_Exp){

   var_split <- fancy_trait_name_dictionary(Var_Split_Exp |>
  rename(Trait = trait)) |>
    mutate(level = trimws(level)) %>%
    filter(RelSumSq.Turnover < 999) %>%
    rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>%
    mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>%
    gather(key = variable, value = value, -Trait, -Trait_fancy, -level) %>%
    filter(variable == "Total") %>%
    filter(level != "Total") %>%
    mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>%
    mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid")))

  return(var_split)
}




importance_intra_vs_inter <- function(Trait_Mean){

  itv_importance <- Trait_Mean %>%
  mutate(itv_diff = mean-mean_noitv,
         Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
  group_by(Trait, Site) |>
  nest() |>
  mutate(fit = map(data, ~ lm(itv_diff ~ Treatment, data = .x)),
         tidy = map(fit, tidy)) |>
  unnest(tidy) |>
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
           Site = recode(Site, SB = "Snowbed", CH = "Cassiope heath", DH = "Dryas heath"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope heath", "Dryas heath"))) |>
    arrange(Trait, Site)


  return(itv_importance)

}



