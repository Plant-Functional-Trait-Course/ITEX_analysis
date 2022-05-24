# Analysis and Figure plan

AnalysisPlan <- list(

  # diversity indices
  tar_target(
    name = CommResp,
    command = calc_comm_metrics(Community)
  ),


  # change in community metrics over time
  tar_target(
    name = Comm_Metric_Change,
    command = community_metric_change(Community, metaItex, CommResp)
  ),

  tar_target(
    name = Comm_t_Test,
    command = community_t_test(Comm_Metric_Change)
  ),

  tar_target(
    name = Comm_t_Test_Supp,
    command = community_t_test(Comm_Metric_Change)
  ),

  # Community ANOVA and tidy results
  tar_target(
    name = Comm_Anova,
    command = Comm_Metric_Change %>%
      group_by(response) %>%
      nest(data = -response) %>%
      mutate(
        aov = map(data, ~ aov(dist ~ Treatment*Site, data = .x)),
        aov_tidy = map(aov, tidy)
      )
  ),

  tar_target(
    name = Comm_Anova_tidy,
    command = Comm_Anova %>%
      select(response, aov_tidy) %>%
      unnest(aov_tidy)
  ),



  # NMDS ordination and results
  tar_target(
    name = NMDS_output,
    command = nmds_ordination(Community)
  ),

  tar_target(
    name = NMDS_result,
    command = NMDS_analysis(NMDS_output)
  ),


  # Canopy height
  tar_target(
    name = Height_result,
    command = height_analysis(Height)
  ),


  # Bootstrapping
  tar_target(
    name = Trait_Mean,
    command = make_bootstrapping(Community, Traits)
  ),

  # Trait PCA
  # make data wide
  tar_target(
    name = trait_pca,
    command = Trait_Mean %>%
      select(-mean_noitv) %>%
      mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("SLA", "LDMC", "Leaf Area", "Leaf Thickness", "%N", "%C", "%P", "C:N", "delta13C", "delta15N", "Dry Mass", "Plant Height"))) %>%
      pivot_wider(names_from = Trait, values_from = mean)
  ),

  # select traits
  tar_target(
    name = trait_pca_data,
    command = trait_pca %>%
      select("%C":"SLA")
  ),

  # meta data
  tar_target(
    name = trait_pca_info,
    command = trait_pca %>%
      select(Site:Treatment)
  ),

  # make pca
  tar_target(
    name = pca_res,
    command = prcomp(trait_pca_data, center = T, scale. = T)
  ),


  #PERMANOVA of PCA groups
  tar_target(
    name = pca_test,
    command = cbind(trait_pca_info, pca_res$x) %>%
      rename("Habitat" = "Site") %>%
      group_by(Habitat, Treatment) %>%
      select(-Site_trt, -Year, -Treatment, -PlotID)
  ),


  tar_target(
    name = perm,
    command = adonis(pca_test[c(3:14)] ~ pca_test$Treatment * pca_test$Habitat, method='eu')
  ),



  #### plot 3: mean trait values by plot ####
  # Anova for traits
  tar_target(
    name = Anova_Trait,
    command = Trait_Mean %>%
      group_by(Trait) %>%
      nest(data = -Trait) %>%
      mutate(
        aov = map(data, ~ aov(mean ~ Treatment*Site, data = .x)),
        aov_tidy = map(aov, tidy)
      )
  ),

  # tidy Anova results
  tar_target(
    name = Anova_Trait_Tidy,
    command = Anova_Trait %>%
      select(Trait, aov_tidy) %>%
      unnest(aov_tidy)
  ),


  #### ITV ####
  ## Intra vs. Inter
  tar_target(
    name = Var_Split_Exp,
    command = Intra_vs_Inter(Traits, Trait_Mean)
  ),

  tar_target(
    name = Var_Split,
    command = Intra_vs_Inter_var_split(Var_Split_Exp)
  ),


  #### Climate data ####
  # monthly temp
  tar_target(
    name = Monthly_Temp,
    command = calculate_temperature_means(Temperature)
  ),

  # daily climate
  tar_target(
    name = Daily_Climate,
    command = calculate_climate_means(Climate)
  ),

  # Max temp
  tar_target(
    name = Max_Temp,
    command = Monthly_Temp %>%
      filter(Variable == "Temperature") %>%
      group_by(year(YearMonth)) %>%
      summarise(max(Value))
  ),

  # Temperature range winter vs summer in control plots
  tar_target(
    name = Temp_Range,
    command = Monthly_Temp %>%
      filter(month(YearMonth) %in% c(1, 2, 3, 7),
             Treatment == "CTL") %>%
      mutate(time = case_when(month(YearMonth) %in% c(1, 2, 3) ~ "winter",
                              TRUE ~ "summer")) %>%
      group_by(LoggerLocation, time) %>%
      summarise(mean = mean(Value),
                se = sd(Value)/sqrt(n()),
                min = min(Value),
                max = max(Value),
                diff = max - min)
  ),

  # monthlyTemp %>%
  #   filter(month(YearMonth) == 1) %>%
  #   group_by(LoggerLocation, Site) %>%
  #   summarise(mean = mean(Value), min = min(Value), max = max(Value)) %>%
  #   mutate(diff = max - min)

  # Temperature difference winter vs summer between OTC and Controls
  tar_target(
    name = Temp_Diff,
    command = Monthly_Temp %>%
      filter(month(YearMonth) %in% c(1, 2, 3, 7)) %>%
      mutate(time = case_when(month(YearMonth) %in% c(1, 2, 3) ~ "winter",
                              TRUE ~ "summer")) %>%
      group_by(LoggerLocation, Treatment, time, Site) %>%
      summarise(mean = mean(Value)) %>%
      pivot_wider(names_from = Treatment, values_from = mean) %>%
      mutate(diff = OTC - CTL)
  ),

  # temp results
  tar_target(
    name = Temperature_Analyses_results,
    command = temperature_analysis(Monthly_Temp)
  )

)
