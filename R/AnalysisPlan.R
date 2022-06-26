# Analysis and Figure plan

AnalysisPlan <- list(


  # NMDS ordination and results
  tar_target(
    name = NMDS_output,
    command = nmds_ordination(Community)
  ),

  tar_target(
    name = NMDS_result,
    command = NMDS_analysis(NMDS_output)
  ),


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

  # tar_target(
  #   name = Comm_t_Test_Supp,
  #   command = community_t_test(Comm_Metric_Change)
  # ),

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
  tar_target(
    name = trait_pca_all,
    command = make_trait_pca(Trait_Mean)

  ),

  #PERMANOVA of PCA groups

  tar_target(
    name = perm,
    command = {

      pca_test <- cbind(trait_pca_all[[3]], trait_pca_all[[2]]) %>%
        rename("Habitat" = "Site") %>%
        group_by(Habitat, Treatment) %>%
        select(-Site_trt, -Year, -Treatment, -PlotID)

      adonis(pca_test[c(3:14)] ~ pca_test$Treatment * pca_test$Habitat, method='eu')

    }
  ),

  ### PCA per habitat
  # Dryas
  tar_target(
    name = trait_pca_DH,
    command = make_trait_pca(Trait_Mean |>
                               filter(Site == "DH"))

  ),

  tar_target(
    name = trait_pca_DH_test,
    command = {

      pca_test <- cbind(trait_pca_DH[[3]], trait_pca_DH[[2]]) %>%
        group_by(Treatment) %>%
        select(-Site_trt, -Year, -Site, -Treatment, -PlotID)

      adonis(pca_test[c(2:11)] ~ pca_test$Treatment, method='eu')

    }

  ),


  # Cassiope
  tar_target(
    name = trait_pca_CH,
    command = make_trait_pca(Trait_Mean |>
                               filter(Site == "CH"))

  ),

  tar_target(
    name = trait_pca_CH_test,
    command = {

      pca_test <- cbind(trait_pca_CH[[3]], trait_pca_CH[[2]]) %>%
        group_by(Treatment) %>%
        select(-Site_trt, -Year, -Site, -Treatment, -PlotID)

      adonis(pca_test[c(2:7)] ~ pca_test$Treatment, method='eu')

    }

  ),

  # Snowbed
  tar_target(
    name = trait_pca_SB,
    command = make_trait_pca(Trait_Mean |>
                               filter(Site == "SB"))

  ),


  tar_target(
    name = trait_pca_SB_test,
    command = {

      pca_test <- cbind(trait_pca_SB[[3]], trait_pca_SB[[2]]) %>%
        group_by(Treatment) %>%
        select(-Site_trt, -Year, -Site, -Treatment, -PlotID)

      adonis(pca_test[c(2:11)] ~ pca_test$Treatment, method='eu')

    }

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

  # importance of ITV in warm vs control
  tar_target(
    name = itv_importance,
    command = importance_intra_vs_inter(Trait_Mean) |>
      select(Trait, "Habitat type" = Site, term:p.value) |>
      mutate(term = recode(term, "(Intercept)" = "Intercept", "TreatmentWarming" = "Warming"),
             estimate = round(estimate, 2),
             std.error = round(std.error, 2),
             statistic = round(statistic, 2),
             p.value = round(p.value, 3))
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
  ),

  # Annual climate control vs otc
  tar_target(
    name = annual_temp,
    command = Monthly_Temp %>%
      mutate(year = year(YearMonth)) %>%
      group_by(LoggerLocation, Treatment) %>%
      summarise(mean = mean(Value)) |>
      pivot_wider(names_from = Treatment, values_from = mean) %>%
      mutate(diff = OTC - CTL)
  ),

  # Annual climate control vs otc test
  tar_target(
    name = annual_temp_analysis,
    command = Monthly_Temp %>%
      mutate(Year = year(YearMonth)) %>%
      group_by(LoggerLocation) %>%
      nest() %>%
      mutate(mod1 = map(data, ~ lm(Value ~ Treatment, data = .x)),
             res = map(mod1, tidy)) |>
      unnest(res)
  )


)
