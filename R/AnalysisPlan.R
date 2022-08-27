# Analysis and Figure plan

AnalysisPlan <- list(


  ### COMMUNITY ANALYSIS
  # Diversity indices
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


  # Species PCA
  # wide community data with rare species removed
  tar_target(
    name = comm_wide,
    command = Community %>%
      select(-c(FunctionalGroup, Elevation_m:Flag)) |>
      mutate(Abundance = sqrt(Abundance)) |>
      pivot_wider(names_from = Taxon, values_from = Abundance, values_fill = 0) |>
      mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))
  ),

  # PCA for all habitats
  tar_target(
    name = pca_sp,
    command = {
      # select traits
      comm_sp <- comm_wide %>%
        select("bistorta vivipara":"bryum sp")

      # meta data
      comm_info <- comm_wide %>%
        select(Year:PlotID)

      make_sp_pca(comm_sp = comm_wide %>%
                    select("bistorta vivipara":"bryum sp"),
                  comm_info = comm_wide %>%
                    select(Year:PlotID))
    }
  ),

  # adonis all species: habitat x treatment
  tar_target(
    name = adonis_sp_all,
    command = adonis(pca_sp[[1]] |>
                       select(PC1:PC6) ~ pca_sp[[1]]$Treatment * pca_sp[[1]]$Site * pca_sp[[1]]$Year, method='eu')
  ),


  # PCA separate per habitat
  ## snowbed
  tar_target(
    name = pca_sp_sb,
    command = make_sp_pca(comm_sp = comm_wide %>%
                            filter(Site == "SB") |>
                            select("bistorta vivipara":"bryum sp"),
                          comm_info = comm_wide %>%
                            filter(Site == "SB") |>
                            select(Year:PlotID))
  ),



  ## cassiope
  tar_target(
    name = pca_sp_ch,
    command = make_sp_pca(comm_sp = comm_wide %>%
                            filter(Site == "CH") |>
                            select("bistorta vivipara":"bryum sp"),
                          comm_info = comm_wide %>%
                            filter(Site == "CH") |>
                            select(Year:PlotID))
  ),

  ## dryas
  tar_target(
    name = pca_sp_dh,
    command = make_sp_pca(comm_sp = comm_wide %>%
                            filter(Site == "DH") |>
                            select("bistorta vivipara":"bryum sp"),
                          comm_info = comm_wide %>%
                            filter(Site == "DH") |>
                            select(Year:PlotID))
  ),


  # adonis per habitat: treatment x year
  tar_target(
    name = pca_sb,
    command = adonis(pca_sp_sb[[1]] |>
                                select(PC1:PC6) ~ pca_sp_sb[[1]]$Treatment * pca_sp_sb[[1]]$Year, method='eu')
  ),

  tar_target(
    name = pca_ch,
    command = adonis(pca_sp_ch[[1]] |>
                       select(PC1:PC6) ~ pca_sp_ch[[1]]$Treatment * pca_sp_ch[[1]]$Year, method='eu')
  ),

  tar_target(
    name = pca_dh,
    command = adonis(pca_sp_dh[[1]] |>
                       select(PC1:PC6) ~ pca_sp_dh[[1]]$Treatment * pca_sp_dh[[1]]$Year, method='eu')
  ),



  # Canopy height
  tar_target(
    name = Height_result,
    command = height_analysis(Height)
  ),


  ### TRAIT ANALYSIS

  # Bootstrapping
  tar_target(
    name = Trait_Mean,
    command = make_bootstrapping(Community, Traits)
  ),

  # Trait PCA
  tar_target(
    name = trait_pca_all,
    command = make_trait_pca(Trait_Mean, habitat = "all")

  ),

  #PERMANOVA of PCA groups

  tar_target(
    name = perm,
    command = {

      pca_test <- trait_pca_all[[1]] %>%
        rename("Habitat" = "Site") %>%
        group_by(Habitat, Treatment) %>%
        select(-Site_trt, -Year, -PlotID)

      adonis(pca_test[c(5:10)] ~ pca_test$Treatment * pca_test$Habitat, method='eu')

    }
  ),

  ### PCA per habitat
  # Dryas
  tar_target(
    name = trait_pca_DH,
    command = make_trait_pca(Trait_Mean |>
                               filter(Site == "DH"), habitat = "DH")

  ),

  tar_target(
    name = trait_pca_DH_test,
    command = {

      pca_test <- trait_pca_DH[[1]] %>%
        group_by(Treatment) %>%
        select(-Site_trt, -Year, -Site, -PlotID)

      adonis(pca_test[c(4:9)] ~ pca_test$Treatment, method='eu')

    }

  ),


  #Cassiope
  tar_target(
    name = trait_pca_CH,
    command = make_trait_pca(Trait_Mean |>
                               filter(Site == "CH"), habitat = "CH")

  ),

  tar_target(
    name = trait_pca_CH_test,
    command = {

      pca_test <- trait_pca_CH[[1]] %>%
        group_by(Treatment) %>%
        select(-Site_trt, -Year, -Site, -PlotID)

      adonis(pca_test[c(4:8)] ~ pca_test$Treatment, method='eu')

    }

  ),

  #Snowbed
  tar_target(
    name = trait_pca_SB,
    command = make_trait_pca(Trait_Mean |>
                               filter(Site == "SB"), habitat = "SB")

  ),


  tar_target(
    name = trait_pca_SB_test,
    command = {

      pca_test <- trait_pca_SB[[1]] %>%
        group_by(Treatment) %>%
        select(-Site_trt, -Year, -Site, -PlotID)

      adonis(pca_test[c(4:9)] ~ pca_test$Treatment, method='eu')

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

  # Annual climate control vs otc (range)
  # only use 2005 and 2015 data because other year are lacking data
  tar_target(
    name = annual_temp,
    command = Monthly_Temp %>%
      mutate(year = year(YearMonth)) %>%
      filter(year %in% c(2005, 2015)) |>
      group_by(year, LoggerLocation, Treatment) %>%
      summarise(mean = mean(Value),
                se = sd(Value)/sqrt(n())) |>
      filter(LoggerLocation == "surface", Treatment == "CTL")
      #pivot_wider(names_from = Treatment, values_from = mean) %>%
      #mutate(diff = OTC - CTL)
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
