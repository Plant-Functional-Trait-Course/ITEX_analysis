#### COMMUNITY DATA ANALYSIS ####



#### CALCULATE COMMUNITY METRICS ####
calc_comm_metrics <- function(comm){

  comm_resp <- comm %>%
    group_by(Year, Site, Treatment, PlotID) %>%
    summarise(Richness = n(),
              Diversity = diversity(Abundance),
              Evenness = Diversity/log(Richness),

              # proportions
              sumAbundance = sum(Abundance),
              propGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")])/sumAbundance,
              propForb = sum(Abundance[FunctionalGroup %in% c("forb")])/sumAbundance,
              propShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])/sumAbundance,
              propEShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")])/sumAbundance,
              propDShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])/sumAbundance,
              propLichen = sum(Abundance[FunctionalGroup %in% c("lichen")])/sumAbundance,
              propBryo = sum(Abundance[FunctionalGroup %in% c("moss", "liverwort")])/sumAbundance,

              # abundance
              totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forb", "eshrub", "dshrub")]),
              totalGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")]),
              totalForb = sum(Abundance[FunctionalGroup %in% c("forb")]),
              totalShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")]),
              totaleShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")]),
              totaldShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])
    )
  return(comm_resp)
}


#### Change in community metrics (Figure S4) ####
community_metric_change <- function(Community, metaItex, CommResp){

  #multivariate community distances
  comm_distances <- Community %>%
    select(-FunctionalGroup, -Elevation_m, -Latitude_N, -Longitude_E, -Flag) %>%
    filter(Year != 2009) %>%
    spread(key = Taxon, value = Abundance, fill = 0) %>%
    group_by(PlotID) %>%
    do(tibble(out = as.vector(vegdist(select(., -(Year:PlotID)), method = "bray")))) %>%
    left_join(metaItex, by = "PlotID")  %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

  #calculate change in community metrics
  metric_plot_dist <- CommResp %>%
    filter(Year != 2009) %>%
    gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>%
    group_by(Treatment, PlotID, Site, response) %>%
    summarise(dist = diff(value)) %>%
    filter(response %in% c("Richness", "Diversity", "Evenness", "sumAbundance", "totalGraminoid", "totalForb", "totaldShrub", "totaleShrub", "propLichen", "propBryo"))

  comm_distances_merge <- comm_distances %>%
    rename("dist" = "out") %>%
    mutate(response = "Bray Curtis Distance")

  metric_plot_dist <- bind_rows(metric_plot_dist, comm_distances_merge) |>
    # prettify response names
    mutate(response = recode(response,
                             "propBryo" = "Bryophyte Abundance",
                             "propLichen" = "Lichen Abundance",
                             "sumAbundance" = "Vascular Abundance",
                             "totalForb" = "Forb Abundance",
                             "totalGraminoid" = "Graminoid Abundance",
                             "totaleShrub" = "Evergreen Shrub Abundance",
                             "totaldShrub" = "Deciduous Shrub Abundance"),
           # sort levels
           response = factor(response,
                             levels = c("Bray Curtis Distance", "Evenness", "Richness", "Diversity", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))

  return(metric_plot_dist)
}


community_t_test <- function(Comm_Metric_Change){
  t_test <- Comm_Metric_Change %>%
    filter(response != "Diversity") %>%
    droplevels() %>%
    group_by(response, Site, Treatment) %>%
    summarise(P = t.test(dist, mu = 0)$p.value,
              Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
              MaxWidth = max(dist)) %>%
    ungroup() %>%
    mutate(Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas")),
           Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"))

  return(t_test)

}


### MAKE SP PCA
make_sp_pca <- function(comm_sp, comm_info){

  # make pca
  res <- rda(comm_sp)

  out <- bind_cols(comm_info, fortify(res) |>
                     filter(Score == "sites"))

  sp <- fortify(res) |>
    filter(Score == "species")

  return(list(out, sp, res))

}



height_analysis <- function(Height){

  height_model <- Height %>%
    nest(data = -Site) %>%
    mutate(
      fit1 = map(data, ~ lmer(Value ~ Treatment + (1|PlotID), data = .x)),
      fit2 = map(data, ~ lmer(Value ~ 1 + (1|PlotID), data = .x)),
      tidy1 = map(fit1, glance),
      tidy2 = map(fit2, glance)
  )

  height_model_result <- bind_rows(Treatment_model = height_model %>%
              unnest(tidy1),
            Null_model = height_model %>%
              unnest(tidy2),
            .id = "Model") %>%
    select(Model, Site, AIC) %>%
    pivot_wider(names_from = "Model", values_from = "AIC") %>%
    mutate(Model_Diff = Treatment_model - Null_model)

  return(height_model_result)

}

