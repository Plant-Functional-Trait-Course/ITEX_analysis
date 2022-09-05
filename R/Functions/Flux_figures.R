#### Flux Figures ####


## Figure 4 Carbon fluxes
make_flux_figure <- function(Trait_Model_Output, Model_Output){

  Trait_Model_Output <- Trait_Model_Output |>
    mutate(Cflux = recode(Cflux, GPP = "GEP"))

  Model_Output <- Model_Output |>
    mutate(Cflux = recode(Cflux, GPP = "GEP"))


  # Variance explained without ITV
  p1 <- Model_Output %>%
    filter(ITV == "no_ITV") %>%
    ggplot(aes(x = Cflux, y = value*100, fill = Variables)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("grey90", "#0072B2", "#0072B2", "#009E73" , "#009E73", "#F0E442", "#F0E442"),
                      labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    geom_col_pattern(aes(#specify angle
      pattern_angle = Variables,
      # specify patter
      pattern = Variables),
      pattern_fill = "black",
      pattern_spacing = 0.01) +
    # distinguish pattern type
    scale_pattern_manual(values = c("stripe", "stripe", "none", "stripe" , "none", "stripe", "none"),
                         labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    #distinguish pattern angle
    scale_pattern_angle_manual(values = c(45, 45, 0, 45 , 0, 45, 0),
                               labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait"),
                               guide = guide_legend(override.aes = list(pattern_spacing = 0.005))) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10)) +
    scale_x_discrete(labels = c("GEP", expression(R[eco]), "NEE")) +
    labs(x = "", y = "Explained variance %", title = "Interspecific", tag = "A") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 14))


  # Variance explained with ITV
  p2 <- Model_Output %>%
    filter(ITV == "ITV") %>%
    ggplot(aes(x = Cflux, y = value*100, fill = Variables)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("grey90", "#0072B2", "#0072B2", "#009E73" , "#009E73", "#F0E442", "#F0E442"),
                      labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    geom_col_pattern(aes(#specify angle
      pattern_angle = Variables,
      # specify patter
      pattern = Variables),
      pattern_fill = "black",
      pattern_spacing = 0.01) +
    # distinguish pattern type
    scale_pattern_manual(values = c("stripe", "stripe", "none", "stripe" , "none", "stripe", "none"),
                         labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    #distinguish pattern angle
    scale_pattern_angle_manual(values = c(45, 45, 0, 45 , 0, 45, 0),
                               labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait"),
                               guide = guide_legend(override.aes = list(pattern_spacing = 0.005))) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10)) +
    scale_x_discrete(labels = c("GEP", expression(R[eco]), "NEE")) +
    labs(x = "", y = "", title = "Inter- and intraspecific", tag = "B") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 14))


  # Variance explained by traits
  p3 <- Trait_Model_Output %>%
    mutate(ITV = ITV - noITV,
           ITV = abs(ITV)) %>%
    gather(key = ITV, value = Var.exp, noITV:ITV) %>%
    ggplot(aes(x = Cflux, y = Var.exp * 100, fill = ITV)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("grey20", "grey70"),
                      labels = c("Intraspecific", "Intraspecific"),
                      name = "Trait variation") +
    geom_hline(yintercept = 0, colour = "grey40") +
    labs(x = "", y = "Explained variance %", tag = "C") +
    scale_y_continuous(limits = c(-10, 70),
                       breaks = seq(-10, 70, by = 10)) +
    scale_x_discrete(labels = c("GEP", expression(R[eco]), "NEE")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 14),
          legend.position = c(0.2, 0.8))

  p4 <- p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

  carbon_flux_figure <- p4 / (p3 + plot_spacer())

  return(carbon_flux_figure)

}



#### Effect size Figure S8 ####

###### USING NOT STANDARDIZED FLUXES!!!!!! #########
# Effect of single predictors on NEE. GPP and Reco

make_effect_size_figure <- function(Flux_and_Traits){

  #effect of single predictors on fluxes
  Flux_analyses <- Flux_and_Traits %>%
    select(Site, PlotID, Treatment, NEE_ln, ER_ln, GPP700, SoilTemp, SoilMoist, CanTemp_Light, Richness, Evenness, Diversity, Height_cm, Graminoid:Lichen, Evergreen, Decidious, ITV_C:No_ITV_SLA) %>%
    rename(NEE = NEE_ln, GPP = GPP700, Reco = ER_ln) %>%
    pivot_longer(cols = SoilTemp:No_ITV_SLA, names_to = "prediction", values_to = "value") %>%
    pivot_longer(cols = NEE:GPP, names_to = "response", values_to = "C_flux") %>%
    group_by(response, prediction) %>%
    mutate(value = scale(value)[,1]) %>%
    do(tidy(lm(C_flux ~ value, data = .))) %>%
    filter(!term == "(Intercept)") %>%
    mutate(lower = (estimate - std.error * 1.96),
           upper = (estimate + std.error * 1.96),
           overlapp_zero = if_else(p.value < 0.05, "signi", "non-signi"),
           response = recode(response, GPP = "GEP", Reco = "R[eco]"),
           response = factor(response, levels = c("GEP", "R[eco]", "NEE")))


  effect_size_figure <- ggplot(Flux_analyses, aes(x = prediction, y = estimate,
                                                  shape = response,
                                                  fill = prediction,
                                                  alpha = p.value < 0.05,
                                                  ymin = lower, ymax = upper)) +
    geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    scale_shape_manual(labels = c("NEE", "GEP", "Reco"), values = c(22, 24,21)) +
    scale_fill_manual(values = c(rep("#F0E442", 18), rep("#0072B2", 10), rep("grey90", 3))) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_x_discrete(labels = c("No ITV CN", "No ITV N", "No ITV C", "No ITV P", "No ITV SLA", "No ITV LDMC", "No ITV LT", "No ITV LA", "No ITV Height", "ITV CN", "ITV N", "ITV C", "ITV P", "ITV SLA", "ITV LDMC", "ITV LT", "ITV LA", "ITV Height", "Richness", "Evenness", "Diversity", "Plant Height", "Graminoid", "Forb", "Bryophyte", "Evergreen", "Decidious", "Lichen", "CanTemp Light","SoilTemp", "SoilMoist")) +
    labs(x = "", y = "Effect size") +
    facet_grid(~response, labeller = label_parsed) +
    theme(axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 13),
          strip.background = element_rect(colour="black", fill="white"),
          panel.background = element_rect(fill= "white"),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.text.x = element_text(size=12, face="bold"),
          axis.line = element_line(colour = "black"),
          legend.position = "none") +
    coord_flip()


  return(effect_size_figure)

}


#### Figure S2 Soil temp and moisture ####
make_soil_microclimate_figure <- function(Flux_and_Traits){

  microclimate <- Flux_and_Traits %>%
  select(Site, PlotID, Treatment, SoilTemp, SoilMoist) |>
  pivot_longer(cols = c(SoilTemp, SoilMoist), names_to = "variable", values_to = "value") |>
  mutate(Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
         Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas")),
         Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"),
         variable = recode(variable, SoilTemp = "Soil temperature (°C)", SoilMoist = "Soil moisture (%)"),
         variable = factor(variable, levels = c("Soil temperature (°C)", "Soil moisture (%)"))) |>
  ggplot(aes(x = Site, y = value, fill = Treatment)) +
    geom_boxplot() +
    scale_size_manual(values = c(3,3,3)) +
    scale_fill_manual(values = c('gray70', 'red')) +
    scale_x_discrete("Habitat type",
                     labels = expression(Snowbed, italic(Cassiope), italic(Dryas))) +
    labs(y = "") +
    facet_wrap(~ variable, scales = "free_y") +
    theme_bw() +
    theme(text = element_text(size = 15),
          legend.position = "top")

  return(microclimate)
}


#### Figure S7 mean fluxes ####

make_flux_mean_figures <- function(Flux_and_Traits, soil_resp){

  flux_means <- Flux_and_Traits %>%
    select(Site, PlotID, Treatment, NEE_ln, ER_ln, GPP700) |>
    pivot_longer(cols = c(NEE_ln:GPP700), names_to = "variable", values_to = "value") |>
    mutate(Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas")),
           variable = factor(variable, levels = c("GPP700", "ER_ln", "NEE_ln")),
           Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
    mutate(variable2 = factor(variable,
                              labels = c("GEP~(µmol~CO[2]~m^{-2}~s^{-1})",
                                         "R[eco]~(µmol~CO[2]~m^{-2}~s^{-1})",
                                         "NEE~(µmol~CO[2]~m^{-2}~s^{-1})"))) |>
    ggplot(aes(x = Site, y = value, fill = Treatment)) +
    geom_boxplot() +
    scale_size_manual(values = c(3, 3, 3)) +
    scale_fill_manual(values = c('gray70', 'red')) +
    scale_x_discrete(labels = expression(Snowbed, italic(Cassiope), italic(Dryas))) +
    labs(x = "", y = "") +
    facet_wrap(~ variable2, scales = "free_y", labeller = label_parsed, nrow = 2) +
    tag_facets(tag_pool = c("A", "B", "C"),
               tag_suffix = "", ) +
    theme_bw() +
    theme(tagger.panel.tag.background = element_blank(),
          text = element_text(size = 15),
          panel.spacing = unit(0.8, "cm"),
          legend.position = "top")



    Rsoil_plot = soil_resp |>
      mutate(variable = "SoilR)",
             variable = factor(variable,
                                labels = c("R[soil]~(µmol~CO[2]~m^{-2}~day^{-1})")),
             Treatment = recode(Treatment, "OTC" = "Warming"),
             Treatment = factor(Treatment)) |>
      ggplot(aes(x = Treatment,
                         y = -Flux_g_CO2_m2_day ,  ## NOTE: added "-" in front of data
                         fill = Treatment)) +
      geom_boxplot() +
      scale_size_manual(values = c(3,3)) +
      scale_fill_manual(values = c('grey', 'red'), label = c("Control", "Warming")) +
      labs(x = "", y = "") +
      ylim(-3.5, 0.3) +
      stat_summary(geom = 'text', label = c('a', 'b'), fun = max, vjust = -1, size = 3) +
      facet_wrap(~ variable, labeller = label_parsed) +
      tag_facets(tag_pool = c("D"),
                 tag_suffix = "") +
      theme_bw() +
      theme(legend.position = "none",
            text = element_text(size = 15),
            tagger.panel.tag.background = element_blank())


    layout <- c(
      area(t = 1, l = 1, b = 100, r = 100),
      area(t = 55, l = 52, b = 100, r = 100)
    )
    flux_mean_plot = flux_means + Rsoil_plot +
      plot_layout(design = layout)


  return(flux_mean_plot)
}
