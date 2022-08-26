### COMMUNITY FIGURES

## Figure 1 Community PCA
make_sp_pca_figure <- function(pca_sp, pca_sp_sb, pca_sp_ch, pca_sp_dh){

  e_B <- eigenvals(pca_sp[[3]])/sum(eigenvals(pca_sp[[3]]))

  species <- pca_sp[[2]] |>
    mutate(length = sqrt(PC1^2 + PC2^2),
           Label = capitalize(Label)) |>
    filter(length > 0.7) |>
    select(Label, length)

  p1 = pca_sp[[1]] |>
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
    ggplot(aes(x = PC1, y = PC2, colour = Site)) +
    geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"), shape = Treatment)) +
    geom_path(aes(linetype = Treatment, group = PlotID)) +
    coord_equal() +
    scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_color_manual(name = "Habitat type",
                       values = c("blue", "forestgreen", "orange"),
                       labels = c("Snowbed", "Cassiope heath", "Dryas heath")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(1, 17)) +

    ## arrows
    geom_text(data = pca_sp[[2]] |>
                mutate(Label = capitalize(Label)) |>
                inner_join(species, by = "Label") |>
                mutate(Label = if_else(Label == "Unidentified liverwort sp", "Liverwort sp", Label)),
              aes(x = PC1 + case_when(Label == "Festuca rubra" ~ -1,
                                      Label == "Liverwort sp" ~ -1,
                                      Label == "Peltigera sp" ~ 0.5,
                                      Label == "Dicranum sp" ~ 1,
                                      TRUE ~ 0),
                  y = PC2 + case_when(PC2 > 0 ~ 0.3,
                                      Label == "Dicranum sp" ~ 0.1,
                                      TRUE ~ -0.3),
                  label = Label), col = 'grey70') +
    geom_segment(data = pca_sp[[2]], aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow=arrow(length=unit(0.2,"cm")),
                 alpha = 0.75, color = 'grey70') +

    # stats
    geom_text(aes(x = -2.5, y = 1, label = "T x H**"), colour = "black") +

    xlim(-3, 3.8) +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA1 ({round(e_B[2] * 100, 1)}%)"),
         tag = "A") +
    theme_bw()


  plot_annotation <- tibble(`Habitat type` = c("Snowbed", "Cassiope heath", "Dryas heath"),
                            label = c("Y***", "T*** + Y*", "T* + Y***")) |>
    mutate(`Habitat type` = factor(`Habitat type`, levels = c("Snowbed", "Cassiope heath", "Dryas heath")))

  # eigenvals(pca_sp_sb[[3]])/sum(eigenvals(pca_sp_sb[[3]]))
  # eigenvals(pca_sp_ch[[3]])/sum(eigenvals(pca_sp_ch[[3]]))
  # eigenvals(pca_sp_dh[[3]])/sum(eigenvals(pca_sp_dh[[3]]))

  p2 = bind_rows(Snowbed = pca_sp_sb[[1]],
                 "Cassiope heath" = pca_sp_ch[[1]],
                 "Dryas heath" = pca_sp_dh[[1]],
                 .id = "Habitat type"
  ) |>
    mutate(`Habitat type` = factor(`Habitat type`, levels = c("Snowbed", "Cassiope heath", "Dryas heath"))) |>
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
    ggplot(aes(x = PC1, y = PC2, colour = `Habitat type`)) +
    geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"), shape = Treatment)) +
    geom_path(aes(linetype = Treatment, group = PlotID)) +
    scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_color_manual(values = c("blue", "forestgreen", "orange")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(1, 17)) +
    facet_wrap(~ `Habitat type`) +
    geom_text(data = plot_annotation, aes(x = 2, y = -3, label = label), colour = "black") +
    labs(tag = "B") +
    theme_bw() +
    theme(legend.position = "none")

  pca_species <- p1 / p2 + plot_layout(height = c(3, 1))

  return(pca_species)

}

# anova_t <- Comm_Anova_tidy
# metric_plot_dist <- Comm_Metric_Change
# t_test <- Comm_t_Test
### Fig S3 change in community metrics
community_metrics_figure <- function(anova_t, metric_plot_dist, t_test){

  anova_text <- anova_t %>%
    ungroup() %>%
    filter(response != "Diversity") %>%
    mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
    mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>%
    filter(term != "Residuals") %>%
    mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>%
    mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>%
    pivot_wider(id_cols = response, names_from = term, values_from = test) %>%
    mutate(T = ifelse(is.na(T), "", T)) %>%
    mutate(H = ifelse(is.na(H), "", H)) %>%
    mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H)))) %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))

  metric_change <- metric_plot_dist %>%
    filter(response != "Diversity") %>%
    mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")),
           Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) %>%
    group_by(response) %>%
    mutate(y_max = max(dist), y_min = min(dist)) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
    ggplot() +
    geom_hline(yintercept = 0) +
    geom_boxplot(aes(x = Site, y = dist, fill = Treatment)) +
    scale_fill_manual(values = c("darkgray", "red")) +
    facet_wrap(~response, scales = "free", ncol = 2) +
    ylab("Change in Metric") +
    xlab("Habitat Type") +
    theme_bw() +
    theme(text = element_text(size = 15),
          legend.position = "top") +
    #stat_compare_means(aes(group = Site), label = "p.signif", method = "anova", hide.ns = F, label.x.npc = 0.05, label.y.npc = 0.05)+
    #stat_compare_means(aes(x = Site, y = dist, group = Treatment), label = "p.signif", method = "anova", hide.ns = T, label.y.npc = 0) +
    geom_blank(aes(y = y_min + 0.5*y_min)) +
    geom_blank(aes(y = y_max + 0.4*y_max)) +
    geom_text(aes(label = text, x = 0.5, y = Inf, hjust = 0, vjust = 2), size = 4, color = "black",  data = anova_text) +
    geom_text(aes(label = Sig, x = Site, y = -Inf, hjust = 0.5, vjust = 0, group = Treatment), size = 6, position = position_dodge(0.75),color = "black",  data = t_test)

  return(metric_change)

}


# S4 Canopy height
make_height_figure <- function(height){

  canopy_height_figure <- height |>
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
    ggplot(aes(x = Site, y = Value, fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey", "red")) +
    labs(y = "Canopy height cm", x = "Habitat Type") +
    annotate("text", x = 2, y = 30, label = "T *") +
    annotate("text", x = 3, y = 30, label = "T *") +
    theme_bw() +
    theme(text = element_text(size = 20))

  return(canopy_height_figure)

}
