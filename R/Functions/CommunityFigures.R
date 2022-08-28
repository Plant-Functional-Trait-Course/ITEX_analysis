### COMMUNITY FIGURES

## Figure 1 Community PCA
make_sp_pca_figure <- function(pca_sp, pca_sp_sb, pca_sp_ch, pca_sp_dh){

  e_B <- eigenvals(pca_sp[[3]])/sum(eigenvals(pca_sp[[3]]))

  species <- pca_sp[[2]] |>
    mutate(length = sqrt(PC1^2 + PC2^2),
           Label = capitalize(Label)) |>
    filter(length > 0.7) |>
    select(Label, length)

  p1 <- pca_sp[[1]] |>
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
    ggplot(aes(x = PC1, y = PC2, colour = Site)) +
    geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"), shape = Treatment)) +
    geom_path(aes(linetype = Treatment, group = PlotID)) +
    coord_equal() +
    scale_size_discrete(name = "Year", range = c(1.5, 3), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_color_manual(name = "Habitat type",
                       values = c("blue", "forestgreen", "orange"),
                       labels = c("Snowbed", "Cassiope heath", "Dryas heath")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(1, 17)) +

    ## arrows
    geom_segment(data = pca_sp[[2]], aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow=arrow(length=unit(0.2,"cm")),
                 alpha = 0.75, color = 'grey70') +
    theme_bw() +
    theme(text = element_text(size = 13),
          legend.box="vertical",
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


    p_all <- p1 +
      labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
           y = glue("PCA1 ({round(e_B[2] * 100, 1)}%)"),
           tag = "A") +
      xlim(-3, 3.8) +
      # species names
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
                    label = Label), col = 'black') +
      # stats
      geom_text(aes(x = -2.5, y = 1, label = "T x H*** + H x Y***"), colour = "black") +
      theme(legend.position = "top")


  # snowbed
  eig_sb <- eigenvals(pca_sp_sb[[3]])/sum(eigenvals(pca_sp_sb[[3]]))

  p_sb <- p1 %+% (pca_sp_sb[[1]]) +
    geom_text(aes(x = 1, y = -Inf, label = "Y***"), vjust = -0.3, size = 5, colour = "black") +
    labs(x = glue("PCA1 ({round(eig_sb[1] * 100, 1)}%)"),
         y = glue("PCA1 ({round(eig_sb[2] * 100, 1)}%)"),
         title = "Snowbed",
         tag = "B")


  # Cassiope heath
  eig_ch <- eigenvals(pca_sp_ch[[3]])/sum(eigenvals(pca_sp_ch[[3]]))

  p_ch <- p1 %+% (pca_sp_ch[[1]]) +
    geom_text(aes(x = 1, y = -Inf, label = "T*** + Y*"), vjust = -0.3, size = 5, colour = "black") +
    scale_color_manual(values = "forestgreen") +
    labs(x = glue("PCA1 ({round(eig_ch[1] * 100, 1)}%)"),
         y = glue("PCA1 ({round(eig_ch[2] * 100, 1)}%)"),
         title = expression(paste(italic(Cassiope), " heath")),
         tag = "C")


  # Dryas heath
  eig_dh <- eigenvals(pca_sp_dh[[3]])/sum(eigenvals(pca_sp_dh[[3]]))

  p_dh <- p1 %+% (pca_sp_dh[[1]]) +
    geom_text(aes(x = 2, y = -Inf, label = "T* + Y***"), vjust = -0.3, size = 5, colour = "black") +
    scale_color_manual(values = "orange") +
    labs(x = glue("PCA1 ({round(eig_dh[1] * 100, 1)}%)"),
         y = glue("PCA1 ({round(eig_dh[2] * 100, 1)}%)"),
         title = expression(paste(italic(Dryas), " heath")),
         tag = "D")

  pca_species <- p_all / (p_sb + p_ch + p_dh) + plot_layout(height = c(3, 1))

  return(pca_species)

}


# S3 Canopy height
make_height_figure <- function(Height){

  canopy_height_figure <- Height |>
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"),
           Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas"))) |>
    ggplot(aes(x = Site, y = Value, fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey", "red")) +
    scale_x_discrete("Habitat type",
                     labels = expression(Snowbed, italic(Cassiope), italic(Dryas))) +
    labs(y = "Canopy height cm") +
    annotate("text", x = 2, y = 30, label = "T *") +
    annotate("text", x = 3, y = 30, label = "T *") +
    theme_bw() +
    theme(text = element_text(size = 15),
          legend.position = "top")

  return(canopy_height_figure)

}



### Fig S4 change in community metrics
community_metrics_figure <- function(Comm_Anova_tidy, Comm_Metric_Change, Comm_t_Test){

  anova_text <- Comm_Anova_tidy %>%
    ungroup() %>%
    filter(response != "Diversity") %>%
    mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>%
    filter(term != "Residuals") %>%
    mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>%
    mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>%
    pivot_wider(id_cols = response, names_from = term, values_from = test) %>%
    mutate(T = ifelse(is.na(T), "", T)) %>%
    mutate(H = ifelse(is.na(H), "", H)) %>%
    mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H))))

  metric_change <- Comm_Metric_Change %>%
    filter(response != "Diversity") %>%
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) %>%
    group_by(response) %>%
    mutate(y_max = max(dist), y_min = min(dist)) %>%
    mutate(Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas"))) %>%
    ggplot() +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_boxplot(aes(x = Site, y = dist, fill = Treatment)) +
    scale_fill_manual(values = c("darkgray", "red")) +
    scale_x_discrete("Habitat type",
                     labels = expression(Snowbed, italic(Cassiope), italic(Dryas))) +
    labs( y = "Change in diversity metric") +
    facet_wrap(~response, scales = "free_y", ncol = 2) +
    theme_bw() +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(size = 10),
          legend.position = "top") +
    geom_blank(aes(y = y_min + 0.5*y_min)) +
    geom_blank(aes(y = y_max + 0.4*y_max)) +
    geom_text(aes(label = text, x = 0.5, y = Inf, hjust = 0, vjust = 2), size = 4, color = "black",  data = anova_text) +
    geom_text(aes(label = Sig, x = Site, y = -Inf, hjust = 0.5, vjust = 0, group = Treatment), size = 6, position = position_dodge(0.75),color = "black",  data = Comm_t_Test)

  return(metric_change)

}
