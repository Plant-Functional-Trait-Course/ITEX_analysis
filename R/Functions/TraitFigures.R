#### TRAIT FIGURES ####

## Figure S5 trait mean ##
make_trait_mean_figure <- function(Anova_Trait, Trait_Mean){

  anova_text_trait <- fancy_trait_name_dictionary(Anova_Trait) %>%
    ungroup() %>%
    #unnest(aov_tidy) |>
    mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>%
    filter(term != "Residuals") %>%
    mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>%
    mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>%
    pivot_wider(id_cols = Trait_fancy, names_from = term, values_from = test) %>%
    mutate(T = ifelse(is.na(T), "", T)) %>%
    mutate(H = ifelse(is.na(H), "", H)) %>%
    mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H))))


  trait_mean_plot <- fancy_trait_name_dictionary(Trait_Mean) %>%
    filter(PlotID != "CAS-4", PlotID != "CAS-9", PlotID != "CAS-10", PlotID != "CAS-6") %>%
    group_by(Trait_fancy) %>%
    mutate(y_max = max(mean), y_min = min(mean)) %>%
    # sort the traits
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"),
           Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas")))  %>%
    ggplot() +
    geom_boxplot(aes(x = Site, y = mean, fill = Treatment)) +
    geom_text(aes(label = text, x = 1, y = Inf), vjust = 1.2, size = 3.5, color = "black",  data = anova_text_trait) +
    scale_fill_manual(values = c("darkgray", "red")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    scale_x_discrete("Habitat type",
                     labels = expression(Snowbed, italic(Cassiope), italic(Dryas))) +
    labs(y = "CWM trait value") +
    facet_wrap(~Trait_fancy, scales = "free_y", labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(size = 9),
          legend.position = "top")



  return(trait_mean_plot)
}


#### Figure 3: Difference fixed and specific mean (ITV) ####

make_itv_figure <- function(Trait_Mean){

  traitMean <- fancy_trait_name_dictionary(Trait_Mean) %>%
    mutate(itv_diff = mean-mean_noitv,
           Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"),
           Site = recode(Site, CH = "Cassiope", DH = "Dryas", SB = "Snowbed"),
           Site = factor(Site, levels = c("Snowbed", "Cassiope", "Dryas")))

  t_test_itv <- traitMean %>%
    group_by(Trait_fancy, Site, Treatment) %>%
    summarise(P = t.test(itv_diff, mu = 0)$p.value,
              Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
              MaxWidth = max(itv_diff))

  itv_plot <- traitMean %>%
    ggplot() +
    geom_boxplot(aes(x = Site, y = itv_diff, fill = Treatment)) +
    geom_hline(aes(yintercept = 0), colour = "grey40") +
    geom_text(aes(label = Sig, y = Inf, x = Site, group = Treatment), vjust = 1.2, position = position_dodge(0.75), data = t_test_itv, size = 4.5) +
    scale_fill_manual(values = c("darkgray", "red")) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.35))) +
    scale_x_discrete("Habitat type",
                     labels = expression(Snowbed, italic(Cassiope), italic(Dryas))) +
    labs(y = "Specific - fixed (no ITV) trait mean") +
    facet_wrap(~Trait_fancy, scales = "free_y", labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(size = 9),
          legend.position = "top")

  return(itv_plot)

}

#### Fig 2 Trait PCA ####

make_trait_pca_figure <- function(trait_pca_all, trait_pca_SB, trait_pca_CH, trait_pca_DH){

  e_B <- eigenvals(trait_pca_all[[3]])/sum(eigenvals(trait_pca_all[[3]]))

  # make fancy names
  pca_traits <- fancy_trait_name_dictionary(trait_pca_all[[2]] |>
                                          rename(Trait = Label))

  trait_data <- trait_pca_all[[1]] |>
    mutate(Site = recode(Site, CH = "Cassiope heath", DH = "Dryas heath", SB = "Snowbed"))

  p1 <- ggplot(trait_data, aes(x = PC1, y = PC2, colour = Site)) +
    geom_point(aes(shape = Treatment), size = 3) +
    coord_equal() +
    scale_shape_manual(values = c(1, 17),
                       labels = c("Control", "Warming")) +
    # arrows
    geom_segment(data = pca_traits, aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow=arrow(length=unit(0.2,"cm")),
                 alpha = 0.75, color = 'grey70') +
    theme_bw() +
    theme(text = element_text(size = 13),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  p_all <- p1 +
    scale_color_manual(name = "Habitat type",
                       values = c("blue", "forestgreen", "orange"),
                       labels = expression(Snowbed, italic(Cassiope)~heath, italic(Dryas)~heath)) +
    # trait names
    geom_text(data = pca_traits, aes(x = PC1 + case_when(PC1 > 0 ~ 0.05,
                                                         TRUE ~ -0.05),
                                     y = PC2 + case_when(PC2 > 0 ~ 0.07,
                                                         Trait_fancy == "N~('%')" ~ 0.1,
                                                         TRUE ~ -0.07),
                                     label = Trait_fancy),
              parse = TRUE,
              col = 'black') +
    # stats
    geom_text(aes(x = -1, y = 1.5, label = "T** + H***"), size = 5, colour = "black") +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
         tag = "A") +
    theme(legend.position = "top")


  ## snowbed
  e_B_sb <- eigenvals(trait_pca_SB[[3]])/sum(eigenvals(trait_pca_SB[[3]]))

  # make fancy names
  pca_sb_traits <- fancy_trait_name_dictionary(trait_pca_SB[[2]] |>
                                              rename(Trait = Label))

  p_sb <- p1 %+% (trait_pca_SB[[1]]) +
    scale_color_manual(values = "blue") +
    lims(x = c(-1.7, 1.7), y = c(-2, 2)) +

    # trait names
    geom_text(data = pca_sb_traits, aes(x = PC1 + case_when(Trait_fancy == "P~('%')" ~ -0.4,
                                                            PC1 > 0 ~ 0.1,
                                                            TRUE ~ -0.1),
                                     y = PC2 + case_when(Trait_fancy == "LDMC~(gg^{-1})" ~ -0.1,
                                                         Trait_fancy == "Plant~Height~(cm)" ~ 0.1,
                                                         Trait_fancy == "N~('%')" ~ -0.3,
                                                         Trait_fancy == "Dry~Mass~(g)" ~ -0.1,
                                                         Trait_fancy == "C13~('‰'))" ~ -0.3,
                                                         PC2 > 0 ~ 0.2,
                                                         TRUE ~ -0.2),
                                     label = Trait_fancy),
              parse = TRUE, col = 'black', size = 2.5) +

    labs(x = glue("PCA1 ({round(e_B_sb[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B_sb[2] * 100, 1)}%)"),
         title = "Snowbed",
         tag = "B")


  ## cassiope
  e_B_ch <- eigenvals(trait_pca_CH[[3]])/sum(eigenvals(trait_pca_CH[[3]]))

  # make fancy names
  pca_ch_traits <- fancy_trait_name_dictionary(trait_pca_CH[[2]] |>
                                                 rename(Trait = Label))

  p_ch <- p1 %+% (trait_pca_CH[[1]]) +
    scale_color_manual(values = "forestgreen") +
    lims(x = c(-1.7, 1.7), y = c(-2, 2)) +

    ## arrows
    geom_text(data = pca_ch_traits, aes(x = PC1 + case_when(Trait_fancy == "LDMC~(gg^{-1})" ~ 0.3,
                                                            Trait_fancy == "SLA~(cm^2*g^{-1})" ~ 0.4,
                                                            Trait_fancy == "Leaf~Area~(cm^2)" ~ 0.3,
                                                            Trait_fancy == "Dry~Mass~(g)" ~ 0.3,
                                                            Trait_fancy == "δC13~('‰')" ~ 0.15,
                                                            Trait_fancy == "δN15~('‰')" ~ 0.45,
                                                            Trait_fancy == "Plant~Height~(cm)" ~ 0.2,
                                                            Trait_fancy == "N~('%')" ~ 0.6,
                                                            Trait_fancy == "P~('%')" ~ 0,
                                                            PC1 > 0 ~ 0.1,
                                                            TRUE ~ -0.1),
                                        y = PC2 + case_when(Trait_fancy == "LDMC~(gg^{-1})" ~ 0.2,
                                                            Trait_fancy == "SLA~(cm^2*g^{-1})" ~ 0.1,
                                                            Trait_fancy == "Leaf~Area~(cm^2)" ~ 0.1,
                                                            Trait_fancy == "Dry~Mass~(g)" ~ -0.05,
                                                            Trait_fancy == "δC13~('‰')" ~ 0.1,
                                                            Trait_fancy == "δN15~('‰')" ~ -0.05,
                                                            Trait_fancy == "Plant~Height~(cm)" ~ -0.15,
                                                            Trait_fancy == "N~('%')" ~ 0.12,
                                                            Trait_fancy == "P~('%')" ~ -0.15,
                                                            PC2 > 0 ~ 0.2,
                                                            TRUE ~ -0.2),
                                        label = Trait_fancy),
              parse = TRUE, col = 'black', size = 2.5) +

    labs(x = glue("PCA1 ({round(e_B_ch[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B_ch[2] * 100, 1)}%)"),
         title = expression(paste(italic(Cassiope), " heath")),
         tag = "C")


  ## dryas
  e_B_dh <- eigenvals(trait_pca_DH[[3]])/sum(eigenvals(trait_pca_DH[[3]]))

  # make fancy names
  pca_dh_traits <- fancy_trait_name_dictionary(trait_pca_DH[[2]] |>                                                                                   rename(Trait = Label))
  p_dh <- p1 %+% (trait_pca_DH[[1]]) +
    scale_color_manual(values = "orange") +
    lims(x = c(-1.7, 1.7), y = c(-2, 2)) +

    ## arrows
    geom_text(data = pca_dh_traits, aes(x = PC1 + case_when(Trait_fancy == "LDMC~(gg^{-1})" ~ -0.3,
                                                            Trait_fancy == "Leaf~Area~(cm^2)" ~ -0.3,
                                                            Trait_fancy == "Plant~Height~(cm)" ~ -0.7,
                                                            Trait_fancy == "δN15~('‰')" ~ 0.45,
                                                            Trait_fancy == "P~('%')" ~ 0.2,
                                                            PC1 > 0 ~ 0.1,
                                                            TRUE ~ -0.1),
                                        y = PC2 + case_when(Trait_fancy == "LDMC~(gg^{-1})" ~ 0.2,
                                                            Trait_fancy == "Plant~Height~(cm)" ~ 0.05,
                                                            Trait_fancy == "Leaf~Area~(cm^2)" ~ 0.1,
                                                            Trait_fancy == "Dry~Mass~(g)" ~ -0.15,
                                                            Trait_fancy == "δN15~('‰')" ~ 0.1,
                                                            PC2 > 0 ~ 0.2,
                                                            TRUE ~ -0.2),
                                        label = Trait_fancy),
              parse = TRUE, col = 'black', size = 2.5) +

    labs(x = glue("PCA1 ({round(e_B_dh[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B_dh[2] * 100, 1)}%)"),
         title = expression(paste(italic(Dryas), " heath")),
         tag = "D")


  pca_plot <- p_all / (p_sb + p_ch + p_dh) + plot_layout(height = c(3, 1))

  return(pca_plot)
}


# var_split_exp <- Var_Split_Exp
# var_split <- Var_Split
## Figure S6 Trait variance partitioning
make_intra_vs_inter_figure <- function(Var_Split_Exp, Var_Split){

  varpart_graph <- Var_Split_Exp %>%
    mutate(level = trimws(level)) %>%
    filter(RelSumSq.Turnover < 999) %>%
    rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>%
    mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>%
    gather(key = variable, value = value, -trait, -level) %>%
    filter(variable != "Covariation", level != "Total", variable != "Total") %>%
    mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>%
    mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid"))) %>%
    rename(Trait = trait) %>%
    fancy_trait_name_dictionary(.) |>
    ggplot() +
    geom_bar(aes(x = level, y = value, fill = variable), stat = "identity") +
    geom_point(aes(x = level, y  = value), data = Var_Split, size = 1) +
    labs(x = "", y = "Proportion variation explained") +
    scale_fill_manual(values = c("blue", "darkorange"), name = "Source of variation") +
    scale_x_discrete(drop = FALSE) +
    facet_wrap(~Trait_fancy, nrow = 3, labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 15),
          legend.position = "top")

}

