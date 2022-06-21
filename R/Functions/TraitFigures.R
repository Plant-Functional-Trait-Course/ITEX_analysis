#### TRAIT FIGURES ####


## Figure 2 trait mean ##
make_trait_mean_figure <- function(anova_trait_tidy, traitMean){

  anova_text_trait <- anova_trait_tidy %>%
    ungroup() %>%
    mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>%
    mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)", "`SLA`*` `*(cm^2/g)", "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>%
    mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>%
    filter(term != "Residuals") %>%
    mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>%
    mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>%
    pivot_wider(id_cols = Trait, names_from = term, values_from = test) %>%
    mutate(T = ifelse(is.na(T), "", T)) %>%
    mutate(H = ifelse(is.na(H), "", H)) %>%
    mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H))))


  trait_mean_plot <- traitMean %>%
    ungroup() %>%  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>%
    mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'",  "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)", "`SLA`*` `*(cm^2/g)", "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )),
           Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"))  %>%
    filter(PlotID != "CAS-4", PlotID != "CAS-9", PlotID != "CAS-10", PlotID != "CAS-6") %>%
    group_by(Trait) %>%
    mutate(y_max = max(mean), y_min = min(mean)) %>%
    ggplot() +
    geom_boxplot(aes(x = Site, y = mean, fill = Treatment)) +
    scale_fill_manual(values = c("darkgray", "red")) +
    geom_blank(aes(y = y_max + 0.1*abs(y_max))) +
    ylab("CWM Trait Value") +
    xlab("Habitat Type") +
    theme_classic() +
    theme(text = element_text(size = 15),
          strip.background = element_blank(),
          legend.position = "top") +
    facet_wrap(~Trait, scales = "free", labeller = label_parsed) +
    geom_text(aes(label = text, x = 0, y = Inf, hjust = -0.15, vjust = 1), size = 3.5, color = "black",  data = anova_text_trait)

  return(trait_mean_plot)
}


#### Figure 3: Difference fixed and specific mean (ITV) ####

make_itv_figure <- function(traitMean){

  traitMean <- traitMean %>%
    mutate(itv_diff = mean-mean_noitv,
           Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"))

  t_test_itv <- traitMean %>%
    group_by(Trait, Site, Treatment) %>%
    summarise(P = t.test(itv_diff, mu = 0)$p.value,
              Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
              MaxWidth = max(itv_diff))%>%
    ungroup() %>%
    mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>%
    mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'",  "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)", "`SLA`*` `*(cm^2/g)", "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))

  itv_plot <- traitMean %>%
    mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>%
    mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "`SLA`*` `*(cm^2/g)", "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>%
    ggplot() +
    geom_boxplot(aes(x = Site, y = itv_diff, fill = Treatment)) +
    geom_hline(aes(yintercept = 0)) +
    geom_blank(aes(x = Site, y = itv_diff + itv_diff*0.6)) +
    geom_text(aes(label = Sig, y = Inf, x = Site, group = Treatment, vjust = 1), position = position_dodge(0.75), data = t_test_itv, size = 4.5) +
    scale_fill_manual(values = c("darkgray", "red")) +
    labs(x = "Habitat Type", y = "Specific - fixed (no ITV) trait mean") +
    facet_wrap(~Trait, scales = "free_y", labeller = label_parsed) +
    theme_classic() +
    theme(text = element_text(size = 15),
          strip.background = element_blank(),
          legend.position = "top")

  return(itv_plot)

}

#### Fig S5 Trait PCA ####


make_trait_pca_figure <- function(trait_pca_info, pca_res, col, tag){

  pca_points <- cbind(trait_pca_info, pca_res$x) %>%
    rename("Habitat" = "Site") |>
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"))
  pca_arrows <- pca_res$rotation

  if(pca_points |> distinct(Habitat) |> count() > 1)
    {
    pca_plot <- autoplot(pca_res, data = pca_points,
                         shape = 'Treatment',
                         colour = "Habitat",
                         size = 3,
                         loadings = TRUE,
                         loadings.colour = 'grey80',
                         loadings.label = TRUE,
                         loadings.label.size = 4,
                         loadings.label.colour = "grey50",
                         loadings.label.repel = TRUE,
                         parse = TRUE) +
      scale_color_manual(values = c("blue", "forestgreen", "orange")) +
      scale_shape_manual(values = c(1, 17)) +
      labs(tag = tag) +
      theme_classic() +
      theme(legend.position = "top",
            text = element_text(size = 12))


  }
  else{

    pca_plot <- autoplot(pca_res, data = pca_points,
                         shape = 'Treatment',
                         colour = col,
                         size = 2,
                         loadings = TRUE,
                         loadings.colour = 'grey80',
                         loadings.label = TRUE,
                         loadings.label.size = 2.5,
                         loadings.label.colour = "grey50",
                         loadings.label.repel = TRUE,
                         parse = TRUE) +
      scale_shape_manual(values = c(1, 17)) +
      labs(tag = tag) +
      theme_classic() +
      theme(legend.position = "none",
            text = element_text(size = 10))

  }

  return(pca_plot)
}



## Figure S6 Trait variance partitioning
make_intra_vs_inter_figure <- function(var_split_exp, var_split){

  varpart_graph <- var_split_exp %>%
    mutate(level = trimws(level)) %>%
    filter(RelSumSq.Turnover < 999) %>%
    rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>%
    mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>%
    gather(key = variable, value = value, -trait, -level) %>%
    filter(variable != "Covariation", level != "Total", variable != "Total") %>%
    mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>%
    mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid"))) %>%
    mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>%
    mutate(trait = factor(trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)", "`SLA`*` `*(cm^2/g)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>%
    ggplot() +
    geom_bar(aes(x = level, y = value, fill = variable), stat = "identity") +
    geom_point(aes(x = level, y  = value), data = var_split, size = 1) +
    facet_wrap(~trait, nrow = 3, labeller = label_parsed) +
    theme_classic() +
    theme(text = element_text(size = 15), legend.position = "top",
          strip.background = element_blank()) +
    xlab(NULL) +
    ylab("Proportion Variation Explained") +
    scale_fill_manual(values = c("blue", "darkorange"), name = "Source of Variation") +
    scale_x_discrete(drop = FALSE)

}

