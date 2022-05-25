# Figure plan

FigurePlan <- list(

  ### Community Figures

  # Fig 1 NMDS ordination
  tar_target(
    name = Fig_1_CommunityOrdination,
    command = make_ordination(NMDS_output)
  ),

  # Fig S3 metric change
  tar_target(
    name = Fig_S3_metric_change,
    command = community_metrics_figure(Comm_Anova_tidy, Comm_Metric_Change, Comm_t_Test)
  ),

  # Fig S4 Canopy Height
  tar_target(
    name = Fig_S4_CanopyHeight,
    command = make_height_figure(Height)
  ),

  # Fig S4 metric change supplement
  # tar_target(
  #   name = Fig_S4_metric_change_supp,
  #   command = community_metrics_figure_supp(Comm_Anova_tidy, Comm_Metric_Change, Comm_t_Test_Supp)
  # ),

  # Fig S5 metric change over time
  # tar_target(
  #   name = Fig_S5_metric_time,
  #   command = metric_time_figure(CommResp)
  # ),

  ### Trait Figures

  # Fig S5 Trait PCA
  tar_target(
    name = Fig_2_pca_plot,
    command = {

      pDCS <- make_trait_pca_figure(trait_pca_all[[3]], trait_pca_all[[1]],
                                    col = c("blue", "forestgreen", "orange"),
                                    tag = "A")
     pD <- make_trait_pca_figure(trait_pca_DH[[3]], trait_pca_DH[[1]],
                                 col = "orange",
                                 tag = "B")
     pC <- make_trait_pca_figure(trait_pca_CH[[3]], trait_pca_CH[[1]],
                                 col = "forestgreen",
                                 tag = "C")
     pS <- make_trait_pca_figure(trait_pca_SB[[3]], trait_pca_SB[[1]],
                                 col = "blue",
                                 tag = "D")

     pDCS / (pS | pC | pD) + plot_layout(height = c(3, 1))

    }


  ),

  # Fig 3 Trait mean
  tar_target(
    name = Fig_3_trait_mean,
    command = make_trait_mean_figure(Anova_Trait_Tidy, Trait_Mean)
  ),

  # Fig 4 ITV
  tar_target(
    name = Fig_4_itv_plot,
    command = make_itv_figure(Trait_Mean)
  ),

  # Fig S7 Inter vs intra
  tar_target(
    name = Fig_S7_varpart_graph,
    command = make_intra_vs_inter_figure(Var_Split_Exp, Var_Split)
  ),

  # Climate plot
  tar_target(
    name = Fig_S1_ClimatePlot,
    command = make_climate_figure(Monthly_Temp, Daily_Climate)
  )

)





