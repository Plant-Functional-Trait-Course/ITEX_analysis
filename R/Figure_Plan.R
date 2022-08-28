# Figure plan

FigurePlan <- list(

  ### COMMUNITY FIGURE

  # Fig 1 PCA species ordination
  tar_target(
    name = Fig_1_Community_PCA,
    command = make_sp_pca_figure(pca_sp, pca_sp_sb, pca_sp_ch, pca_sp_dh)
  ),

  # Fig S4 Canopy Height
  tar_target(
    name = Fig_S3_CanopyHeight,
    command = make_height_figure(Height)
  ),

  # Fig S3 metric change
  tar_target(
    name = Fig_S4_metric_change,
    command = community_metrics_figure(Comm_Anova_tidy, Comm_Metric_Change, Comm_t_Test)
  ),


  ### TRAIT FIGURES

  # Fig 2 Trait PCA
  tar_target(
    name = Fig_2_Trait_PCA,
    command =  make_trait_pca_figure(trait_pca_all, trait_pca_SB, trait_pca_CH, trait_pca_DH)

  ),

  # Fig 3 Specific vs fixed mean ITV
  tar_target(
    name = Fig_3_specific_fixed,
    command = make_itv_figure(Trait_Mean)
  ),

  # Fig S5 Trait mean
  tar_target(
    name = Fig_S5_trait_mean,
    command = make_trait_mean_figure(Anova_Trait_Tidy, Trait_Mean)
  ),

  # Fig S6 Variance partitioning
  tar_target(
    name = Fig_S6_varpart,
    command = make_intra_vs_inter_figure(Var_Split_Exp, Var_Split)
  ),


  ##  Climate figure ##
  tar_target(
    name = Fig_S1_ClimatePlot,
    command = make_climate_figure(Monthly_Temp, Daily_Climate)
  ),

  # Fig S2 Microclimate
  tar_target(
    name = Fig_S2_Microclimate,
    command = make_soil_microclimate_figure(Flux_and_Traits)
  )

)





