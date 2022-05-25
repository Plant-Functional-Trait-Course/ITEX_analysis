# Flux plan

FluxPlan <- list(

  # Calculate GPP
  tar_target(
    name = ITEX.data.post.calcs,
    command = calc_GPP(ITEX.data.pre.calcs)
  ),

  # Standardize fluxes
  tar_target(
    name = Standard_Fluxes,
    command = standardize_fluxes(CommResp, Height, ITEX.data.post.calcs)
  ),

  # Join with traits
  tar_target(
    name = Flux_and_Traits,
    command = flux_and_trait(Trait_Mean, Standard_Fluxes)
  ),


  # Model selection
  tar_target(
    name = Trait_Model_Output,
    command = trait_model_selelction(Flux_and_Traits)
  ),

  tar_target(
    name = Model_Output,
    command = model_selection(Flux_and_Traits)
  ),

  # results
  tar_target(
    name = Results_Model_Selection,
    command = model_selection_results(Flux_and_Traits)
  ),

  # Fig 4 Flux Figure
  tar_target(
    name = Fig_4_Fluxes,
    command = make_flux_figure(Trait_Model_Output, Model_Output)
  ),

  # Fig S2 Microclimate
  tar_target(
    name = Fig_S2_Microclimate,
    command = make_soil_microclimate_figure(Flux_and_Traits)
  ),

  # Fig S7 Mean fluxes
  tar_target(
    name = Fig_S7_Mean_fluxes,
    command = make_flux_mean_figures(Flux_and_Traits)
  ),

  # Fig S8 Effect size plot
  tar_target(
    name = Fig_S8_Effect_size,
    command = make_effect_size_figure(Flux_and_Traits)
  )



)
