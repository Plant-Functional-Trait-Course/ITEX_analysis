#### NMDS ORDINATION ####
nmds_ordination <- function(comm){
  set.seed(32)

  comm <- comm %>%
    mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming"))


  # SNOWBED (SB)
  comm_fat_SB <- comm %>%
    select(-c(FunctionalGroup:Flag)) %>%
    arrange(Year) %>%
    spread(key = Taxon, value = Abundance, fill = 0) %>%
    filter(Site == "SB")

  comm_fat_spp_SB <- comm_fat_SB %>% select(-(Year:PlotID))

  NMDS_SB <- metaMDS(comm_fat_spp_SB, noshare = TRUE, try = 30)

  fNMDS_SB <- fortify(NMDS_SB) %>%
    filter(Score == "sites") %>%
    bind_cols(comm_fat_SB %>% select(Year:PlotID))


  # CASSIOPE HEATH (CH)
  comm_fat_CH <- comm %>%
    select(-c(FunctionalGroup:Flag)) %>%
    arrange(Year) %>%
    spread(key = Taxon, value = Abundance, fill = 0) %>%
    filter(Site == "CH")

  comm_fat_spp_CH <- comm_fat_CH %>% select(-(Year:PlotID))

  NMDS_CH <- metaMDS(comm_fat_spp_CH, noshare = TRUE, try = 100)

  fNMDS_CH <- fortify(NMDS_CH) %>%
    filter(Score == "sites") %>%
    bind_cols(comm_fat_CH %>% select(Year:PlotID))


  # DRYAS HEATH
  comm_fat_DH <- comm %>%
    select(-c(FunctionalGroup:Flag)) %>%
    arrange(Year) %>%
    spread(key = Taxon, value = Abundance, fill = 0) %>%
    filter(Site == "DH")

  comm_fat_spp_DH <- comm_fat_DH %>% select(-(Year:PlotID))

  NMDS_DH <- metaMDS(comm_fat_spp_DH, noshare = TRUE, try = 100)

  fNMDS <- fortify(NMDS_DH) %>%
    filter(Score == "sites") %>%
    bind_cols(comm_fat_DH %>% select(Year:PlotID)) %>%
    bind_rows(fNMDS_SB, fNMDS_CH)

  return(fNMDS)

}


# Check if community composition changes in treatments and over time
NMDS_analysis <- function(NMDS_output){

  NMDS_result <- NMDS_output %>%
    nest(data = -Site) %>%
    mutate(
      fit = map(data, ~ lm(NMDS1 ~ Treatment * Year, data = .x)),
      tidied = map(fit, tidy)
    ) %>%
    unnest(tidied) %>%
    filter(p.value < 0.05,
           term != "(Intercept)")

  return(NMDS_result)
}


### Figure 1 NMDS ordination
make_ordination <- function(NMDS_data){

  plot_annotation <- tibble(Site = c("SB", "CH", "DH"),
                            label = c("Year*", "", "Year*"))
  CommunityOrdination <- NMDS_data %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
    ggplot(aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"), shape = Treatment)) +
    geom_path(aes(linetype = Treatment, group = PlotID)) +
    coord_equal() +
    scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_shape_manual(values = c(1, 17)) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    labs(x = "NMDS axis 1", y = "NMDS axis 2") +
    geom_text(data = plot_annotation, aes(x = 0.5, y = 0.5, label = label)) +
    facet_grid(~ fct_relevel(Site, "SB", "CH", "DH")) +
    theme_bw()

  return(CommunityOrdination)

}
