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



#### difference between trait effects on Cflux when taking into account ITV or not
# z2 <- Flux_and_Traits %>%
#   select(Site, PlotID, Treatment, NEE_ln, ER_ln, GPP700, ITV_C:No_ITV_SLA) %>%
#   rename(NEE = NEE_ln, GPP = GPP700, Reco = ER_ln) %>%
#   group_by(Site, Treatment) %>%
#   gather(key = Trait, value = Tvalue, ITV_C:No_ITV_SLA) %>%
#   mutate(Trait = str_replace(Trait, "No_ITV", "NoITV")) %>%
#   separate(Trait, c("ITV", "Trait"), sep = "_") %>%
#   gather(key = response, value = Cflux, -c(Site, PlotID, Treatment, ITV, Trait, Tvalue)) %>%
#   group_by(response, Trait, ITV) %>%
#   mutate(Tvalue = scale(Tvalue)[,1]) %>%
#   do(tidy(lm(Cflux ~ Tvalue , data=. ))) %>%   # not sure if Site should be in here
#   filter(!term == "(Intercept)") %>%
#   mutate(lower = (estimate - std.error*1.96),
#          upper = (estimate + std.error*1.96))
#
#
# #postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure1.eps", width = 8.3, height = 5.8)
# ggplot(z2, aes(x =Trait, y = estimate, shape = response,
#                col = ITV, ymin = lower, ymax = upper)) +
#   geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
#   geom_hline(yintercept = 0, linetype = "solid", color="grey") +
#   #geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
#   geom_point(position = position_dodge(width = 0.5), size = 3) +
#   #scale_shape_manual(labels = c("NEE", "GPP", "Reco"), values = c(22, 24,21)) +
#   #scale_fill_manual(labels = c("NEE", "GPP", "Reco"), values = c("red", "grey70", "black")) +
#   #scale_x_discrete(limits=c( "Wvar_CN", "Wvar_N", "Wvar_C", "Wvar_VH", "Wvar_SLA", "Wvar_LDMC", "Wvar_Lth", "Wvar_LA", "CWM_CN", "CWM_N", "CWM_C", "CWM_VH", "CWM_SLA", "CWM_LDMC", "CWM_Lth", "CWM_LA", "Richness", "Evenness", "Diversity", "VegetationHeight", "Temp.C", "P.mm"), labels=c( "Wvar_CN" = "LCNvar", "Wvar_N" = "LNvar", "Wvar_C"= "LCvar", "Wvar_Height"= "Hvar", "Wvar_SLA" = "SLAvar", "Wvar_LDMC" = "LDMCvar", "Wvar_Lth" = "LTvar", "Wvar_LA" = "LAvar", "Wmean_CN" = "LCNmean", "Wmean_N" = "LNmean", "Wmean_C"= "LCmean", "Wmean_Height"= "Hmean", "Wmean_SLA" = "SLAmean", "Wmean_LDMC" = "LDMCmean", "Wmean_Lth" = "LTmean", "Wmean_LA" = "LAmean", "VegetationHeight" = "Vegetation Height", "Temp.C" = "Temperature", "P.mm" = "Precipitation"))+
#   #labs(x= "             CWV traits                CWM traits             Veg structure      Climate")+
#   facet_grid(~response)+ #, labeller=labeller(response = labels)
#   theme(axis.title.x=element_text(size = 12), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 12), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"))+
#   coord_flip()
