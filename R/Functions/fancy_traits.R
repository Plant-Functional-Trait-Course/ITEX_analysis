#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){

  dat <- dat %>%

    # rename
   mutate(Trait = factor(Trait,
                         levels = c("Plant_Height_cm", "Dry_Mass_g", "Leaf_Area_cm2", "SLA_cm2_g", "Leaf_Thickness_mm", "LDMC", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent")),
          Trait_fancy = fct_recode(Trait,
                          "SLA~(cm^2*g^{-1})" = "SLA_cm2_g",
                          "LDMC~(gg^{-1})" = "LDMC",
                          "Leaf~Area~(cm^2)" = "Leaf_Area_cm2",
                          "Leaf~Thickness~(mm)" = "Leaf_Thickness_mm",
                          "N~('%')" = "N_percent",
                          "C~('%')" = "C_percent",
                          "P~('%')" = "P_Ave",
                          "C:N" = "CN_ratio",
                          "δC13~('‰')" = "dC13_percent",
                          "δN15~('‰')" = "dN15_percent",
                          "Dry~Mass~(g)" = "Dry_Mass_g",
                          "Plant~Height~(cm)" = "Plant_Height_cm")
          )


  return(dat)
}


# mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "`SLA`*` `*(cm^2/g)", "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))

