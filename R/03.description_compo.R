################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# 03.description_compo.R
#
# Script with functions to show output for composition of samples
################################################################################


################################ TABLES ########################################
############### across all samples (with no averaging per species) #############

#'
#'
#'
#'
#'
# function to generate table of elemental composition per species
summarise_compo_fish_across_samples <- function(res_fish_tib, 
                                     file_name # should be explicit whether
                                     # the tibble is with or without loq replaced
                                     ) {
  
  table_all_samples <- res_fish_tib |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Ag = as.numeric(Ag)) |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(min = round(min(concentration_mg_kg_dw, 
                                     na.rm = TRUE), 3), 
                     `2.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                   probs = c(0.025), 
                                                   na.rm = TRUE), 3),
                     mean = round(mean(concentration_mg_kg_dw, 
                                       na.rm = TRUE), 3),
                     `97.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                    probs = c(0.975), 
                                                    na.rm = TRUE), 3),
                     max = round(max(concentration_mg_kg_dw, 
                                     na.rm = TRUE), 3), 
                     sd = round(sd(concentration_mg_kg_dw, 
                                   na.rm = TRUE), 3), 
                     cv = round(sd(concentration_mg_kg_dw, 
                                   na.rm = TRUE)/mean, 3)) |>
    tidyr::pivot_longer(cols = c(min, `2.5p_quant`, mean, `97.5p_quant`,
                                 max, sd, cv), 
                        names_to = "stat_variable", 
                        values_to = "stat_value") |>
    tidyr::pivot_wider(names_from = "Nutrient", 
                       values_from = "stat_value")
  
  openxlsx::write.xlsx(table_all_samples, 
                       file = paste0("output/02.compo-nutrient-patterns/summary_fish_compo_all_samples_", 
                                     file_name, 
                                     ".xlsx"))
  
  table_all_samples
  
}

############### across all species (with averaging per species before) #########

#'
#'
#'
#'
#'
# function to generate table of elemental composition per species
summarise_compo_fish_across_sp <- function(res_fish_tib, 
                                     file_name # should be explicit whether
                                     # the tibble is with or without loq replaced
) {
  
  table_all_sp <- res_fish_tib |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Ag = as.numeric(Ag)) |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Nutrient, Species) |>
    dplyr::summarise(mean_sp = round(mean(concentration_mg_kg_dw), 3)) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(min = round(min(mean_sp, 
                                     na.rm = TRUE), 3), 
                     `2.5p_quant` = round(quantile(mean_sp, 
                                                   probs = c(0.025), 
                                                   na.rm = TRUE), 3),
                     mean = round(mean(mean_sp, 
                                       na.rm = TRUE), 3),
                     `97.5p_quant` = round(quantile(mean_sp, 
                                                    probs = c(0.975), 
                                                    na.rm = TRUE), 3),
                     max = round(max(mean_sp, 
                                     na.rm = TRUE), 3), 
                     sd = round(sd(mean_sp, 
                                   na.rm = TRUE), 3), 
                     cv = round(sd(mean_sp, 
                                   na.rm = TRUE)/mean, 3)) |>
    tidyr::pivot_longer(cols = c(min, `2.5p_quant`, mean, `97.5p_quant`,
                                 max, sd, cv), 
                        names_to = "stat_variable", 
                        values_to = "stat_value") |>
    tidyr::pivot_wider(names_from = "Nutrient", 
                       values_from = "stat_value")
  
  openxlsx::write.xlsx(table_all_sp, 
                       file = paste0("output/02.compo-nutrient-patterns/summary_fish_compo_all_sp_", 
                                     file_name, 
                                     ".xlsx"))
  
  table_all_sp
  
}


######################## averaging per species  #################################

#'
#'
#'
#'
#'
# function to generate table of elemental composition per species
summarise_compo_fish_per_sp <- function(res_fish_tib, 
                                        file_name # should be explicit whether
                                           # the tibble is with or without loq replaced
) {
  
  table_compo_sp <- res_fish_tib |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Ag = as.numeric(Ag)) |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co",
                                               "Sr", "Cd", "Pb", "Ag"))) |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Family, Nutrient, Species) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample),
                     min = round(min(concentration_mg_kg_dw, 
                                     na.rm = TRUE), 3), 
                     `2.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                   probs = c(0.025), 
                                                   na.rm = TRUE), 3),
                     mean = round(mean(concentration_mg_kg_dw, 
                                       na.rm = TRUE), 3),
                     `97.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                    probs = c(0.975), 
                                                    na.rm = TRUE), 3),
                     max = round(max(concentration_mg_kg_dw, 
                                     na.rm = TRUE), 3), 
                     sd = round(sd(concentration_mg_kg_dw, 
                                   na.rm = TRUE), 3), 
                     cv = round(sd(concentration_mg_kg_dw, 
                                   na.rm = TRUE)/mean, 3)) |>
    tidyr::pivot_longer(cols = c(min, `2.5p_quant`, mean, `97.5p_quant`,
                                 max, sd, cv), 
                        names_to = "stat_variable", 
                        values_to = "stat_value") |>
    tidyr::pivot_wider(names_from = "Nutrient", 
                       values_from = "stat_value")
  
  openxlsx::write.xlsx(table_compo_sp, 
                       file = paste0("output/02.compo-nutrient-patterns/summary_fish_compo_per_sp_", 
                                     file_name, 
                                     ".xlsx"))
  
  table_compo_sp
  
}


#' Title
#'
#' @param res_fish_tib 
#'
#' @return
#' @export
#'
#' @examples
table_nut_fish_compo_relative <- function(res_fish_tib_sp) {
  
  # with mean per species, order based on families
  table <- res_fish_tib_sp |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                    Ag + Pb) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co, 
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut) |>
    dplyr::select(-c(conc_mg_kg_dw, sum_nut)) |>
    tidyr::pivot_wider(values_from = conc_relative,
                       names_from = Nutrient)
  
  openxlsx::write.xlsx(table,
                       file ="output/02.compo-nutrient-patterns/fish_sp_relative_nut_fraction.xlsx")
  
}

######################## PLOTS #################################################

#'
#'
#'
#'
#' keep only mean from the summary table
#' and add other functional info such as habitat and if 
#' species identified in the diet of any marine predator from the area before
mean_per_species <- function(clean_summary_table) {
  clean_summary_table |>
    dplyr::filter(stat_variable == "mean") |>
    # add information that could be used to analyse results
    dplyr::mutate(Habitat = dplyr::case_when(Species %in% c("Bathylagus tenuis",
                                                            "Poromitra crassiceps", 
                                                            "Nansenia antarctica",
                                                            "Electrona antarctica",
                                                            "Electrona carlsbergi", 
                                                            "Electrona subaspera",
                                                            "Gymnoscopelus bolini", 
                                                            "Gymnoscopelus braueri", 
                                                            "Gymnoscopelus fraseri",
                                                            "Krefftichthys anderssoni",
                                                            "Protomyctophum andriashevi",
                                                            "Protomyctophum bolini", 
                                                            "Protomyctophum choriodon", 
                                                            "Protomyctophum tenisoni",
                                                            "Luciosudis normani",
                                                            "Arctozenus risso",
                                                            "Notolepis coatsi",
                                                            "Idiacanthus atlanticus",
                                                            "Stomias spp.",
                                                            "Melanostigma gelatinosum") ~ "Mesopelagic",
                                             Species %in% c("Gymnoscopelus nicholsi",
                                                            "Gymnoscopelus piabilis") ~ "Mesopelagic/epibenthic",
                                             Species %in% c("Champsocephalus gunnari", 
                                                            "Paradiplospinus gracilis",
                                                            "Muraenolepis marmorata",
                                                            "Lepidonotothen squamifrons",
                                                            "Lindbergichthys mizops") ~ "Benthopelagic", 
                                             Species %in% c("Mancopsetta maculata",
                                                            "Channichthys rhinoceratus", 
                                                            "Dissostichus eleginoides",
                                                            "Gobionotothen acuta") ~ "Demersal", 
                                             Species %in% c("Bathydraco antarcticus",
                                                            "Echiodon cryomargarites",
                                                            "Macrourus carinatus") ~ "Bathydemersal"), 
                  confirmed_forage_sp = dplyr::case_when(Species %in% c("Mancopsetta maculata", 
                                                                        "Champsocephalus gunnari",
                                                                        "Channichthys rhinoceratus", 
                                                                        "Paradiplospinus gracilis",
                                                                        "Macrourus carinatus",
                                                                        "Nansenia antarctica",
                                                                        "Muraenolepis marmorata", 
                                                                        "Electrona antarctica",
                                                                        "Electrona carlsbergi", 
                                                                        "Electrona subaspera",
                                                                        "Gymnoscopelus bolini", 
                                                                        "Gymnoscopelus braueri", 
                                                                        "Gymnoscopelus fraseri",
                                                                        "Gymnoscopelus nicholsi",
                                                                        "Gymnoscopelus piabilis",
                                                                        "Krefftichthys anderssoni",
                                                                        "Protomyctophum andriashevi",
                                                                        "Protomyctophum bolini", 
                                                                        "Protomyctophum choriodon", 
                                                                        "Protomyctophum tenisoni",
                                                                        "Dissostichus eleginoides",
                                                                        "Gobionotothen acuta",
                                                                        "Lepidonotothen squamifrons",
                                                                        "Lindbergichthys mizops",
                                                                        "Melanostigma gelatinosum") ~ "yes",
                                                         TRUE ~ "no")) |>
    dplyr::select(c(Habitat, Family, Species, n, confirmed_forage_sp,
                  Ca, P, Na, K, Mg, 
                  Fe, Zn, Cu, Mn,
                  As, Se, Ni, Co, 
                  Sr, Cd, Pb, Ag))
  
}

#'
#'
#'
#'
#'
# function to compare composition of all fish analized 
# density plot normalised per type
density_plot_all_nut <- function(res_fish_tib_per_sp) {
  #options(scipen = 999)
  
  # simple density plots
  res_fish_tib_per_sp |> 
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co",
                                               "Sr", "Cd", "Pb", "Ag"))) |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_kg_dw,
                                 fill = Nutrient, 
                                 color = Nutrient)) +
    ggplot2::geom_density(alpha = 0.8) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", ncol = 4) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#26432FFF", "#D98594FF",
                                          "#2e276a", "#5c4d73",
                                          "#DE7862FF","#D8AF39FF",
                                          "#583B2BFF")) +
    ggplot2::scale_color_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                           "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                           "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                           "#92BBD9FF", "#26432FFF", "#D98594FF",
                                           "#2e276a", "#5c4d73",
                                           "#DE7862FF","#D8AF39FF",
                                           "#583B2BFF")) +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, 
                                                       hjust = 0.8), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 14, 
                                                      face = "bold", 
                                                      color = "white"),
                   strip.background = ggplot2::element_rect(fill = "gray30"),
                   panel.spacing.x = ggplot2::unit(0.5, "cm"),
                   legend.position = "none"
    )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/densityplot_all_nut.jpg",
                  scale = 1,
                  height = 6, width = 10.5
  )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/densityplot_all_nut.svg",
                  scale = 1,
                  height = 6, width = 10.5
  )
  
  # density ridge plot
  res_fish_tib_per_sp |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "mean_sp_conc_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co",
                                               "Sr", "Cd", "Pb", "Ag"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(norm_conc = (mean_sp_conc_mg_kg_dw - min(mean_sp_conc_mg_kg_dw))/
                    (max(mean_sp_conc_mg_kg_dw) - min(mean_sp_conc_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = norm_conc,
                                 y = Nutrient,
                                 fill = Nutrient, 
                                 color = Nutrient)) +
    ggridges::geom_density_ridges(alpha = 0.6, 
                                  scale = 2) +
    #ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#26432FFF", "#D98594FF",
                                          "#2e276a", "#5c4d73",
                                          "#DE7862FF","#D8AF39FF",
                                          "#583B2BFF")) +
    ggplot2::scale_color_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                           "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                           "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                           "#92BBD9FF", "#26432FFF", "#D98594FF",
                                           "#2e276a", "#5c4d73",
                                           "#DE7862FF","#D8AF39FF",
                                           "#583B2BFF")) +
    ggplot2::xlab("Nutrient concentration normalized between 0 and 1") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   axis.title.x = ggplot2::element_text(size = 18, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 15, face = "bold"),
                   panel.spacing.x = ggplot2::unit(0.5, "cm"),
                   legend.position = "none"
    )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/density-ridge-plot_all_nut_norm.jpg",
                  scale = 1,
                  height = 8, width = 7
  )
  
}

#' Title
#'
#' @param res_fish_tib 
#'
#' @return
#' @export
#'
#' @examples
barplot_nut_fish_compo_relative <- function(res_fish_tib_per_sp) {
  
  # with mean per species, order based on families
  res_fish_tib_per_sp |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                    Ag + Pb) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co, 
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Sr", "Fe", "Zn",
                                               "Cu", "Mn", "As", "Ni", "Se",
                                               "Cd", "Co", "Pb", "Ag")),
                  micro_macro = factor(dplyr::case_when(
                    Nutrient %in% c("Ca", "P", "Na", 
                                    "K", "Mg") ~ "Ca, P, Na, K, Mg", 
                    Nutrient %in% c("Fe", "Zn", "Sr") ~ "Sr, Fe, Zn", 
                    Nutrient %in% c("Cu", "Mn", "As", "Ni", "Se") ~ "Cu, Mn, As, Ni, Se",  
                    Nutrient %in% c("Cd", "Co", "Pb", "Ag") ~ "Cd, Co, Pb, Ag"), 
                    levels = c("Ca, P, Na, K, Mg", 
                               "Sr, Fe, Zn", 
                               "Cu, Mn, As, Ni, Se", 
                               "Cd, Co, Pb, Ag"))) |> 
    dplyr::mutate(sp_short = dplyr::case_when(Species == "Stomias spp." ~ "*Stomias* spp.",
                                              TRUE ~ paste0("*",
                                                            stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2], 
                                                            "*"))) |>
    dplyr::mutate(sp_short = factor(sp_short, 
                                    levels = c(# order by family
                                      #Achiropsettidae
                                      "*M. maculata*",
                                      #Bathydraconidae
                                      "*B. antarcticus*",
                                      #Bathylagidae    
                                      "*B. tenuis*",
                                      #Carapidae
                                      "*E. cryomargarites*",
                                      #Channichthyidae
                                      "*C. gunnari*", 
                                      "*C. rhinoceratus*", 
                                      #Gempylidae
                                      "*P. gracilis*",
                                      #Macrouridae
                                      "*M. carinatus*",
                                      #Melamphaidae
                                      "*P. crassiceps*",  
                                      #Microstomatidae
                                      "*N. antarctica*",  
                                      #Muraenolepididae  
                                      "*M. marmorata*",  
                                      #Myctophidae  
                                      "*E. antarctica*",
                                      "*E. carlsbergi*", 
                                      "*E. subaspera*",  
                                      "*G. bolini*",
                                      "*G. braueri*",     
                                      "*G. fraseri*",   
                                      "*G. nicholsi*",     
                                      "*G. piabilis*",  
                                      "*K. anderssoni*",
                                      "*P. andriashevi*",    
                                      "*P. bolini*", 
                                      "*P. choriodon*",
                                      "*P. tenisoni*",  
                                      #Notosudidae         
                                      "*L. normani*",
                                      #Nototheniidae
                                      "*D. eleginoides*", 
                                      "*G. acuta*",  
                                      "*L. squamifrons*",    
                                      "*L. mizops*",
                                      #Paralepididae
                                      "*A. risso*", 
                                      "*N. coatsi*", 
                                      #Stomiidae  
                                      "*I. atlanticus*",
                                      "*Stomias* spp.",
                                      #Zoarcidae
                                      "*M. gelatinosum*"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = sp_short, y = conc_relative, 
                                   fill = Nutrient), 
                      stat = "identity", 
                      position = ggplot2::position_stack()) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#26432FFF", "#D98594FF",
                                          "#2e276a", "#5c4d73",
                                          "#DE7862FF","#D8AF39FF",
                                          "#583B2BFF")) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
                        ncol = 1, 
                        strip.position = "left") +
    ggplot2::ggtitle("") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggtext::element_markdown(size = 9, 
                                                          angle = 35, 
                                                          hjust = 1), 
                   axis.title.x = ggplot2::element_text(size = 12, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 12, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 12),
                   strip.background = ggplot2::element_rect(fill = "lightgrey", 
                                                            colour = "white"),
                   strip.placement = "outside", 
                   legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 11),
                   legend.title = ggplot2::element_text(size = 11, 
                                                        face = "bold")
    )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_order_family.jpg",
                  scale = 1,
                  height = 7, width = 8
  )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_order_family.svg",
                  scale = 1,
                  height = 7, width = 8
  )
  
  # ## other orders trials
  # # with mean per species, order based on relative Ca concentrations
  # res_fish_tib_per_sp |>
  #   dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
  #                   Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
  #                   Ag + Pb) |>
  #   tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
  #                                Fe, Zn, Sr, Cu, Mn, Se,
  #                                Ni, Cd, As, Co, 
  #                                Pb, Ag), 
  #                       names_to = "Nutrient", 
  #                       values_to = "conc_mg_kg_dw") |> 
  #   dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
  #                 Nutrient = factor(Nutrient, 
  #                                   levels = c("Ca", "P", "Na", "K", "Mg", 
  #                                              "Fe", "Zn", "Cu", "Mn",
  #                                              "As", "Se", "Ni", "Co",
  #                                              "Sr", "Cd", "Pb", "Ag")),
  #                 micro_macro = factor(dplyr::case_when(
  #                   Nutrient %in% c("Ca", "P", "Na", 
  #                                   "K", "Mg") ~ "Ca, P, Na, K, Mg", 
  #                   Nutrient %in% c("Fe", "Zn", "Sr") ~ "Sr, Fe, Zn", 
  #                   Nutrient %in% c("Cu", "Mn", "As", "Ni", "Se") ~ "Cu, Mn, As, Ni, Se",  
  #                   Nutrient %in% c("Cd", "Co", "Pb", "Ag") ~ "Cd, Co, Pb, Ag"), 
  #                   levels = c("Ca, P, Na, K, Mg", 
  #                              "Sr, Fe, Zn", 
  #                              "Cu, Mn, As, Ni, Se", 
  #                              "Cd, Co, Pb, Ag"))) |> 
  #   dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias spp.") ~ Species,
  #                                             TRUE ~ paste0(stringr::str_sub(Species, 
  #                                                                            start = 1, end = 1),
  #                                                           ". ",
  #                                                           stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
  #   dplyr::mutate(sp_short = factor(sp_short, 
  #                                   levels = c(# order is relative to Ca concentration as
  #                                     # determined in work-in-progress/set-up-the-rest
  #                                     "B. antarcticus",
  #                                     "L. mizops",
  #                                     "M. maculata",
  #                                     "G. acuta",       
  #                                     "P. bolini",
  #                                     "P. tenisoni",   
  #                                     "G. bolini",
  #                                     "P. andriashevi",
  #                                     "L. squamifrons",
  #                                     "E. carlsbergi",      
  #                                     "G. fraseri",
  #                                     "E. subaspera",       
  #                                     "G. piabilis",
  #                                     "G. braueri",     
  #                                     "P. choriodon",
  #                                     "C. rhinoceratus", 
  #                                     "M. carinatus",
  #                                     "G. nicholsi",    
  #                                     "E. antarctica",
  #                                     "D. eleginoides",
  #                                     "P. gracilis",
  #                                     "P. crassiceps",      
  #                                     "M. marmorata",
  #                                     "C. gunnari",   
  #                                     "Stomias spp.",
  #                                     "N. coatsi",
  #                                     "K. anderssoni",
  #                                     "M. gelatinosum",  
  #                                     "E. cryomargarites",
  #                                     "A. risso",          
  #                                     "L. normani",
  #                                     "N. antarctica",       
  #                                     "B. tenuis",
  #                                     "I. atlanticus"))) |>
  #   ggplot2::ggplot() +
  #   ggplot2::geom_bar(ggplot2::aes(x = sp_short, y = conc_relative, 
  #                                  fill = Nutrient), 
  #                     stat = "identity", 
  #                     position = ggplot2::position_stack()) +
  #   ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
  #                                         "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
  #                                         "#CEC917FF", "#3A160AFF", "#E75B64FF",
  #                                         "#92BBD9FF", "#26432FFF", "#D98594FF",
  #                                         "#2e276a", "#5c4d73",
  #                                         "#DE7862FF","#D8AF39FF",
  #                                         "#583B2BFF")) +
  #   ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  #   ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
  #                       ncol = 1, 
  #                       strip.position = "left") +
  #   ggplot2::ylab("Relative fraction") +
  #   ggplot2::xlab("Species") +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
  #                  axis.text.x = ggplot2::element_text(size = 9, 
  #                                                      angle = 35, 
  #                                                      hjust = 1,
  #                                                      face = "italic"), 
  #                  axis.title.x = ggplot2::element_text(size = 12, 
  #                                                       face = "bold"), 
  #                  axis.title.y = ggplot2::element_text(size = 12, 
  #                                                       face = "bold"),
  #                  strip.text.x = ggplot2::element_text(size = 12),
  #                  strip.background = ggplot2::element_rect(fill = "lightgrey", 
  #                                                           colour = "white"),
  #                  strip.placement = "outside", 
  #                  legend.position = "right", 
  #                  legend.text = ggplot2::element_text(size = 11),
  #                  legend.title = ggplot2::element_text(size = 11, 
  #                                                       face = "bold"),
  #   )
  # ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_orderCa.jpg",
  #                 scale = 1,
  #                 height = 7, width = 8
  # )
  # 
  # 
  # # with mean per species, order based on absolute Fe concentrations
  # res_fish_tib_per_sp |>
  #   dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
  #                   Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
  #                   Ag + Pb) |>
  #   tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
  #                                Fe, Zn, Sr, Cu, Mn, Se,
  #                                Ni, Cd, As, Co, 
  #                                Pb, Ag), 
  #                       names_to = "Nutrient", 
  #                       values_to = "conc_mg_kg_dw") |> 
  #   dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
  #                 Nutrient = factor(Nutrient, 
  #                                   levels = c("Ca", "P", "Na", "K", "Mg", 
  #                                              "Fe", "Zn", "Cu", "Mn",
  #                                              "As", "Se", "Ni", "Co",
  #                                              "Sr", "Cd", "Pb", "Ag")),
  #                 micro_macro = factor(dplyr::case_when(
  #                   Nutrient %in% c("Ca", "P", "Na", 
  #                                   "K", "Mg") ~ "Ca, P, Na, K, Mg", 
  #                   Nutrient %in% c("Fe", "Zn", "Sr") ~ "Sr, Fe, Zn", 
  #                   Nutrient %in% c("Cu", "Mn", "As", "Ni", "Se") ~ "Cu, Mn, As, Ni, Se",  
  #                   Nutrient %in% c("Cd", "Co", "Pb", "Ag") ~ "Cd, Co, Pb, Ag"), 
  #                   levels = c("Ca, P, Na, K, Mg", 
  #                              "Sr, Fe, Zn", 
  #                              "Cu, Mn, As, Ni, Se", 
  #                              "Cd, Co, Pb, Ag"))) |> 
  #   dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias spp.") ~ Species,
  #                                             TRUE ~ paste0(stringr::str_sub(Species, 
  #                                                                            start = 1, end = 1),
  #                                                           ". ",
  #                                                           stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
  #   dplyr::mutate(sp_short = factor(sp_short, 
  #                                   levels = c(# classification is that of median
  #                                     # for Fe concentrations
  #                                     "A. risso", 
  #                                     "N. coatsi",
  #                                     "Stomias spp.",
  #                                     "P. bolini",
  #                                     "P. tenisoni",
  #                                     "K. anderssoni",
  #                                     "P. andriashevi",
  #                                     "M. carinatus",
  #                                     "B. antarcticus",
  #                                     "P. choriodon",
  #                                     "M. marmorata",
  #                                     "L. mizops",
  #                                     "M. maculata",
  #                                     "G. acuta",
  #                                     "I. atlanticus", 
  #                                     "L. normani",
  #                                     "E. carlsbergi", 
  #                                     "P. gracilis",
  #                                     "D. eleginoides",
  #                                     "G. fraseri",
  #                                     "L. squamifrons",
  #                                     "N. antarctica",
  #                                     "P. crassiceps",
  #                                     "B. tenuis",
  #                                     "E. antarctica",
  #                                     "E. subaspera",
  #                                     "G. piabilis", 
  #                                     "G. nicholsi", 
  #                                     "M. gelatinosum",
  #                                     "C. rhinoceratus",
  #                                     "E. cryomargarites",
  #                                     "G. bolini",
  #                                     "G. braueri",
  #                                     "C. gunnari"))) |>
  #   ggplot2::ggplot() +
  #   ggplot2::geom_bar(ggplot2::aes(x = sp_short, y = conc_relative, 
  #                                  fill = Nutrient), 
  #                     stat = "identity", 
  #                     position = ggplot2::position_stack()) +
  #   ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
  #                                         "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
  #                                         "#CEC917FF", "#3A160AFF", "#E75B64FF",
  #                                         "#92BBD9FF", "#26432FFF", "#D98594FF",
  #                                         "#2e276a", "#5c4d73",
  #                                         "#DE7862FF","#D8AF39FF",
  #                                         "#583B2BFF")) +
  #   ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  #   ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
  #                       ncol = 1, 
  #                       strip.position = "left") +
  #   ggplot2::ylab("Relative fraction") +
  #   ggplot2::xlab("Species") +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
  #                  axis.text.x = ggplot2::element_text(size = 9, 
  #                                                      angle = 35, 
  #                                                      hjust = 1,
  #                                                      face = "italic"), 
  #                  axis.title.x = ggplot2::element_text(size = 12, 
  #                                                       face = "bold"), 
  #                  axis.title.y = ggplot2::element_text(size = 12, 
  #                                                       face = "bold"),
  #                  strip.text.x = ggplot2::element_text(size = 12),
  #                  strip.background = ggplot2::element_rect(fill = "lightgrey", 
  #                                                           colour = "white"),
  #                  strip.placement = "outside", 
  #                  legend.position = "right", 
  #                  legend.text = ggplot2::element_text(size = 11),
  #                  legend.title = ggplot2::element_text(size = 11, 
  #                                                       face = "bold"),
  #   )
  # ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_orderFeabs.jpg",
  #                 scale = 1,
  #                 height = 7, width = 8
  # )
  # 
  # # with mean per species, order based on families but nutrients in order of 
  # # their absolute concentrations
  # res_fish_tib_per_sp |>
  #   dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
  #                   Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
  #                   Ag + Pb) |>
  #   tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
  #                                Fe, Zn, Cu, Mn,
  #                                As, Se, Ni, Co, 
  #                                Sr, Cd, Pb, Ag), 
  #                       names_to = "Nutrient", 
  #                       values_to = "conc_mg_kg_dw") |> 
  #   dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
  #                 Nutrient = factor(Nutrient, 
  #                                   levels = c("Ca", "P", "Na", "K", "Mg", 
  #                                              "Fe", "Zn", "Cu", "Mn",
  #                                              "As", "Se", "Ni", "Co",
  #                                              "Sr", "Cd", "Pb", "Ag")),
  #                 micro_macro = factor(dplyr::case_when(
  #                   Nutrient %in% c("Ca", "P", "Na", 
  #                                   "K", "Mg") ~ "Ca, P, Na, K, Mg", 
  #                   Nutrient %in% c("Fe", "Zn", "Cu", "Mn") ~ "Fe, Zn, Cu, Mn", 
  #                   Nutrient %in% c("As", "Ni", "Se", "Co") ~ "Co, As, Ni, Se",  
  #                   Nutrient %in% c("Sr", "Cd", "Pb", "Ag") ~ "Sr, Cd, Pb, Ag"), 
  #                   levels = c("Ca, P, Na, K, Mg", 
  #                              "Fe, Zn, Cu, Mn", 
  #                              "Co, As, Ni, Se", 
  #                              "Sr, Cd, Pb, Ag"))) |> 
  #   dplyr::mutate(sp_short = dplyr::case_when(Species == "Stomias spp." ~ "*Stomias* spp.",
  #                                             TRUE ~ paste0("*",
  #                                                           stringr::str_sub(Species, 
  #                                                                            start = 1, end = 1),
  #                                                           ". ",
  #                                                           stringr::str_split_fixed(Species, " ", 2)[,2], 
  #                                                           "*"))) |>
  #   dplyr::mutate(sp_short = factor(sp_short, 
  #                                   levels = c(# order by family
  #                                     #Achiropsettidae
  #                                     "*M. maculata*",
  #                                     #Bathydraconidae
  #                                     "*B. antarcticus*",
  #                                     #Bathylagidae    
  #                                     "*B. tenuis*",
  #                                     #Carapidae
  #                                     "*E. cryomargarites*",
  #                                     #Channichthyidae
  #                                     "*C. gunnari*", 
  #                                     "*C. rhinoceratus*", 
  #                                     #Gempylidae
  #                                     "*P. gracilis*",
  #                                     #Macrouridae
  #                                     "*M. carinatus*",
  #                                     #Melamphaidae
  #                                     "*P. crassiceps*",  
  #                                     #Microstomatidae
  #                                     "*N. antarctica*",  
  #                                     #Muraenolepididae  
  #                                     "*M. marmorata*",  
  #                                     #Myctophidae  
  #                                     "*E. antarctica*",
  #                                     "*E. carlsbergi*", 
  #                                     "*E. subaspera*",  
  #                                     "*G. bolini*",
  #                                     "*G. braueri*",     
  #                                     "*G. fraseri*",   
  #                                     "*G. nicholsi*",     
  #                                     "*G. piabilis*",  
  #                                     "*K. anderssoni*",
  #                                     "*P. andriashevi*",    
  #                                     "*P. bolini*", 
  #                                     "*P. choriodon*",
  #                                     "*P. tenisoni*",  
  #                                     #Notosudidae         
  #                                     "*L. normani*",
  #                                     #Nototheniidae
  #                                     "*D. eleginoides*", 
  #                                     "*G. acuta*",  
  #                                     "*L. squamifrons*",    
  #                                     "*L. mizops*",
  #                                     #Paralepididae
  #                                     "*A. risso*", 
  #                                     "*N. coatsi*", 
  #                                     #Stomiidae  
  #                                     "*I. atlanticus*",
  #                                     "*Stomias* spp.",
  #                                     #Zoarcidae
  #                                     "*M. gelatinosum*"))) |>
  #   ggplot2::ggplot() +
  #   ggplot2::geom_bar(ggplot2::aes(x = sp_short, y = conc_relative, 
  #                                  fill = Nutrient), 
  #                     stat = "identity", 
  #                     position = ggplot2::position_stack()) +
  #   ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
  #                                         "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
  #                                         "#CEC917FF", "#3A160AFF", "#E75B64FF",
  #                                         "#92BBD9FF", "#26432FFF", "#D98594FF",
  #                                         "#2e276a", "#5c4d73",
  #                                         "#DE7862FF","#D8AF39FF",
  #                                         "#583B2BFF")) +
  #   ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  #   ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
  #                       ncol = 1, 
  #                       strip.position = "left") +
  #   ggplot2::ggtitle("") +
  #   ggplot2::ylab("Relative fraction") +
  #   ggplot2::xlab("Species") +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
  #                  axis.text.x = ggtext::element_markdown(size = 9, 
  #                                                         angle = 35, 
  #                                                         hjust = 1), 
  #                  axis.title.x = ggplot2::element_text(size = 12, 
  #                                                       face = "bold"), 
  #                  axis.title.y = ggplot2::element_text(size = 12, 
  #                                                       face = "bold"),
  #                  strip.text.x = ggplot2::element_text(size = 12),
  #                  strip.background = ggplot2::element_rect(fill = "lightgrey", 
  #                                                           colour = "white"),
  #                  strip.placement = "outside", 
  #                  legend.position = "right", 
  #                  legend.text = ggplot2::element_text(size = 11),
  #                  legend.title = ggplot2::element_text(size = 11, 
  #                                                       face = "bold")
  #   )
  # ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_order_family_nut_order_tables.jpg",
  #                 scale = 1,
  #                 height = 7, width = 8
  # )
  
}



#'
#'
#'
#'
#'
# function to create barplot displaying CV for major and trace nutrients 
barplot_nut_CV <- function(res_fish_tib_sp) {
  
  options(scipen = 999)
  
  res_fish_tib_sp |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co, 
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Pb", "Ag")), 
                  major_or_trace = dplyr::case_when(Nutrient %in% c("Ca", "P", 
                                                                    "Na", "K", 
                                                                    "Mg", "Sr") ~ "Major", 
                                                    Nutrient %in% c("Fe", "Zn",
                                                                    "Cu", "Mn", 
                                                                    "Se", "As", 
                                                                    "Ni","Co", 
                                                                    "Cd",
                                                                    "Pb", "Ag") ~ "Trace")) |>
    dplyr::group_by(major_or_trace, Nutrient) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = cv, 
                                   fill = major_or_trace), 
                      stat = "identity") +
    ggplot2::scale_fill_manual(values = c("Major" = "#DE7862FF", 
                                          "Trace" = "#1D2645FF")) +
    ggplot2::ylab("Coefficient of variation") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 15),
                   legend.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(0.5, "cm"), 
                   legend.position = "bottom"
    )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/CV_per_nutrient.jpg",
                  scale = 1,
                  height = 3.5, width = 6
  )
  
}


#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of fish
corr_compo_fish <- function(res_fish_tib) {
  
  corr_mat <- robCompositions::corCoDa(
    as.data.frame(res_fish_tib |>
                    dplyr::ungroup()|>
                    dplyr::select(c(Ca, P, Na, K, Mg, 
                                    Fe, Zn, Cu, Mn,
                                    As, Ni, Se, Co,
                                    Sr, Cd, Pb, Ag 
                    )))) 
  
  colnames(corr_mat) <- rownames(corr_mat) <- c("Ca", "P", "Na", "K", "Mg", 
                                                "Fe", "Zn", "Cu", "Mn",
                                                "As", "Se", "Ni", "Co",
                                                "Sr", "Cd", "Pb", "Ag")
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat1 <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                    na.rm = TRUE)) 
  # save for supplementary material
  openxlsx::write.xlsx(melted_cormat1, 
                       file = "output/02.compo-nutrient-patterns/fish_correlation_nutrient_conc.xlsx")
    
  
  ggplot2::ggplot(data = melted_cormat1, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                  high = "#E75B64FF", 
                                  mid = "white", 
                                  midpoint = 0, limit = c(-1,1),
                                  name = "Correlation\ncoefficient") +
    ggplot2::theme_bw() + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_text(size = 14, 
                                                        vjust = 1.3), 
                   legend.text = ggplot2::element_text(size = 11))
  ggplot2::ggsave("output/02.compo-nutrient-patterns/corrplot_compo_fish.jpg",
                  scale = 1,
                  height = 5, width = 5.5
  )
  
  melted_cormat2 <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                    na.rm = TRUE)) |>
    dplyr::mutate(value = factor(dplyr::case_when(value > 0.4 ~ "> 0.4", 
                                                  value < -0.4 ~ "< -0.4", 
                                                  TRUE ~ "[-0.4;0.4]"), 
                                 levels = c("< -0.4", 
                                            "[-0.4;0.4]", 
                                            "> 0.4")))
  
  
  ggplot2::ggplot(data = melted_cormat2, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_manual(values = c("deepskyblue3", 
                                          "gray", 
                                          "firebrick"),
                               name = "Correlation\ncoefficient") +
    ggplot2::theme_bw() + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_text(size = 14, 
                                                        vjust = 1.3), 
                   legend.text = ggplot2::element_text(size = 11))
  ggplot2::ggsave("output/02.compo-nutrient-patterns/corrplot_compo_fish_bicolor.jpg",
                  scale = 1,
                  height = 5, width = 5.5
  )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/corrplot_compo_fish_bicolor.svg",
                  scale = 1,
                  height = 5, width = 5.5
  )
  
}

## NOW LOOKING PER SPECIES (with no means, with all samples ####################

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compo_sp_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Ni, Se, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = dplyr::case_when(Species %in% c("Stomias spp.") ~ Species,
                                                   TRUE ~ paste0(stringr::str_sub(Species, 
                                                                                  start = 1, end = 1),
                                                                 ". ",
                                                                 stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
    dplyr::mutate(Species_short = factor(Species_short, 
                                         levels = c(#Achiropsettidae
                                           "M. maculata",
                                           #Bathydraconidae
                                           "B. antarcticus",
                                           #Bathylagidae    
                                           "B. tenuis",
                                           #Carapidae
                                           "E. cryomargarites",
                                           #Channichthyidae
                                           "C. gunnari", 
                                           "C. rhinoceratus", 
                                           #Gempylidae
                                           "P. gracilis",
                                           #Macrouridae
                                           "M. carinatus",
                                           #Melamphaidae
                                           "P. crassiceps",  
                                           #Microstomatidae
                                           "N. antarctica",  
                                           #Muraenolepididae  
                                           "M. marmorata",  
                                           #Myctophidae  
                                           "E. antarctica",
                                           "E. carlsbergi", 
                                           "E. subaspera",  
                                           "G. bolini",
                                           "G. braueri",     
                                           "G. fraseri",   
                                           "G. nicholsi",     
                                           "G. piabilis",  
                                           "K. anderssoni",
                                           "P. andriashevi",    
                                           "P. bolini", 
                                           "P. choriodon",
                                           "P. tenisoni",  
                                           #Notosudidae         
                                           "L. normani",
                                           #Nototheniidae
                                           "D. eleginoides", 
                                           "G. acuta",  
                                           "L. squamifrons",    
                                           "L. mizops",
                                           #Paralepididae
                                           "A. risso", 
                                           "N. coatsi", 
                                           #Stomiidae  
                                           "I. atlanticus",
                                           "Stomias spp.",
                                           #Zoarcidae
                                           "M. gelatinosum")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Pb", "Ag"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species_short, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Speciesn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#26432FFF", "#D98594FF",
                                          "#2e276a", "#5c4d73",
                                          "#DE7862FF","#D8AF39FF",
                                          "#583B2BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/02.compo-nutrient-patterns/boxplot_sp_all_nut.jpg"),
                  scale = 1,
                  height = 28, width = 22
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with a gradient color and only mean and quantiles 
# only for one nutrient
lineplot_compo_fish_sp_one_nut_grad <- function(res_fish_tib, 
                                                nutrient
) {
  
  # calculate median for all species
  mean_med_allsp <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Ni, Se, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::filter(Nutrient == nutrient) |>
    dplyr::summarise(mean_allsp = mean(concentration_mg_kg_dw), 
                     median_allsp = median(concentration_mg_kg_dw))
  
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Ni, Se, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = dplyr::case_when(Species == "Stomias spp." ~ Species,
                                                   TRUE ~ paste0(stringr::str_sub(Species, 
                                                                                  start = 1, end = 1),
                                                                 ". ",
                                                                 stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "Stomias spp. (n = 10)",
                                      "P. bolini (n = 10)",
                                      "P. tenisoni (n = 10)",
                                      "K. anderssoni (n = 4)",
                                      "P. andriashevi (n = 3)",
                                      "M. carinatus (n = 10)",
                                      "B. antarcticus (n = 5)",
                                      "P. choriodon (n = 8)",
                                      "M. marmorata (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. maculata (n = 2)",
                                      "G. acuta (n = 8)",
                                      "I. atlanticus (n = 1)", 
                                      "L. normani (n = 1)",
                                      "E. carlsbergi (n = 10)", 
                                      "P. gracilis (n = 10)",
                                      "D. eleginoides (n = 2)",
                                      "G. fraseri (n = 12)",
                                      "L. squamifrons (n = 10)",
                                      "N. antarctica (n = 5)",
                                      "P. crassiceps (n = 1)",
                                      "B. tenuis (n = 11)",
                                      "E. antarctica (n = 10)",
                                      "E. subaspera (n = 10)",
                                      "G. piabilis (n = 10)", 
                                      "G. nicholsi (n = 10)", 
                                      "M. gelatinosum (n = 10)",
                                      "C. rhinoceratus (n = 10)",
                                      "E. cryomargarites (n = 10)",
                                      "G. bolini (n = 10)",
                                      "G. braueri (n = 12)",
                                      "C. gunnari (n = 10)"
                                    ))) |>
    dplyr::group_by(Speciesn, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = reorder(Speciesn, 
                                                     median), 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = Speciesn), 
                            linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = reorder(Speciesn, 
                                                 median), 
                                     y = median, 
                                     color = Speciesn), 
                        size = 3) +
    ggplot2::geom_hline(data = mean_med_allsp, 
                        ggplot2::aes(yintercept = median_allsp), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_med_allsp, 
                        ggplot2::aes(yintercept = mean_allsp), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/02.compo-nutrient-patterns/boxplot_sp_nut_grad_", 
                         nutrient, ".jpg"),
                  scale = 1,
                  height = 5, width = 5
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with a gradient color and only mean and quantiles 
# only for one nutrient but just to get the legend right 
lineplot_compo_fish_sp_one_nut_legend <- function(res_fish_tib, 
                                                  nutrient
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Ni, Se, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2])) |>
    dplyr::mutate(Species_short = dplyr::case_when(Species_short == "S. spp." ~ "Stomias spp.", 
                                                   TRUE ~ Species_short)) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "Stomias spp. (n = 10)",
                                      "P. bolini (n = 10)",
                                      "P. tenisoni (n = 10)",
                                      "K. anderssoni (n = 4)",
                                      "P. andriashevi (n = 3)",
                                      "M. carinatus (n = 10)",
                                      "B. antarcticus (n = 5)",
                                      "P. choriodon (n = 8)",
                                      "M. marmorata (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. maculata (n = 2)",
                                      "G. acuta (n = 8)",
                                      "I. atlanticus (n = 1)", 
                                      "L. normani (n = 1)",
                                      "E. carlsbergi (n = 10)", 
                                      "P. gracilis (n = 10)",
                                      "D. eleginoides (n = 2)",
                                      "G. fraseri (n = 12)",
                                      "L. squamifrons (n = 10)",
                                      "N. antarctica (n = 5)",
                                      "P. crassiceps (n = 1)",
                                      "B. tenuis (n = 11)",
                                      "E. antarctica (n = 10)",
                                      "E. subaspera (n = 10)",
                                      "G. piabilis (n = 10)", 
                                      "G. nicholsi (n = 10)", 
                                      "M. gelatinosum (n = 10)",
                                      "C. rhinoceratus (n = 10)",
                                      "E. cryomargarites (n = 10)",
                                      "G. bolini (n = 10)",
                                      "G. braueri (n = 12)",
                                      "C. gunnari (n = 10)"
                                    ))) |>
    dplyr::group_by(Speciesn, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    dplyr::rename(Species = Speciesn) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = reorder(Species, 
                                                     median), 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = Species), 
                            linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = reorder(Species, 
                                                 median), 
                                     y = median, 
                                     color = Species), 
                        size = 3) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE, 
                                 labels = c(# classification is that of median
                                   # for Fe concentrations
                                   "*A. risso* (n = 2)", 
                                   "*N. coatsi* (n = 10)",
                                   "*Stomias* spp. (n = 10)",
                                   "*P. bolini* (n = 10)",
                                   "*P. tenisoni* (n = 10)",
                                   "*K. anderssoni* (n = 4)",
                                   "*P. andriashevi* (n = 3)",
                                   "*M. carinatus* (n = 10)",
                                   "*B. antarcticus* (n = 5)",
                                   "*P. choriodon* (n = 8)",
                                   "*M. marmorata* (n = 10)",
                                   "*L. mizops* (n = 7)",
                                   "*M. maculata* (n = 2)",
                                   "*G. acuta* (n = 8)",
                                   "*I. atlanticus* (n = 1)", 
                                   "*L. normani* (n = 1)",
                                   "*E. carlsbergi* (n = 10)", 
                                   "*P. gracilis* (n = 10)",
                                   "*D. eleginoides* (n = 2)",
                                   "*G. fraseri* (n = 12)",
                                   "*L. squamifrons* (n = 10)",
                                   "*N. antarctica* (n = 5)",
                                   "*P. crassiceps* (n = 1)",
                                   "*B. tenuis* (n = 11)",
                                   "*E. antarctica* (n = 10)",
                                   "*E. subaspera* (n = 10)",
                                   "*G. piabilis* (n = 10)", 
                                   "*G. nicholsi* (n = 10)", 
                                   "*M. gelatinosum* (n = 10)",
                                   "*C. rhinoceratus* (n = 10)",
                                   "*E. cryomargarites* (n = 10)",
                                   "*G. bolini* (n = 10)",
                                   "*G. braueri* (n = 12)",
                                   "*C. gunnari* (n = 10)"
                                 )) +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "right", 
                   legend.text = ggtext::element_markdown(size = 16), 
                   legend.title = ggplot2::element_text(size = 17, 
                                                        face = "bold"))
  ggplot2::ggsave("output/02.compo-nutrient-patterns/boxplot_sp_nut_grad_legend_italic.jpg",
                  scale = 1,
                  height = 10, width = 9)
  
  
}
