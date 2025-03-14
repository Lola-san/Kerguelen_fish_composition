################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# 03.description_compo.R
#
# Script with functions to show output for composition of samples
################################################################################


#'
#'
#'
#'
#' keep only mean from the summary table
#' and add other functional info such as habitat and if 
#' species identified in the diet of any marine predator from the area before
mean_per_species <- function(clean_summary_table) {
  clean_summary_table |>
    dplyr::filter(statistic == "mean") |>
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
                                                            "Stomias sp",
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
    dplyr::select(Habitat, Family, Species, n, confirmed_forage_sp,
                  Ca, P, Na, K, Mg, 
                  Fe, Zn, Cu, Mn,
                  As, Se, Ni, Co, 
                  Sr, Cd, Pb, Ag)
  
}

#'
#'
#'
#'
#'
# function to compare composition of all fish analized 
# density plot normalised per type
density_plot_all_nut <- function(res_fish_tib) {
  #options(scipen = 999)
  
  res_fish_tib |> 
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
  
  
  res_fish_tib |>
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
barplot_nut_fish_compo_relative <- function(res_fish_tib) {
  
  # with mean per species, order based on relative Ca concentrations
  res_fish_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                    Ag + Pb) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co",
                                               "Sr", "Cd", "Pb", "Ag")),
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
    dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias sp") ~ Species,
                                              TRUE ~ paste0(stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
    dplyr::mutate(sp_short = factor(sp_short, 
                                    levels = c(# order is relative to Ca concentration as
                                      # determined in work-in-progress/set-up-the-rest
                                      "B. antarcticus",
                                      "L. mizops",
                                      "M. maculata",
                                      "G. acuta",       
                                      "P. bolini",
                                      "P. tenisoni",   
                                      "G. bolini",
                                      "P. andriashevi",
                                      "L. squamifrons",
                                      "E. carlsbergi",      
                                      "G. fraseri",
                                      "E. subaspera",       
                                      "G. piabilis",
                                      "G. braueri",     
                                      "P. choriodon",
                                      "C. rhinoceratus", 
                                      "M. carinatus",
                                      "G. nicholsi",    
                                      "E. antarctica",
                                      "D. eleginoides",
                                      "P. gracilis",
                                      "P. crassiceps",      
                                      "M. marmorata",
                                      "C. gunnari",   
                                      "Stomias sp",
                                      "N. coatsi",
                                      "K. anderssoni",
                                      "M. gelatinosum",  
                                      "E. cryomargarites",
                                      "A. risso",          
                                      "L. normani",
                                      "N. antarctica",       
                                      "B. tenuis",
                                      "I. atlanticus"))) |>
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
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_text(size = 9, 
                                                       angle = 35, 
                                                       hjust = 1,
                                                       face = "italic"), 
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
                                                        face = "bold"),
    )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_orderCa.jpg",
                  scale = 1,
                  height = 7, width = 8
  )
  
  
  # with mean per species, order based on absolute Fe concentrations
  res_fish_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                    Ag + Pb) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co",
                                               "Sr", "Cd", "Pb", "Ag")),
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
    dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias sp") ~ Species,
                                              TRUE ~ paste0(stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
    dplyr::mutate(sp_short = factor(sp_short, 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso", 
                                      "N. coatsi",
                                      "Stomias sp",
                                      "P. bolini",
                                      "P. tenisoni",
                                      "K. anderssoni",
                                      "P. andriashevi",
                                      "M. carinatus",
                                      "B. antarcticus",
                                      "P. choriodon",
                                      "M. marmorata",
                                      "L. mizops",
                                      "M. maculata",
                                      "G. acuta",
                                      "I. atlanticus", 
                                      "L. normani",
                                      "E. carlsbergi", 
                                      "P. gracilis",
                                      "D. eleginoides",
                                      "G. fraseri",
                                      "L. squamifrons",
                                      "N. antarctica",
                                      "P. crassiceps",
                                      "B. tenuis",
                                      "E. antarctica",
                                      "E. subaspera",
                                      "G. piabilis", 
                                      "G. nicholsi", 
                                      "M. gelatinosum",
                                      "C. rhinoceratus",
                                      "E. cryomargarites",
                                      "G. bolini",
                                      "G. braueri",
                                      "C. gunnari"))) |>
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
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_text(size = 9, 
                                                       angle = 35, 
                                                       hjust = 1,
                                                       face = "italic"), 
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
                                                        face = "bold"),
    )
  ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_orderFeabs.jpg",
                  scale = 1,
                  height = 7, width = 8
  )
  
  # with mean per species, order based on families
  res_fish_tib |>
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
    dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias sp") ~ Species,
                                              TRUE ~ paste0(stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
    dplyr::mutate(sp_short = factor(sp_short, 
                                    levels = c(# order by family
                                      #Achiropsettidae
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
                                      "Stomias sp",
                                      #Zoarcidae
                                      "M. gelatinosum"))) |>
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
                   axis.text.x = ggplot2::element_text(size = 9, 
                                                       angle = 35, 
                                                       hjust = 1,
                                                       face = "italic"), 
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
  
  
  # with mean per species, order based on families
  res_fish_tib |>
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
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co",
                                               "Sr", "Cd", "Pb", "Ag")),
                  micro_macro = factor(dplyr::case_when(
                    Nutrient %in% c("Ca", "P", "Na", 
                                    "K", "Mg") ~ "Ca, P, Na, K, Mg", 
                    Nutrient %in% c("Fe", "Zn", "Cu", "Mn") ~ "Fe, Zn, Cu, Mn", 
                    Nutrient %in% c("As", "Ni", "Se", "Co") ~ "Co, As, Ni, Se",  
                    Nutrient %in% c("Sr", "Cd", "Pb", "Ag") ~ "Sr, Cd, Pb, Ag"), 
                    levels = c("Ca, P, Na, K, Mg", 
                               "Fe, Zn, Cu, Mn", 
                               "Co, As, Ni, Se", 
                               "Sr, Cd, Pb, Ag"))) |> 
    dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias sp") ~ Species,
                                              TRUE ~ paste0(stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
    dplyr::mutate(sp_short = factor(sp_short, 
                                    levels = c(# order by family
                                      #Achiropsettidae
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
                                      "Stomias sp",
                                      #Zoarcidae
                                      "M. gelatinosum"))) |>
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
                   axis.text.x = ggplot2::element_text(size = 9, 
                                                       angle = 35, 
                                                       hjust = 1,
                                                       face = "italic"), 
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
  ggplot2::ggsave("output/02.compo-nutrient-patterns/fish_compo_rel_micro-macro_order_family2.jpg",
                  scale = 1,
                  height = 7, width = 8
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
table_nut_fish_compo_relative <- function(res_fish_tib) {

  # with mean per species, order based on families
  table <- res_fish_tib |>
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


#'
#'
#'
#'
#'
# function to create barplot displaying CV for major and trace nutrients 
barplot_nut_CV <- function(res_fish_tib
) {
  
  options(scipen = 999)
  
  res_fish_tib |>
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
  
}


#'
#'
#'
#'
#'
# function to generate table of elemental composition per species
table_compo_fish_sp <- function(res_fish_tib
) {
  
  
  table_all_samples <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(min = round(min(concentration_mg_kg_dw), 3), 
                     `2.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                   probs = c(0.025)), 3),
                     mean = round(mean(concentration_mg_kg_dw), 3),
                     `97.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                    probs = c(0.975)), 3),
                     max = round(max(concentration_mg_kg_dw), 3), 
                     sd = round(sd(concentration_mg_kg_dw), 3), 
                     cv = round(sd(concentration_mg_kg_dw)/mean, 3)) |>
    tidyr::pivot_longer(cols = c(min, `2.5p_quant`, mean, `97.5p_quant`,
                                 max, sd, cv), 
                        names_to = "stat_variable", 
                        values_to = "stat_value") |>
    tidyr::pivot_wider(names_from = "Nutrient", 
                       values_from = "stat_value")
  
  openxlsx::write.xlsx(table_all_samples, 
                       file = "output/02.compo-nutrient-patterns/summary_fish_compo_all_samples.xlsx")
  
  # all samples but with averaging by species before 
  table_all_samples <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Species, Nutrient) |>
    dplyr::summarise(mean_sp = round(mean(concentration_mg_kg_dw), 3)) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(min = round(min(mean_sp), 3), 
                     `2.5p_quant` = round(quantile(mean_sp, 
                                                   probs = c(0.025)), 3),
                     mean = round(mean(mean_sp), 3),
                     `97.5p_quant` = round(quantile(mean_sp, 
                                                    probs = c(0.975)), 3),
                     max = round(max(mean_sp), 3), 
                     sd = round(sd(mean_sp), 3), 
                     cv = round(sd(mean_sp)/mean, 3)) |>
    tidyr::pivot_longer(cols = c(min, `2.5p_quant`, mean, `97.5p_quant`,
                                 max, sd, cv), 
                        names_to = "stat_variable", 
                        values_to = "stat_value") |>
    tidyr::pivot_wider(names_from = "Nutrient", 
                       values_from = "stat_value")
  
  openxlsx::write.xlsx(table_all_samples, 
                       file = "output/02.compo-nutrient-patterns/summary_fish_compo_all_mean_sp.xlsx")
  
  
  table_sp <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Ni, Se, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Species, Nutrient) |>
    dplyr::summarise(min = round(min(concentration_mg_kg_dw), 3), 
                     `2.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                   probs = c(0.025)), 3),
                     mean = round(mean(concentration_mg_kg_dw), 3),
                     `97.5p_quant` = round(quantile(concentration_mg_kg_dw, 
                                                    probs = c(0.975)), 3),
                     max = round(max(concentration_mg_kg_dw), 3), 
                     sd = round(sd(concentration_mg_kg_dw), 3), 
                     cv = round(sd(concentration_mg_kg_dw)/mean, 3)) |>
    tidyr::pivot_longer(cols = c(min, `2.5p_quant`, mean, `97.5p_quant`,
                                 max, sd, cv), 
                        names_to = "stat_variable", 
                        values_to = "stat_value") |>
    tidyr::pivot_wider(names_from = "Nutrient", 
                       values_from = "stat_value")
  
  openxlsx::write.xlsx(table_sp, 
                       file = "output/02.compo-nutrient-patterns/summary_fish_compo_sp.xlsx")
  
}

###################### PER SPECIES #############################################

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
    dplyr::mutate(Species_short = dplyr::case_when(Species %in% c("Stomias sp") ~ Species,
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
                                           "Stomias sp",
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
  ggplot2::ggsave(paste0("output/03.compo-sp-fam_hab/sp/boxplot_sp_all_nut.jpg"),
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
    dplyr::mutate(Species_short = dplyr::case_when(Species == "Stomias sp" ~ "Stomias spp.",
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
  ggplot2::ggsave(paste0("output/03.compo-sp-fam_hab/sp/boxplot_sp_nut_grad_", 
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
    dplyr::mutate(Species_short = dplyr::case_when(Species_short == "S. sp" ~ "Stomias sp", 
                                                   TRUE ~ Species_short)) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "Stomias sp (n = 10)",
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
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
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
                   legend.text = ggplot2::element_text(size = 16, 
                                                       face = "italic"), 
                   legend.title = ggplot2::element_text(size = 17, 
                                                        face = "bold"))
  ggplot2::ggsave("output/03.compo-sp-fam_hab/sp/boxplot_sp_nut_grad_legend_italic.jpg",
                  scale = 1,
                  height = 10, width = 9)
  
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per family
boxplot_compo_fish_fam <- function(res_fish_tib, 
                                   nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Ni, Se, Co,
                                 Sr, Cd, Pb, Ag), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, "\n(n = ", n, ")")) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Familyn, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = Family)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/03.compo-sp-fam_hab/fam/boxplot_order_fam_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per family
boxplot_compo_fish_fam_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Family = factor(Family, 
                                  levels = c("Achiropsettidae", 
                                             "Bathydraconidae",
                                             "Bathylagidae",
                                             "Carapidae",
                                             "Channichthyidae",
                                             "Gempylidae",
                                             "Macrouridae",
                                             "Melamphaidae", 
                                             "Microstomatidae",
                                             "Muraenolepididae",
                                             "Myctophidae",
                                             "Notosudidae", 
                                             "Nototheniidae", 
                                             "Paralepididae", 
                                             "Stomiidae",
                                             "Zoarcidae")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Familyn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF"))+
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/per-fam/boxplot_fam_all_nut.jpg",
                  scale = 1,
                  height = 18, width = 22
  )
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per family
boxplot_compo_fish_fam_few_nut <- function(res_fish_tib, 
                                           nutrients
) {
  
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, " (n = ", n, ")"))|> 
    dplyr::mutate(Familyn = factor(Familyn, 
                                   levels = c("Channichthyidae (n = 20)",
                                              "Zoarcidae (n = 10)",
                                              "Microstomatidae (n = 5)",
                                              "Melamphaidae (n = 1)", 
                                              "Bathylagidae (n = 11)",
                                              "Notosudidae (n = 1)", 
                                              "Nototheniidae (n = 27)", 
                                              "Carapidae (n = 10)",
                                              "Myctophidae (n = 119)",
                                              "Achiropsettidae (n = 2)", 
                                              "Muraenolepididae (n = 10)",
                                              "Macrouridae (n = 10)",
                                              "Bathydraconidae (n = 5)",
                                              "Gempylidae (n = 10)",
                                              "Stomiidae (n = 11)",
                                              "Paralepididae (n = 12)")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::filter(Nutrient %in% nutrients) |>
    ggplot2::ggplot(ggplot2::aes(x = Familyn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 2) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-fam/boxplot_fam_", 
                         stringr::str_c(nutrients, collapse = ""), ".jpg"),
                  scale = 1,
                  height = 8, width = 9
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per habitat
boxplot_compo_fish_hab_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(habitat = factor(habitat, 
                                   levels = c("Demersal", 
                                              "Bathydemersal", 
                                              "Benthopelagic",
                                              "Bathypelagic")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(habitat) |>
    dplyr::mutate(nsample = dplyr::n_distinct(Code_sample),
                  nsp = dplyr::n_distinct(Species)) |>
    dplyr::group_by(Species, habitat, Nutrient) |>
    dplyr::summarise(habitatnsp = paste0(habitat, " (n(sp) = ", nsp, ")"), 
                     mean_conc_mg_kg_dw = mean(concentration_mg_kg_dw)) |>
    dplyr::mutate(habitatnsp = factor(habitatnsp, 
                                      levels = c("Demersal (n(sp) = 5)", 
                                                 "Bathydemersal (n(sp) = 4)", 
                                                 "Benthopelagic (n(sp) = 6)",
                                                 "Bathypelagic (n(sp) = 19)")))|>
    ggplot2::ggplot(ggplot2::aes(x = habitatnsp, 
                                 y = mean_conc_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot(alpha=0.9) +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Habitat") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-habitat/boxplot_hab_all_nut.jpg"),
                  scale = 1,
                  height = 10, width = 14
  )
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
MWtest_fish_hab <- function(res_fish_tib) {
  
  compo_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::group_by(habitat, Species, Nutrient) |>
      dplyr::summarise(mean_sp_conc_mg_kg_dw = mean(concentration_mg_kg_dw)) |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = habitat, 
                         values_from = mean_sp_conc_mg_kg_dw)
    
    demersal <- na.omit(table$Demersal)
    bathypel <- na.omit(table$Bathypelagic)
    bathydem <- na.omit(table$Bathydemersal)
    benthopel <- na.omit(table$Benthopelagic)
    
    nut_test <- data.frame(Nutrient = rep(nut, 6), 
                           Habitat1 = c("Demersal", "Demersal", "Demersal",
                                        "Bathypelagic", "Bathypelagic", 
                                        "Bathydemersal"), 
                           Habitat2 = c("Bathypelagic", "Bathydemersal", "Benthopelagic",
                                        "Bathydemersal", "Benthopelagic", 
                                        "Benthopelagic"), 
                           alpha_MW = c(wilcox.test(demersal, bathypel)[[3]],
                                        wilcox.test(demersal, bathydem)[[3]],
                                        wilcox.test(demersal, benthopel)[[3]],
                                        
                                        wilcox.test(bathypel, bathydem)[[3]],
                                        wilcox.test(bathypel, benthopel)[[3]],
                                        
                                        wilcox.test(bathydem, benthopel)[[3]]))
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        Habitat1 = NA,
                        Habitat2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,]
  
  df_test <- df_test |>
    dplyr::mutate(significant = dplyr::case_when(alpha_MW <= 0.05 ~ "yes", 
                                                 TRUE ~ "no"))
  
  openxlsx::write.xlsx(df_test, 
                       file = paste0("output/compo fish/per-habitat/Mann_Whitney_test_fish_habitat.xlsx"))
  
  
  
}



#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of all fish
boxplot_compo_fish_tot <- function(res_fish_tib) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/boxplot_compo_fish_tot.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
}
