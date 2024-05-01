################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# 02.description_compo.R
#
# Script with functions to show output for composition of samples
################################################################################

#'
#'
#'
#'
#'
# function to identify values under limit of quantification (loq), for non
# essential nutrients 
ID_under_loq_values <- function(fish_compo_tib
) 
{
  
  # table to log data about values under LOQ 
  # i.e. nutrients and % of values per nutrient 
  table_LOQ_all <- fish_compo_tib |>
    dplyr::mutate(Cr = as.numeric(Cr),
                  Mo = as.numeric(Mo), 
                  V = as.numeric(V),
                  Pb = as.numeric(Pb), 
                  Sr = as.numeric(Sr), 
                  Ag = as.numeric(Ag)) |>
    tidyr::pivot_longer(cols = c(Cr, Mo, V, 
                                 Ag, Pb, Cd, Sr,
                                 Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn, Se,
                                 As, Ni, Co), 
                        names_to = 'Nutrient', 
                        values_to = "conc_mg_kg_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::reframe(n_under_LOQ = sum(is.na(conc_mg_kg_dw)), 
                   p_under_LOQ = round(100*(n_under_LOQ/length(unique(Code_sample))), 1))
  # save 
  openxlsx::write.xlsx(table_LOQ_all, 
                       file = "output/samples_fish_nut_under_LOQ.xlsx")
  
  table_LOQ_sp <- fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Sr = as.numeric(Sr), 
                  Ag = as.numeric(Ag)) |>
    tidyr::pivot_longer(cols = c(Ag, Pb, Cd, Sr,
                                 Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn, Se,
                                 As, Ni, Co), 
                        names_to = 'Nutrient', 
                        values_to = "conc_mg_kg_dw") |>
    dplyr::group_by(Species, Nutrient) |>
    dplyr::reframe(n_under_LOQ = sum(is.na(conc_mg_kg_dw)), 
                   p_under_LOQ = round(100*(n_under_LOQ/length(unique(Code_sample))), 1))
  # save 
  openxlsx::write.xlsx(table_LOQ_sp, 
                       file = "output/samples_fish_sp_nut_under_LOQ.xlsx")
  
  list(table_LOQ_all, table_LOQ_sp)
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
table_compo_fish_sp_without_loq_replaced <- function(fish_compo_tib) {
  
  table <- fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Sr = as.numeric(Sr), 
                  Ag = as.numeric(Ag)) |>
    tidyr::pivot_longer(cols = c(Ag, Pb, Cd, Sr,
                                 Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn, Se,
                                 As, Ni, Co), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Family, Species, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     mean = round(mean(concentration_mg_kg_dw, 
                                       na.rm = TRUE), 3),
                     sd = round(sd(concentration_mg_kg_dw, 
                                   na.rm = TRUE), 3)) |> 
    tidyr::pivot_longer(cols = c(mean, sd), 
                        names_to = "statistic", 
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = value)
  
  openxlsx::write.xlsx(table, 
                       file = paste0("output/summary_fish_compo_sp_loq_not_replaced.xlsx"))
  
  table
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with values under loq replaced by loq/2, so it depends on samples and/or
# nutrients
table_compo_fish_sp_with_loq_replaced <- function(fish_compo_tib) {
  
  table <- fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Sr = as.numeric(Sr), 
                  Ag = as.numeric(Ag)) |>
    dplyr::mutate(Ag = dplyr::case_when(is.na(Ag) ~ 0.01/2, 
                                        # loq is 0.01 for all samples 
                                        TRUE ~ Ag), 
                  Pb = dplyr::case_when(is.na(Pb) ~ 0.01/2, 
                                        # loq is 0.01 for all samples 
                                        TRUE ~ Pb)) |>
    tidyr::pivot_longer(cols = c(Ag, Pb, Cd, Sr,
                                 Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn, Se,
                                 As, Ni, Co), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    ## remove NAs if there is still some
    #dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::group_by(Family, Species, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     mean = round(mean(concentration_mg_kg_dw, 
                                       na.rm = FALSE), 3),
                     sd = round(sd(concentration_mg_kg_dw, 
                                   na.rm = FALSE), 3)) |> 
    tidyr::pivot_longer(cols = c(mean, sd), 
                        names_to = "statistic", 
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = value)
  
  openxlsx::write.xlsx(table, 
                       file = paste0("output/summary_fish_compo_sp_loq_replaced.xlsx"))
  
  table
  
}

#'
#'
#'
#'
#'
# function to add data to the result table with composition of prey
complete_fish_data <- function(res_fish_tib,
                               diet_tib) {
  
  # create vector with list of species identified as prey of Arctocephalus gazella in the SO
  prey_sp <- unique(diet_tib$Species)
  
  res_fish_tib |>
    # prey species
    dplyr::mutate(campaign = dplyr::case_when(stringr::str_starts(Code_sample, 
                                                                  "2005", 
                                                                  negate = FALSE) ~ "Isotopes 2005", 
                                              stringr::str_starts(Code_sample, 
                                                                  "2010", 
                                                                  negate = FALSE) ~ "Poker II 2010"), 
                  diet = dplyr::case_when(Species %in% c(prey_sp) ~ 1,
                                          # some sp of Muraenolepis sp. were found in 
                                          # A. gazella scats but they don't match here because 
                                          # we did not specified species
                                          Species == "Muraenolepis sp" ~ 1,
                                          TRUE ~ 0), 
                  habitat = dplyr::case_when(Species %in% c("Gobionotothen acuta", 
                                                            "Channichthys rhinoceratus",
                                                            "Dissostichus eleginoides",
                                                            "Mancopsetta mancopsetta",
                                                            "Electrona antarctica") ~ "Demersal", 
                                             Species %in% c("Lepidonotothen squamifrons", 
                                                            "Champsocephalus gunnari",
                                                            "Lindbergichthys mizops",
                                                            "Muraenolepis sp",
                                                            "Gymnoscopelus piabilis",
                                                            "Gymnoscopelus bolini") ~ "Benthopelagic", 
                                             Species %in% c("Bathydraco antarcticus",
                                                            "Macrourus carinatus",
                                                            "Paradiplospinus gracilis",
                                                            "Echiodon cryomargarites") ~ "Bathydemersal", 
                                             Species %in% c("Krefftichthys anderssoni",
                                                            "Melanostigma gelatinosum",
                                                            "Bathylagus tenuis",
                                                            "Luciosudis normani", 
                                                            "Gymnoscopelus braueri", 
                                                            "Gymnoscopelus fraseri", 
                                                            "Gymnoscopelus nicholsi", 
                                                            "Electrona subaspera",
                                                            "Poromitra crassiceps", 
                                                            "Nansenia antarctica",
                                                            "Electrona carlsbergi", 
                                                            "Protomyctophum andriashevi",
                                                            "Protomyctophum bolini", 
                                                            "Protomyctophum choriodon",
                                                            "Stomias sp",
                                                            "Idiacanthus atlanticus",
                                                            "Arctozenus risso",
                                                            "Notolepis coatsi", 
                                                            "Protomyctophum tenisoni") ~ "Bathypelagic"))
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
    dplyr::filter(statistic == "mean") |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Ag, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb"))) |>
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
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 18, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 15, face = "bold"),
                   panel.spacing.x = ggplot2::unit(0.5, "cm"),
                   legend.position = "none"
    )
  ggplot2::ggsave("output/densityplot_all_nut.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  
  res_fish_tib |>
    dplyr::filter(statistic == "mean") |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Ag, Pb), 
                        names_to = "Nutrient", 
                        values_to = "mean_sp_conc_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb"))) |>
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
  ggplot2::ggsave("output/density-ridge-plot_all_nut_norm.jpg",
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
  
  # with mean per species
  res_fish_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                    Ag + Pb) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Ag, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::filter(statistic == "mean") |>
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb")),
                  micro_macro = dplyr::case_when(
                    Nutrient %in% c("Ca", "P", "Na", 
                                    "K", "Mg") ~ "macro", 
                    Nutrient %in% c("Fe", "Zn", "Sr") ~ "micro", 
                    Nutrient %in% c("Cu", "Mn", "As", "Ni", "Se") ~ "micromicro",  
                    Nutrient %in% c("Cd", "Co", "Ag", "Pb") ~ "micromicromicro",)) |> 
    # order of samples determined in work-in-progress/set-up-the-rest.R
    dplyr::mutate(Species = factor(Species, 
                                   levels = c(
                                     # order is relative to Ca concentration as
                                     # determined in work-in-progress/set-up-the-rest
                                     "Bathydraco antarcticus",
                                     "Lindbergichthys mizops",
                                     "Mancopsetta mancopsetta",
                                     "Gobionotothen acuta",       
                                     "Protomyctophum bolini",
                                     "Protomyctophum tenisoni",   
                                     "Gymnoscopelus bolini",
                                     "Protomyctophum andriashevi",
                                     "Lepidonotothen squamifrons",
                                     "Electrona carlsbergi",      
                                     "Gymnoscopelus fraseri",
                                     "Electrona subaspera",       
                                     "Gymnoscopelus piabilis",
                                     "Gymnoscopelus braueri",     
                                     "Protomyctophum choriodon",
                                     "Channichthys rhinoceratus", 
                                     "Macrourus carinatus",
                                     "Gymnoscopelus nicholsi",    
                                     "Electrona antarctica",
                                     "Dissostichus eleginoides",
                                     "Paradiplospinus gracilis",
                                     "Poromitra crassiceps",      
                                     "Muraenolepis sp",
                                     "Champsocephalus gunnari",   
                                     "Stomias sp",
                                     "Notolepis coatsi",
                                     "Krefftichthys anderssoni",
                                     "Melanostigma gelatinosum",  
                                     "Echiodon cryomargarites",
                                     "Arctozenus risso",          
                                     "Luciosudis normani",
                                     "Nansenia antarctica",       
                                     "Bathylagus tenuis",
                                     "Idiacanthus atlanticus"))) |>
    dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias sp", 
                                                             "Muraenolepis sp") ~ Species,
                                              TRUE ~ paste0(stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
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
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
                        nrow = 3) +
    ggplot2::ggtitle("") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_text(size = 5, 
                                                       angle = 30, 
                                                       hjust = 1), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
    )
  ggplot2::ggsave("output/fish_compo_rel_micro-macro_orderCa.jpg",
                  scale = 1,
                  height = 4, width = 10
  )
  
  # with mean per species
  res_fish_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                    Ag + Pb) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Ag, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::filter(statistic == "mean") |>
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb")),
                  micro_macro = dplyr::case_when(
                    Nutrient %in% c("Ca", "P", "Na", 
                                    "K", "Mg") ~ "macro", 
                    Nutrient %in% c("Fe", "Zn", "Sr") ~ "micro", 
                    Nutrient %in% c("Cu", "Mn", "As", "Ni", "Se") ~ "micromicro",  
                    Nutrient %in% c("Cd", "Co", "Ag", "Pb") ~ "micromicromicro",)) |> 
    # order of samples determined in work-in-progress/set-up-the-rest.R
    dplyr::mutate(Species = factor(Species, 
                                   levels = c(# order by family
                                     #Achiropsettidae
                                     "Mancopsetta mancopsetta",
                                     #Bathydraconidae
                                     "Bathydraco antarcticus",
                                     #Bathylagidae    
                                     "Bathylagus tenuis",
                                     #Carapidae
                                     "Echiodon cryomargarites",
                                     #Channichthyidae
                                     "Champsocephalus gunnari", 
                                     "Channichthys rhinoceratus", 
                                     #Gempylidae
                                     "Paradiplospinus gracilis",
                                     #Macrouridae
                                     "Macrourus carinatus",
                                     #Melamphaidae
                                     "Poromitra crassiceps",  
                                     #Microstomatidae
                                     "Nansenia antarctica",  
                                     #Muraenolepididae  
                                     "Muraenolepis sp",  
                                     #Myctophidae  
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
                                     #Notosudidae         
                                     "Luciosudis normani",
                                     #Nototheniidae
                                     "Dissostichus eleginoides", 
                                     "Gobionotothen acuta",  
                                     "Lepidonotothen squamifrons",    
                                     "Lindbergichthys mizops",
                                     #Paralepididae
                                     "Arctozenus risso", 
                                     "Notolepis coatsi", 
                                     #Stomiidae  
                                     "Idiacanthus atlanticus",
                                     "Stomias sp",
                                     #Zoarcidae
                                     "Melanostigma gelatinosum"))) |>
    dplyr::mutate(sp_short = dplyr::case_when(Species %in% c("Stomias sp", 
                                                             "Muraenolepis sp") ~ Species,
                                              TRUE ~ paste0(stringr::str_sub(Species, 
                                                                             start = 1, end = 1),
                                                            ". ",
                                                            stringr::str_split_fixed(Species, " ", 2)[,2]))) |>
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
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
                        nrow = 3) +
    ggplot2::ggtitle("") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_text(size = 5, 
                                                       angle = 30, 
                                                       hjust = 1,
                                                       face = "italic"), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
    )
  ggplot2::ggsave("output/fish_compo_rel_micro-macro_order_family.jpg",
                  scale = 1,
                  height = 4, width = 10
  )
  
}



#'
#'
#'
#'
#'
# function to create barplot displaying CV for major and trace nutrients in
# both scats and prey
barplot_nut_CV <- function(res_fish_tib
) {
  
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, As, Co, 
                                 Ag, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::filter(statistic == "mean") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb")), 
                  major_or_trace = dplyr::case_when(Nutrient %in% c("Ca", "P", 
                                                                    "Na", "K", 
                                                                    "Mg", "Sr") ~ "Major", 
                                                    Nutrient %in% c("Fe", "Zn",
                                                                    "Cu", "Mn", 
                                                                    "Se", "As", 
                                                                    "Ni","Co", 
                                                                    "Cd",
                                                                    "Ag", "Pb") ~ "Trace")) |>
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
  ggplot2::ggsave("output/CV_per_nutrient.jpg",
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
                    dplyr::filter(statistic =="mean") |>
                    dplyr::ungroup()|>
                    dplyr::select(c(Ca, P, Mg, Na, K, 
                                    Fe, Zn, Sr, Cu, Mn, Se,
                                    Ni, Cd, As, Co, 
                                    Ag, Pb 
                    )))) 
  
  colnames(corr_mat) <- rownames(corr_mat) <- c("Ca", "P", "Na", "K", "Mg", "Sr",
                                                "Fe", "Zn", "Cu", "Mn",
                                                "As", "Ni", "Se",
                                                "Cd", "Co", "Ag", "Pb")
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                    na.rm = TRUE)) 
  
  ggplot2::ggplot(data = melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
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
  ggplot2::ggsave("output/corrplot_compo_fish.jpg",
                  scale = 1,
                  height = 5, width = 5.5
  )
  
}


#'
#'
#'
# simple function to create table with mean per species 
compute_means_sp <- function(compo_tib) {
  
  compo_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Species) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species, "\n(n = ", n, ")")) |>
    dplyr::group_by(Family, habitat, Speciesn, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = mean_sp) |>
    dplyr::rename(Species = Speciesn)
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compo_fish_sp <- function(res_fish_tib, 
                                  nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    dplyr::group_by(Species) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species, "\n(n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Speciesn, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = Species)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_order_sp_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 14, width = 17
  )
  
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compo_fish_sp_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2])) |>
    dplyr::mutate(Species_short = factor(Species_short, 
                                         levels = c(# Paralepididae
                                           "A. risso", 
                                           "N. coatsi",
                                           # Bathydraconidae
                                           "B. antarcticus",
                                           # Bathylagidae
                                           "B. tenuis",
                                           # Channichthyidae
                                           "C. gunnari",
                                           "C. rhinoceratus",
                                           # Nototheniidae
                                           "D. eleginoides",
                                           "G. acuta",
                                           "L. squamifrons",
                                           "L. mizops",
                                           # Carapidae
                                           "E. cryomargarites",
                                           # Myctophidae
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
                                           # Stomiidae
                                           "I. atlanticus", 
                                           "S. sp",
                                           # Notosudidae
                                           "L. normani",
                                           # Macrouridae
                                           "M. carinatus",
                                           # Achiropsettidae
                                           "M. mancopsetta",
                                           "M. gelatinosum",
                                           # Muraenolepididae
                                           "M. sp",
                                           # Microstomatidae
                                           "N. antarctica",
                                           # Gempylidae
                                           "P. gracilis",
                                           # Melamphaidae
                                           "P. crassiceps")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
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
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
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
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_all_nut.jpg"),
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
boxplot_compo_fish_sp_all_nut_grad <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2])) |>
    dplyr::mutate(Species_short = factor(Species_short, 
                                         levels = c(# Paralepididae
                                           "A. risso", 
                                           "N. coatsi",
                                           # Bathydraconidae
                                           "B. antarcticus",
                                           # Bathylagidae
                                           "B. tenuis",
                                           # Channichthyidae
                                           "C. gunnari",
                                           "C. rhinoceratus",
                                           # Nototheniidae
                                           "D. eleginoides",
                                           "G. acuta",
                                           "L. squamifrons",
                                           "L. mizops",
                                           # Carapidae
                                           "E. cryomargarites",
                                           # Myctophidae
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
                                           # Stomiidae
                                           "I. atlanticus", 
                                           "S. sp",
                                           # Notosudidae
                                           "L. normani",
                                           # Macrouridae
                                           "M. carinatus",
                                           # Achiropsettidae
                                           "M. mancopsetta",
                                           "M. gelatinosum",
                                           # Muraenolepididae
                                           "M. sp",
                                           # Microstomatidae
                                           "N. antarctica",
                                           # Gempylidae
                                           "P. gracilis",
                                           # Melamphaidae
                                           "P. crassiceps")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species_short, " (n = ", n, ")")) |>
    dplyr:: group_by(Speciesn, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
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
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "bottom")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_all_nut_grad.jpg"),
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
boxplot_compo_fish_sp_one_nut_grad <- function(res_fish_tib, 
                                               nutrient
) {
  
  # calculate median for all species
  mean_med_allsp <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::filter(Nutrient == nutrient) |>
    dplyr::summarise(mean_allsp = mean(concentration_mg_kg_dw), 
                     median_allsp = median(concentration_mg_kg_dw))
  
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2]),
      Nutrient = factor(Nutrient, 
                        levels = c("Ca", "P", "Na", "K", "Mg", 
                                   "Fe", "Zn", "Cu", "Mn", "Se",
                                   "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "S. sp (n = 10)",
                                      "P. bolini (n = 10)",
                                      "P. tenisoni (n = 10)",
                                      "K. anderssoni (n = 4)",
                                      "P. andriashevi (n = 3)",
                                      "M. carinatus (n = 10)",
                                      "B. antarcticus (n = 5)",
                                      "P. choriodon (n = 8)",
                                      "M. sp (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. mancopsetta (n = 2)",
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
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_nut_grad_", 
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
# only for one nutrient, with sp names
boxplot_compo_fish_sp_one_nut_grad_sp <- function(res_fish_tib, 
                                                  nutrient
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2]),
      Nutrient = factor(Nutrient, 
                        levels = c("Ca", "P", "Na", "K", "Mg", 
                                   "Fe", "Zn", "Cu", "Mn", "Se",
                                   "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "S. sp (n = 10)",
                                      "P. bolini (n = 10)",
                                      "P. tenisoni (n = 10)",
                                      "K. anderssoni (n = 4)",
                                      "P. andriashevi (n = 3)",
                                      "M. carinatus (n = 10)",
                                      "B. antarcticus (n = 5)",
                                      "P. choriodon (n = 8)",
                                      "M. sp (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. mancopsetta (n = 2)",
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
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_nut_grad_spnames_", 
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
boxplot_compo_fish_sp_one_nut_legend <- function(res_fish_tib, 
                                                 nutrient
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2]),
      Nutrient = factor(Nutrient, 
                        levels = c("Ca", "P", "Na", "K", "Mg", 
                                   "Fe", "Zn", "Cu", "Mn", "Se",
                                   "As", "Ni","Co"))) |>
    dplyr::mutate(Species_short = dplyr::case_when(Species_short == "S. sp" ~ "Stomias sp", 
                                                   Species_short == "M. sp" ~ "Muraenolepis sp", 
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
                                      "Muraenolepis sp (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. mancopsetta (n = 2)",
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
  ggplot2::ggsave("output/compo fish/per-sp/boxplot_sp_nut_grad_legend_italic.jpg",
                  scale = 1,
                  height = 10, width = 9)
  # ggplot2::ggsave("output/compo fish/per-sp/boxplot_sp_nut_grad_legend_plain.jpg",
  #                 width = 9,
  #                 height = 4,
  #                 dpi = 300)
  
}




#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per genus
boxplot_compo_fish_genus <- function(res_fish_tib, 
                                     nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    dplyr::group_by(Genus) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Genusn = paste0(Genus, "\n(n = ", n, ")")) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Genusn,
                                             concentration_mg_kg_dw),
                                 y = concentration_mg_kg_dw, fill = Genus)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Genus") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-genus/boxplot_order_genus_",
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
# function to display boxplot of elemental composition per genus
boxplot_compo_fish_genus_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2], 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::mutate(Genus = factor(Genus, 
                                 levels = c("Arctozenus", 
                                            "Bathydraco",
                                            "Bathylagus",
                                            "Champsocephalus",
                                            "Channichthys",
                                            "Dissostichus",
                                            "Echiodon",
                                            "Electrona",
                                            "Gobionotothen", 
                                            "Gymnoscopelus",
                                            "Idiacanthus",
                                            "Krefftichthys",
                                            "Lepidonotothen",
                                            "Lindbergichthys",
                                            "Luciosudis", 
                                            "Macrourus", 
                                            "Mancopsetta", 
                                            "Melanostigma",
                                            "Muraenolepis",
                                            "Nansenia",
                                            "Notolepis",
                                            "Paradiplospinus",
                                            "Poromitra",
                                            "Protomyctophum",
                                            "Stomias"))) |>
    dplyr::group_by(Genus) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Genusn = paste0(Genus, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Genusn, 
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
    ggplot2::xlab("Genus") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-genus/boxplot_genus_all_nut.jpg"),
                  scale = 1,
                  height = 22, width = 22
  )
  
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
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
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
  ggplot2::ggsave(paste0("output/compo fish/per-fam/boxplot_order_fam_",
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
# function to display boxplot of elemental composition per campaign
boxplot_compo_fish_camp <- function(res_fish_tib, 
                                    nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(campaign,
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = campaign)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Campaign") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-camp/boxplot_order_camp_",
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
# function to display boxplot of elemental composition per campaign
boxplot_compo_fish_camp_full <- function(res_fish_tib) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(campaign,
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = campaign)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Campaign") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/per-camp/boxplot_order_camp_full.jpg",
                  scale = 1,
                  height = 12, width = 21
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


#'
#'
#'
#'
#'
# function to display densityplot of elemental composition of all fish
densplot_compo_fish_tot <- function(res_fish_tib) {
  
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
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/densplot_compo_fish_tot.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
table_compare_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  table <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey"))) |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     median = round(median(concentration_mg_kg_dw), 2), 
                     `2.5% quantile` = round(quantile(concentration_mg_kg_dw, 
                                                      probs = c(0.025)), 2), 
                     `97.5% quantile` = round(quantile(concentration_mg_kg_dw, 
                                                       probs = c(0.975)), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    tidyr::pivot_longer(cols = c(mean:cv), 
                        names_to = "Statistic",
                        values_to = "value") |>
    dplyr::mutate(Statistic = factor(Statistic, 
                                     levels = c("mean", "median", "sd", "cv",
                                                "2.5% quantile", "97.5% quantile"))) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = value) |>
    dplyr::arrange(type, Statistic)
  
  openxlsx::write.xlsx(table, 
                       file = "output/compo fish/stats_comp_prey_vs_not_prey.xlsx")
  
  
}


#'
#'
#'
#'
#'
# function to create barplot displaying CV for major and trace nutrients in
# both scats and prey
barplot_comp_nut_prey_not_prey <- function(res_fish_tib
) {
  
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")), 
                  major_or_trace = dplyr::case_when(Nutrient %in% c("Ca", "P", 
                                                                    "Na", "K", 
                                                                    "Mg") ~ "Major", 
                                                    Nutrient %in% c("Fe", "Zn",
                                                                    "Cu", "Mn", 
                                                                    "Se", "As", 
                                                                    "Ni","Co") ~ "Trace"), 
                  type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr::group_by(major_or_trace, Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     mean_norm = round(mean(conc_norm), 2),
                     `2.5_quant_norm` = round(quantile(conc_norm, 
                                                       probs = c(0.025)), 2), 
                     `97.5_quant_norm` = round(quantile(conc_norm, 
                                                        probs = c(0.975)), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = cv, 
                                   fill = major_or_trace), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant_norm`, 
                                         ymax = `97.5_quant_norm`), 
                            linewidth = 1, 
                            color = "#B4DAE5FF", 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = mean_norm), 
                        size = 3, 
                        color = "#B4DAE5FF", 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::scale_fill_manual(values = c("Major" = "#DE7862FF", 
                                          "Trace" = "#1D2645FF")) +
    ggplot2::facet_wrap(~ type) +
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
                   legend.key.height = ggplot2::unit(1, "cm")
    )
  ggplot2::ggsave("output/compo fish/Cv_per_nut_comp_prey_vs_not_prey.jpg",
                  scale = 1,
                  height = 4, width = 11
  )
  
  
  
  
}




#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
lineplot_compare_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     median = round(mean(concentration_mg_kg_dw), 2),
                     `2.5_quant` = round(quantile(concentration_mg_kg_dw, 
                                                  probs = c(0.025)), 2), 
                     `97.5_quant` = round(quantile(concentration_mg_kg_dw, 
                                                   probs = c(0.975)), 2),
                     mean_norm = round(mean(conc_norm), 2),
                     median_norm = round(mean(conc_norm), 2),
                     `2.5_quant_norm` = round(quantile(conc_norm, 
                                                       probs = c(0.025)), 2), 
                     `97.5_quant_norm` = round(quantile(conc_norm, 
                                                        probs = c(0.975)), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                           "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/lineplot_compo_prey_vs_not_prey_abs.jpg",
                  scale = 1,
                  height = 6, width = 5
  )
  
}


#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
lineplot_compare_compo_prey_not_prey_mean_sp_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr::group_by(type, Species, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw)) |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(mean_sp), 2), 
                     median = round(mean(mean_sp), 2),
                     `2.5_quant` = round(quantile(mean_sp, 
                                                  probs = c(0.025)), 2), 
                     `97.5_quant` = round(quantile(mean_sp, 
                                                   probs = c(0.975)), 2)) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                           "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/lineplot_compo_prey_vs_not_prey_mean_sp_abs.jpg",
                  scale = 1,
                  height = 5, width = 5
  )
  
}




#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
boxplot_compare_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Nutrient, 
                                       y = conc_norm, 
                                       color = type), 
                          size = 3, 
                          position = ggplot2::position_dodge(0.5)) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                           "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/boxplot_compo_prey_vs_not_prey_abs.jpg",
                  scale = 1,
                  height = 8, width = 9
  )
  
}



#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
boxplot_compare_compo_prey_not_prey_rel <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    dplyr::mutate(sum = As + Co + Cu + Fe + 
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey"))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                          "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/compo_prey_vs_not_prey_rel_trace_only.jpg",
                  scale = 1,
                  height = 8, width = 9
  )
  
}




#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# absolute concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = concentration_mg_kg_dw)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_absolute.xlsx")
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# absolute concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_mean_sp_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) |>
    dplyr::group_by(type, Species, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = mean_sp)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_absolute_mean_sp.xlsx")
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# relative concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_rel_trace_only <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    dplyr::mutate(sum = As + Co + Cu + Fe + 
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = relative_concentration)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_rel_trace_only.xlsx")
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# relative concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_rel_all_nut <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = relative_concentration)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_rel_all_nut.xlsx")
  
}
