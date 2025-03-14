################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# May 2024
# 04.handle_stat_outliers.R
#
# Script with functions to handle statistical outliers identified after 
# description of compositions and handle them before conducting statistical 
# analysis
################################################################################


# statistical and technical outliers were identified as outliers per species and
# per element
# ie boxplot of elemental concentrations per species and element were 
# examined to detect any adnormal value - as compared to other samples
# of the same species but also to samples of other species

# Were identified as adnormal values that could mess up with statistical 
# methods results: 
###### 2010PII_MACRCAR_CHA103_MC07 for Ni
###### 2005_GYMNFRA_GF11 for Zn
###### 2005_STOMSP_SS09 for Fe
###### 2005_NOTOCOA_NC03 for Fe
###### 2005_PARAGRA_PG06 for Fe 

# for statistical analysis, these values will be replaced by the highest 
# quantile of all samples for the nutrient

#'
#'
#'
#'
#'
# function to replace IDed adnormal values per highest quantiles 
replace_outliers_conc <- function(compo_tib) {
  
  # calculate quantiles 
  all_quant <- compo_tib |>
    # exclude unwanted samples 
    dplyr::filter(!(Code_sample %in% c("2010PII_MACRCAR_CHA103_MC07",
                                       "2005_GYMNFRA_GF11",
                                       "2005_STOMSP_SS09",
                                       "2005_NOTOCOA_NC03",
                                       "2005_PARAGRA_PG06"))) |>
    # get rid of unneeded nutrient
    dplyr::select(c(Code_sample, Fe, Zn, Ni)) |> 
    tidyr::pivot_longer(cols = c("Fe":"Ni"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(high_quant = quantile(concentration_mg_g_dw, 0.975)) |> 
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = high_quant)
  
  quant_zn <- all_quant$Zn
  quant_ni <- all_quant$Ni
  quant_fe <- all_quant$Fe
  
  compo_tib |>
    dplyr::mutate(Fe = dplyr::case_when(Code_sample %in% c("2005_STOMSP_SS09",
                                                           "2005_NOTOCOA_NC03",
                                                           "2005_PARAGRA_PG06") ~ quant_fe, 
                                        TRUE ~ Fe),
                  Ni = dplyr::case_when(Code_sample == "2010PII_MACRCAR_CHA103_MC07" ~ quant_ni,
                                        TRUE ~ Ni), 
                  Zn = dplyr::case_when(Code_sample == "2005_GYMNFRA_GF11" ~ quant_zn,
                                        TRUE ~ Zn)) |>
    dplyr::select(Code_sample, Family, Species,
                  Ca, P, Na, K, Mg, 
                  Fe, Zn, Cu, Mn,
                  As, Se, Ni, Co, 
                  Sr, Cd, Ag, Pb)
}


#'
#'
#'
#'
#'
# function to replace IDed adnormal values per highest quantiles 
set_up_tible_for_stat_analysis <- function(compo_tib_no_outliers) {
  
  compo_tib_no_outliers |>
    # compute mean per sp as stat. analysis will be computed on means per sp
    tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg, 
                                 Fe, Zn, Cu, Mn,
                                 As, Se, Ni, Co, 
                                 Sr, Cd, Ag, Pb), 
                        names_to = 'Nutrient', 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Family, Species, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     mean = round(mean(concentration_mg_kg_dw, 
                                       na.rm = TRUE), 3)) |> 
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = mean) |>
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
                  Sr, Cd, Ag, Pb)
}