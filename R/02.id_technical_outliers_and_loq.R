################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# May 2024
# 02.id_technical_outliers_and_loq.R
#
# Script with functions to identify analytical outliers and samples that were
# potentially contaminated when processed
################################################################################

################################  OUTLIERS #####################################
#'
#'
#'
#'
#'
# function to display boxplots with labeled outliers
boxplot_id_outliers_stats <- function(fish_compo_tib) {
  
  # small function to find statistical outliers as an indication of 
  # potential adnormal values 
  find_outlier <- function(x) {
    return(x < quantile(x, 
                        .25) - 1.5*IQR(x) | x > quantile(x,
                                                         .75) + 1.5*IQR(x))  }
  
  fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |> # too many values under loq
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
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), 
                                   Code_sample, NA), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb"))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm=TRUE, 
                       hjust=-.1, size = 2.5) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#26432FFF", "#D98594FF",
                                          "#2e276a", "#5c4d73",
                                          "#DE7862FF","#D8AF39FF",
                                          "#583B2BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave("output/01.technical_outliers_and_loq/boxplot_stat_outliers.jpg", 
                  scale = 1,
                  height = 5, width = 10
  )
  
}

#'
#'
#'
#'
#'
# function to show histograms and identify extreme values 
# analytical outliers 
hist_id_outliers <- function(fish_compo_tib
) {
  
  fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |> # too many values under loq
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
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2], 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb"))) |>
    dplyr::group_by(Nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#26432FFF", "#D98594FF",
                                          "#2e276a", "#5c4d73",
                                          "#DE7862FF","#D8AF39FF",
                                          "#583B2BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave("output/01.technical_outliers_and_loq/hist_id_outliers.jpg",
                  scale = 1,
                  height = 5, width = 7
  )
  
}

#'
#'
#'
#'
#'
# function to generate tables with all samples identified as 
# statistical outliers, and 
tib_id_outliers <- function(fish_compo_tib, # tibble with results of compo of prey
                            fish_samples_tib, # tibble with data on samples 
                            name_file
) {
  
  # small function to find outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  options(scipen = 999)
  
  table <- fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |> # too many values under loq
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
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Code_sample = 
                    dplyr::case_when(Code_sample == "2005_GYMNBOL_GB6AB" ~ "2005_GYMNBOL_GB6", # name not adapted when gone to analysis
                                     TRUE ~ Code_sample), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", "Sr",
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Ni", "Se",
                                               "Cd", "Co", "Ag", "Pb"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), 
                                   Code_sample, NA), 
                  mean_all = mean(concentration_mg_g_dw), 
                  min_all = min(concentration_mg_g_dw), 
                  max_all = max(concentration_mg_g_dw)) |>
    dplyr::filter(!is.na(outlier)) |> # keep only samples with abnormal values 
    dplyr::left_join(fish_samples_tib |>
                       dplyr::rename(Code_sample = Code_new_format),
                     by = "Code_sample") |>
    dplyr::select(Code_sample, Nutrient, concentration_mg_g_dw, 
                  mean_all, min_all, max_all,
                  Water_percent, Prepa_operator, Comment) |>
    dplyr::arrange(Code_sample, Nutrient)
  
  openxlsx::write.xlsx(table,
                       file ="output/01.technical_outliers_and_loq/fish_id_outliers.xlsx")
  
}

#'
#'
#'
#'
#'
# function to generate tables with all samples identified as 
# statistical outliers, and clean names (sp changed afterwards thanks to 
# the expertise of Yves Cherel, and sample codes are also adapted)
remove_tech_outliers_and_clean_names <- function(fish_compo_tib
) {
  # this table is the one that will be made available 
  table <- fish_compo_tib |>
    dplyr::filter(!(Code_sample %in% c("2005_PROTAND_PA03",
                                       "2010PII_ARCTRIS_CHA94_AR01"))) |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Mancopsetta mancopsetta" ~ "Mancopsetta maculata", 
                                             Species == "Muraenolepis sp" ~ "Muraenolepis marmorata", 
                                             TRUE ~ Species),
                  # name of species corrected by Yves Cherel after analysis
                  # completed
                  Code_sample = dplyr::case_when(Species == "Mancopsetta maculata" ~ 
                                                   stringr::str_replace(Code_sample,
                                                                        "MANCMAN","MANCMAC"),
                                                 Species == "Mancopsetta maculata" ~ 
                                                   stringr::str_replace(Code_sample,
                                                                        "MURASP","MURAMAR"), 
                                                 TRUE ~ Code_sample))
  
  openxlsx::write.xlsx(table,
                       file ="output/01.technical_outliers_and_loq/Kerguelen_fish_samples_compo_results_no-tech-outliers.xlsx")
  
  table
  
}


############################## LOQ #############################################
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
                       file = "output/01.technical_outliers_and_loq/samples_fish_nut_under_LOQ.xlsx")
  
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
                       file = "output/01.technical_outliers_and_loq/samples_fish_sp_nut_under_LOQ.xlsx")
  
  list(table_LOQ_all, table_LOQ_sp)
  
}


#'
#'
#'
#'
#'
# function to have table of elemental composition per samples
# with values under loq replaced by loq/2, so it depends on samples and/or
# nutrients
table_compo_fish_samples_with_loq_replaced <- function(fish_compo_tib) {
  
  fish_compo_tib |>
    dplyr::select(-c(Cr, Mo, V)) |>
    dplyr::mutate(Pb = as.numeric(Pb), 
                  Sr = as.numeric(Sr), 
                  Ag = as.numeric(Ag)) |>
    dplyr::mutate(Ag = dplyr::case_when(is.na(Ag) ~ 0.01/2, 
                                        # loq is 0.01 for all samples 
                                        TRUE ~ Ag), 
                  Pb = dplyr::case_when(is.na(Pb) ~ 0.01/2, 
                                        # loq is 0.01 for all samples 
                                        TRUE ~ Pb),
                  Species = dplyr::case_when(Species == "Mancopsetta mancopsetta" ~ "Mancopsetta maculata", 
                                             Species == "Muraenolepis sp" ~ "Muraenolepis marmorata", 
                                             TRUE ~ Species),
                  # name of species corrected by Yves Cherel after analysis
                  # completed
                  Code_sample = dplyr::case_when(Species == "Mancopsetta maculata" ~ 
                                                   stringr::str_replace(Code_sample,
                                                                        "MANCMAN","MANCMAC"),
                                                 Species == "Mancopsetta maculata" ~ 
                                                   stringr::str_replace(Code_sample,
                                                                        "MURASP","MURAMAR"), 
                                                 TRUE ~ Code_sample))

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
                       values_from = value) |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Mancopsetta mancopsetta" ~ "Mancopsetta maculata", 
                                             Species == "Muraenolepis sp" ~ "Muraenolepis marmorata", 
                                             TRUE ~ Species))
  
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
                       values_from = value) |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Mancopsetta mancopsetta" ~ "Mancopsetta maculata", 
                                             Species == "Muraenolepis sp" ~ "Muraenolepis marmorata", 
                                             TRUE ~ Species))
  
  openxlsx::write.xlsx(table, 
                       file = paste0("output/summary_fish_compo_sp_loq_replaced.xlsx"))
  
  table
  
}

