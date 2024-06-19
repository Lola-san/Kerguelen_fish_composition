################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# 01_summarize_data_samples.R
#
# Script with functions to show data on samples
################################################################################


# load excel files  
load_xl <- function(pathxl) {
  readxl::read_xlsx(pathxl)
}

# clean file and summarise data on samples
summary_samples <- function(sample_data, 
                            sample_compo) {
  table <- sample_data |>
    dplyr::filter(Code_new_format %in% sample_compo$Code_sample) |>
    # add data on habitat as given on Fishbase + Yves Cherel expertise
    dplyr::mutate(Species = dplyr::case_when(Species == "Mancopsetta mancopsetta" ~ "Mancopsetta maculata", 
                                             Species == "Muraenolepis sp" ~ "Muraenolepis marmorata", 
                                             TRUE ~ Species),
                  # name of species corrected by Yves Cherel after analysis
                  # completed
                  Habitat = dplyr::case_when(Species %in% c("Bathylagus tenuis",
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
                                                            "Gobionotothen acuta"
                                                            ) ~ "Demersal", 
                                             Species %in% c("Bathydraco antarcticus",
                                                            "Echiodon cryomargarites",
                                                            "Macrourus carinatus") ~ "Bathydemersal" 
                                             )) |>
    dplyr::group_by(Family, Species, Habitat) |>
    dplyr::mutate(SL_cm = as.integer(SL_cm)) |> # generates warnings because
    # of samples with approximate length (*XX) as they were damaged
    dplyr::summarize(n = dplyr::n_distinct(Code_new_format), 
                     length_mean = mean(SL_cm, na.rm = TRUE), 
                     length_min = min(SL_cm, na.rm = TRUE), 
                     length_max = max(SL_cm, na.rm = TRUE), 
                     length_sd = sd(SL_cm, na.rm = TRUE),  
                     length_cv = sd(SL_cm, na.rm = TRUE)/mean(SL_cm, na.rm = TRUE),
                     H20_mean = mean(Water_percent), 
                     H20_min = min(Water_percent), 
                     H20_max = max(Water_percent), 
                     H20_sd = sd(Water_percent), 
                     H20_cv = H20_sd/H20_mean)
  
  openxlsx::write.xlsx(table, 
                       file = "output/summary_data_samples_sp.xlsx")
}