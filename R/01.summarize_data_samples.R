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
    # add data on habitat as given on Fishbase
    dplyr::mutate(Habitat = dplyr::case_when(Species %in% c("Gobionotothen acuta", 
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
                                                            "Protomyctophum tenisoni") ~ "Bathypelagic")) |>
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