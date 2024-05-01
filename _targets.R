################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# _targets.R
#
# Script decomposing all steps of the analysis with target
################################################################################

library("targets")


# Source all functions contained in all files in the R directory
lapply(list.files(here::here("R"),
                  recursive = TRUE, full.names = T),
       source)


list(
  #################### DATA : SAMPLES  #########################################
  # script 01_summarize_data_samples.R
  # define and load data on samples of fish
  tar_target(data_file,
             "data/data_fish_YC.xlsx", 
             format = "file"),
  tar_target(data_samples, load_xl(data_file)),
  # define and load results of composition of fish samples
  tar_target(output_compo_file,
             "data/res_compo_fish.xlsx", 
             format = "file"),
  tar_target(output_compo, 
             load_xl(output_compo_file)),
  # summarize data on samples 
  tar_target(data_summary, summary_samples(data_samples, 
                                           output_compo)),
  
  #################### RESULTS : SAMPLE COMPOSITION ############################
  # identify and assess proportion of loq values 
  tar_target(nut_under_loq, ID_under_loq_values(output_compo)), 

  # table with concentrations per species 
  tar_target(table_fish_sp_conc_results, 
             table_compo_fish_sp_without_loq_replaced(output_compo)),
  tar_target(table_fish_sp_conc_results_loq_replaced, 
             table_compo_fish_sp_with_loq_replaced(output_compo)),
  # add * on following species and nutrient values to indicate it includes
  # samples with conc < loq
  # Ag: Mancopsetta mancopsetta (1), Bathydraco antarcticus (1), 
  # Echiodon cryomargarites (2),
  # Champsocephalus gunnari (3), Channichthys rhinoceratus (7),
  # Electrona antarctica (2), Gymnoscopelus bolini (4), 
  # Gymnoscopelus braueri (9),
  # Dissostichus eleginoides (2), Gobionotothen acuta (3), 
  # Lindbergichthys mizops (2),
  # Notolepis coatsi (3), Stomias sp (2)
  # Pb: Champsocephalus gunnari (7), Channichthys rhinoceratus (3),
  # Gobionotothen acuta (3), Muraenolepis sp (2)
  
  ################## DESCRIPTION COMPOSITION ###################################
  tar_target(density_plot, 
             density_plot_all_nut(table_fish_sp_conc_results_loq_replaced)),
  tar_target(barplot_compo_relative, 
             barplot_nut_fish_compo_relative(table_fish_sp_conc_results_loq_replaced)),
  tar_target(barplot_compo_CV, 
             barplot_nut_CV(table_fish_sp_conc_results_loq_replaced)),
  tar_target(corrplot_covariation_nut, 
             corr_compo_fish(table_fish_sp_conc_results_loq_replaced))
  
  ####### IDENTIFY ANALYTICAL OUTLIERS + POTENTIAL CONTAMINATION ###############
  ###### samples identified with adnormal values must be treated independently 
  # with several possibilities: 
  # - if they display adnormal values for a majority of nutrients (more than 7), 
  #   there must have been a contamination during sample preparation ----> 
  #   THESE ARE REMOVED 
  # - if not, then analysis can be rerun for same samples, 2 possible outcomes 
  #       - value is back into "normal" range --> REPLACE ODD VALUE WITH NEW ONE
  #       - values is still "abnormal" --> KEEP SAMPLE FOR CHARACTERIZATION OF 
  #                                       COMPOSITION OF SAMPLE AND FOR 
  #                                       STATISTICAL ANALYSIS REPLACE VALUE 
  #                                       WITH VALUE OF HIGHER QUANTILE (97.5 %) 
  #                                       OF OTHER SAMPLES FOR SAME ELEMENT
  # script 02_id_technical_outliers.R
  # 
  # # -------------- FISH ------------------
  # # identify outliers
  # # statistical outliers
  # tar_target(boxplot_outliers_fish_stats, 
  #            boxplot_id_outliers_stats(res_compo_fish, 
  #                                      "fish")), # boxplot
  # tar_target(tib_outliers_fish_stats, 
  #            tib_id_outliers(res_compo_fish, 
  #                            data_fish_samples, 
  #                            "fish", "fish_id_outliers")), # tibble
  # # technical outliers 
  # tar_target(hist_outliers_fish_tech, 
  #            hist_id_outliers(res_compo_fish, "fish")), # histograms
  # ### two samples identified as potentially contaminated during sample preparation
  # ### other samples identified with high values for just a few elements were rerun for analysis
  # ### and values were confirmed: they are kept as is, eventually to modify for statistical analysis
  # # get rid of technical outliers + add campaign data 
  # # format by adding campaign data and identify sp of prey included in diet of Agazella
  # # & remove contaminated samples (2005_PROTAND_PA03 & 2010PII_ARCTRIS_CHA94_AR01)
  # # & remove unwanted elements
  # tar_target(res_compo_fish_notechoutliers, res_compo_fish |> 
  #              dplyr::filter(!(Code_sample %in% c("2005_PROTAND_PA03",
  #                                                 "2010PII_ARCTRIS_CHA94_AR01"))) |>
  #              dplyr::select(-c(Mo, V, Cr)) |>
  #              dplyr::mutate(Ag = as.numeric(Ag), 
  #                            Pb = as.numeric(Pb))) 
  
  
  )
