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
  # script 01.summarize_data_samples.R
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
  
  ########### RESULTS : TECHNICAL RESULTS WRANGLING ############################
  # script 02.id_technical_outliers_and_loq.R
  ######### outliers
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
  # boxplot
  tar_target(id_outliers_boxplot, 
             boxplot_id_outliers_stats(output_compo)), 
  # histograms
  tar_target(id_outliers_hist, 
             hist_id_outliers(output_compo)),
  # table
  tar_target(tib_outliers_stats, 
             tib_id_outliers(output_compo, 
                             data_samples)),
  
  ### two samples identified as potentially contaminated during sample preparation
  ### other samples identified with high values for just a few elements were rerun for analysis
  ### and values were confirmed: they are kept as is, eventually to modify for statistical analysis
  # remove contaminated samples (2005_PROTAND_PA03 & 2010PII_ARCTRIS_CHA94_AR01)
  tar_target(output_compo_notechoutliers, 
             remove_tech_outliers_and_clean_names(output_compo)),
  
  ########### loq
  # identify and assess proportion of loq values 
  tar_target(nut_under_loq, ID_under_loq_values(output_compo_notechoutliers)), 

  # table with concentrations per samples 
  tar_target(table_fish_samples_conc_results_clean, 
             table_compo_fish_samples_with_loq_replaced(output_compo_notechoutliers)),
  # summaries across samples
  tar_target(table_summary_compo_samples, 
             table_compo_fish_sp(table_fish_samples_conc_results_clean)),
  # table with concentrations averaged per species 
  tar_target(table_fish_sp_conc_results_clean, 
             table_compo_fish_sp_with_loq_replaced(output_compo_notechoutliers)),
  tar_target(table_fish_sp_conc_results_without_loq_replaced, 
             table_compo_fish_sp_without_loq_replaced(output_compo_notechoutliers)),
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
  # script 03.description_compo.R
  # general patterns per nutrient with conc. averaged per species
  #┌ keep only mean per species from the summary table 
  tar_target(tibble_mean_compo_per_sp, 
             mean_per_species(table_fish_sp_conc_results_clean)),
  tar_target(density_plot, 
             density_plot_all_nut(tibble_mean_compo_per_sp)),
  tar_target(barplot_compo_relative, 
             barplot_nut_fish_compo_relative(tibble_mean_compo_per_sp)),
  tar_target(table_compo_relative, 
             table_nut_fish_compo_relative(tibble_mean_compo_per_sp)),
  tar_target(barplot_compo_CV, 
             barplot_nut_CV(tibble_mean_compo_per_sp)),
  tar_target(corrplot_covariation_nut, 
             corr_compo_fish(tibble_mean_compo_per_sp)), 
  
  # sample values per species
  tar_target(boxplot_compo_per_sp_all_nut, 
             boxplot_compo_sp_all_nut(table_fish_samples_conc_results_clean)),
  # linepots with mean per species and only one nutrient
  # gradient of color ordered according to Fe concentrations
  tar_target(boxplot_fish_sp_nut_grad_Ca, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Ca")),
  tar_target(boxplot_fish_sp_nut_grad_P, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "P")),
  tar_target(boxplot_fish_sp_nut_grad_Na, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Na")),
  tar_target(boxplot_fish_sp_nut_grad_K, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "K")),
  tar_target(boxplot_fish_sp_nut_grad_Mg, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Mg")),
  tar_target(boxplot_fish_sp_nut_grad_Sr, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Sr")),
  tar_target(boxplot_fish_sp_nut_grad_Fe, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                "Fe")),
  tar_target(boxplot_fish_sp_nut_grad_Zn, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                "Zn")),
  tar_target(boxplot_fish_sp_nut_grad_Cu, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Cu")),
  tar_target(boxplot_fish_sp_nut_grad_Mn, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Mn")),
  tar_target(boxplot_fish_sp_nut_grad_As, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                "As")),
  tar_target(boxplot_fish_sp_nut_grad_Ni, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Ni")),
  tar_target(boxplot_fish_sp_nut_grad_Se, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Se")),
  tar_target(boxplot_fish_sp_nut_grad_Cd, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Cd")),
  tar_target(boxplot_fish_sp_nut_grad_Co, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Co")),
  tar_target(boxplot_fish_sp_nut_grad_Ag, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Ag")),
  tar_target(boxplot_fish_sp_nut_grad_Pb, 
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_results_clean, 
                                                 "Pb")),
  # a version to get a good picture of the legend for the above
  tar_target(boxplot_fish_sp_nut_grad_legend, 
             lineplot_compo_fish_sp_one_nut_legend(table_fish_samples_conc_results_clean, 
                                                  "As")),
  
  ########### ANALYSIS : PREPARE PCA and HC #####################################
  # script 04.handle_stat_outliers_and_loq.R
  tar_target(table_fish_compo_no_stat_outliers, 
             replace_outliers_conc(table_fish_samples_conc_results_clean)),
  tar_target(table_fish_compo_for_stat_analysis, 
             set_up_tible_for_stat_analysis(table_fish_compo_no_stat_outliers)),
  ########### ANALYSIS : PCA and HC #####################################
  # script 05.PCA.R
  tar_target(fish_pca_essentials,
             pca_coda_essentials(table_fish_compo_for_stat_analysis)),
  tar_target(fish_pca_with_non_essentials,
             pca_coda_essentials_and_not(table_fish_compo_for_stat_analysis)),
  tar_target(biplot_pca_essentials,
             biplot_pca_coda(fish_pca_essentials,
                             table_fish_compo_for_stat_analysis,
                             "essentials_only",
                             groups = "Family",
                             ellipse = FALSE)),
  tar_target(biplot_pca_with_non_essentials,
             biplot_pca_coda(fish_pca_with_non_essentials,
                             table_fish_compo_for_stat_analysis,
                             "all_nutrients",
                             groups = "Family",
                             ellipse = TRUE)),
  # script 06.HC.R
  tar_target(fish_HC_essentials_find_k,
             clust_find_k_table_PCs(fish_pca_essentials,
                                    "HC_table_essentials_only")),
  tar_target(fish_HC_essentials_find_k_boxplot,
             boxplot_clust_find_k_val(fish_HC_essentials_find_k,
                                    "HC_table_essentials_only")),
  tar_target(fish_HC_essentials_find_k_means,
             means_clust_find_k_val(fish_HC_essentials_find_k,
                                      "HC_table_essentials_only")),
  tar_target(fish_HC_essentials_k4,
             clust_compo_PCs(fish_pca_essentials,
                             k = 4,
                             "HC_essentials_only")),
  # tar_target(fish_HC_essentials_dendro_fam_k4,
  #            clust_compo_PCs_dendro(fish_pca_essentials,
  #                                   table_fish_compo_for_stat_analysis,
  #                                   k = 4,
  #                                   "HC_essentials_only")),
  tar_target(fish_HC_essentials_dendro_clust_k4,
             clust_compo_PCs_dendro(fish_pca_essentials,
                                    table_fish_compo_for_stat_analysis,
                                    k = 4,
                                    "HC_essentials_only", 
                                    colour = "Cluster")),
  # tar_target(fish_HC_essentials_dendro_hab_k4,
  #            clust_compo_PCs_dendro(fish_pca_essentials,
  #                                   table_fish_compo_for_stat_analysis,
  #                                   k = 4,
  #                                   "HC_essentials_only", 
  #                                   colour = "Habitat")),
  # tar_target(fish_HC_essentials_dendro_diet_k4,
  #            clust_compo_PCs_dendro(fish_pca_essentials,
  #                                   table_fish_compo_for_stat_analysis,
  #                                   k = 4,
  #                                   "HC_essentials_only", 
  #                                   colour = "diet")),
  tar_target(fish_HC_essentials_boxplots_k4,
             boxplot_compo_clust(fish_HC_essentials_k4,
                                 table_fish_compo_for_stat_analysis,
                                 "HC_essentials_only")),
  tar_target(fish_HC_essentials_table_stats_k4,
             stats_compo_clust(fish_HC_essentials_k4,
                                 table_fish_compo_for_stat_analysis,
                                 "HC_essentials_only")),
  tar_target(fish_HC_essentials_table_sp_attribution_k4,
             clust_sp_attrib(fish_HC_essentials_k4,
                               table_fish_compo_for_stat_analysis,
                               "HC_essentials_only")),
  tar_target(fish_HC_essentials_MWtest_k4,
             MWtest_clust_k4(fish_HC_essentials_k4,
                             table_fish_compo_for_stat_analysis,
                             "HC_essentials_only")),
  tar_target(fish_HC_essentials_biplot_k4,
             biplot_after_clust(fish_pca_essentials,
                                fish_HC_essentials_k4,
                                table_fish_compo_for_stat_analysis,
                                "HC_essentials_only")),
  
  tar_target(fish_HC_with_non_essentials,
             clust_compo_PCs(fish_pca_with_non_essentials,
                             k = 4,
                             "HC_all_nutrients")),
  # tar_target(fish_HC_with_non_essentials_dendro_fam_k4,
  #            clust_compo_PCs_dendro(fish_pca_with_non_essentials,
  #                                   table_fish_compo_for_stat_analysis,
  #                                   k = 4,
  #                                   "HC_all_nutrients")),
  tar_target(fish_HC_with_non_essentials_dendro_clust_k4,
             clust_compo_PCs_dendro(fish_pca_with_non_essentials,
                                    table_fish_compo_for_stat_analysis,
                                    k = 4,
                                    "HC_all_nutrients",
                                    colour = "Cluster"))
  
  
  
  )
