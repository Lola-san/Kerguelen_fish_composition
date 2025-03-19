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
  #--------- the above was used to build Table 1 in the article ----------------
  
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

  # table with concentrations per samples with concentration replaced by 
  # loq/2 when concentrations was < loq
  tar_target(table_fish_samples_conc_with_loq_replaced, 
             table_compo_fish_samples_with_loq_replaced(output_compo_notechoutliers)),
  # to be clear, identify the table without outliers and without loq replaced
  tar_target(table_fish_samples_conc_without_loq_replaced, 
             output_compo_notechoutliers),
  
  ################## DESCRIPTION COMPOSITION ###################################
  # script 03.description_compo.R
  # TABLES
  # general summaries per nutrient across samples 
  tar_target(table_summary_compo_samples_with_loq_replaced, 
             summarise_compo_fish_across_samples(table_fish_samples_conc_with_loq_replaced, 
                                      "with_loq_replaced")),
  tar_target(table_summary_compo_samples_without_loq_replaced, 
             summarise_compo_fish_across_samples(table_fish_samples_conc_without_loq_replaced, 
                                      "without_loq_replaced")),
  # general summaries per nutrient across species with sp averaging before
  tar_target(table_summary_compo_all_sp_with_loq_replaced, 
             summarise_compo_fish_across_sp(table_fish_samples_conc_with_loq_replaced, 
                                            "with_loq_replaced")),
  tar_target(table_summary_compo_all_sp_without_loq_replaced, 
             summarise_compo_fish_across_sp(table_fish_samples_conc_without_loq_replaced, 
                                            "without_loq_replaced")),
  # general summaries per nutrient per species 
  tar_target(table_summary_compo_per_sp_with_loq_replaced, 
             summarise_compo_fish_per_sp(table_fish_samples_conc_with_loq_replaced, 
                                            "with_loq_replaced")),
  tar_target(table_summary_compo_per_sp_without_loq_replaced, 
             summarise_compo_fish_per_sp(table_fish_samples_conc_without_loq_replaced, 
                                            "without_loq_replaced")),
  #--------- the above (table_summary_compo_per_sp_without_loq_replaced) -------
  # was Table 2 in the article -------------------------------------------------
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
  
  # table with only the mean composition per species (with loq replaced)
  # ------- to use for some plots and statistical analysis --------------------
  tar_target(tibble_mean_compo_per_sp,
             mean_per_species(table_summary_compo_per_sp_with_loq_replaced)),
  # compute average nutrient relative fractions
  tar_target(table_relative_compo_per_sp_with_loq_replaced, 
             table_nut_fish_compo_relative(tibble_mean_compo_per_sp)),

  # PLOTS - exploring the variability in species nutrient composition
  tar_target(density_plot,
             density_plot_all_nut(tibble_mean_compo_per_sp)),
  #--------- the above generates Fig.2 in the article --------------------------
  tar_target(barplot_compo_relative,
             barplot_nut_fish_compo_relative(tibble_mean_compo_per_sp)),
  #--------- the above generates Fig.4 in the article --------------------------
  tar_target(corrplot_covariation_nut,
            corr_compo_fish(tibble_mean_compo_per_sp)),
  #--------- the above generates Fig.5 in the article --------------------------
  #--------- and first data sheet of Appendix A supplementary material ---------
  
  # now looking species per species, so using all samples data and not mean
  # per species
  tar_target(boxplot_fish_sp_nut_grad_P,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "P")),
  tar_target(boxplot_fish_sp_nut_grad_Fe,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Fe")),
  tar_target(boxplot_fish_sp_nut_grad_As,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "As")),
  tar_target(boxplot_fish_sp_nut_grad_Zn,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Zn")),
  # a version to get a good picture of the legend for the above
  tar_target(boxplot_fish_sp_nut_grad_legend,
             lineplot_compo_fish_sp_one_nut_legend(table_fish_samples_conc_with_loq_replaced,
                                                   "As")),
  #--------- the aboves were combined to make Fig.3 in the article -------------
  
  ##### other exploration plots 
  tar_target(barplot_compo_CV,
             barplot_nut_CV(tibble_mean_compo_per_sp)),

  # nutrient values per species
  tar_target(boxplot_compo_per_sp_all_nut,
             boxplot_compo_sp_all_nut(table_fish_samples_conc_with_loq_replaced)),
  # linepots with mean per species and only one nutrient
  # gradient of color ordered according to Fe concentrations
  tar_target(boxplot_fish_sp_nut_grad_Ca,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Ca")),
  tar_target(boxplot_fish_sp_nut_grad_Na,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Na")),
  tar_target(boxplot_fish_sp_nut_grad_K,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "K")),
  tar_target(boxplot_fish_sp_nut_grad_Mg,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Mg")),
  tar_target(boxplot_fish_sp_nut_grad_Sr,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Sr")),
  tar_target(boxplot_fish_sp_nut_grad_Cu,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Cu")),
  tar_target(boxplot_fish_sp_nut_grad_Mn,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Mn")),
  tar_target(boxplot_fish_sp_nut_grad_Ni,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Ni")),
  tar_target(boxplot_fish_sp_nut_grad_Se,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Se")),
  tar_target(boxplot_fish_sp_nut_grad_Cd,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Cd")),
  tar_target(boxplot_fish_sp_nut_grad_Co,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Co")),
  tar_target(boxplot_fish_sp_nut_grad_Ag,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Ag")),
  tar_target(boxplot_fish_sp_nut_grad_Pb,
             lineplot_compo_fish_sp_one_nut_grad(table_fish_samples_conc_with_loq_replaced,
                                                 "Pb")),

  ########### ANALYSIS : PREPARE PCA and HC #####################################
  # script 04.handle_stat_outliers_and_loq.R
  tar_target(table_fish_compo_no_stat_outliers,
             replace_outliers_conc(table_fish_samples_conc_with_loq_replaced)),
  tar_target(table_fish_compo_for_stat_analysis,
             set_up_tible_for_stat_analysis(table_fish_compo_no_stat_outliers)),
  ########### ANALYSIS : PCA and HC #####################################
  # script 05.PCA.R
  tar_target(fish_pca_essentials,
             pca_coda_essentials(table_fish_compo_for_stat_analysis)),
  tar_target(biplot_pca_essentials,
             biplot_pca_coda(fish_pca_essentials,
                             table_fish_compo_for_stat_analysis,
                             "essentials",
                             groups = "Family",
                             ellipse = FALSE)),
  # script 06.HC.R
  tar_target(fish_HC_essentials_find_k,
             clust_find_k_table_PCs(fish_pca_essentials,
                                    "HC_table_essentials")),
  tar_target(fish_HC_essentials_find_k_boxplot,
             boxplot_clust_find_k_val(fish_HC_essentials_find_k,
                                    "HC_table_essentials")),
  tar_target(fish_HC_essentials_find_k_means,
             means_clust_find_k_val(fish_HC_essentials_find_k,
                                      "HC_table_essentials")),
  tar_target(fish_HC_essentials_k4,
             clust_compo_PCs(fish_pca_essentials,
                             k = 4,
                             "HC_essentials")),
  tar_target(fish_HC_essentials_dendro_clust_k4,
             clust_compo_PCs_dendro(fish_pca_essentials,
                                    table_fish_compo_for_stat_analysis,
                                    k = 4,
                                    "HC_essentials",
                                    colour = "Cluster")),
  #----------- the above generates Fig.6 a -------------------------------------
  tar_target(fish_HC_essentials_boxplots_k4,
             boxplot_compo_clust(fish_HC_essentials_k4,
                                 table_fish_compo_for_stat_analysis,
                                 "HC_essentials")),
  #----------- the above generates Fig.6 b & c ---------------------------------
  tar_target(fish_HC_essentials_table_stats_k4,
             stats_compo_clust(fish_HC_essentials_k4,
                                 table_fish_compo_for_stat_analysis,
                                 "HC_essentials")),
  tar_target(fish_HC_essentials_table_sp_attribution_k4,
             clust_sp_attrib(fish_HC_essentials_k4,
                               table_fish_compo_for_stat_analysis,
                               "HC_essentials")),
  tar_target(fish_HC_essentials_MWtest_k4,
             MWtest_clust_k4(fish_HC_essentials_k4,
                             table_fish_compo_for_stat_analysis,
                             "HC_essentials")),
  #--------- the above generates 2nd data sheet of Appendix A ------------------
  #--------- supplementary material ---------
  tar_target(fish_HC_essentials_biplot_k4,
             biplot_after_clust(fish_pca_essentials,
                                fish_HC_essentials_k4,
                                table_fish_compo_for_stat_analysis,
                                "HC_essentials"))
  )
