################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr/lola.gilbert@lilo.org
#
# June 2024
# 06.HC.R
#
# Script with functions to compute clustering on compositional data 
################################################################################



#'
#'
#'
#'
# function to perform clustering with different cluster nb and plot 
# different validating values of the outputs
clust_find_k_table_PCs <- function(res_pca, 
                                   file_name,  
                                   k_range = c(2:10),
                                   method = "ward.D2"
) {
  
  pcomp <- c(1:5)
  
  # extract the data i.e coordinates of individuals on the PCs
  data.act <- as.data.frame(res_pca$scores[, pcomp])
  
  # define distance matrix
  d <- dist(data.act)
  
  # perform clustering
  tree <- stats::hclust(d, method = method)
  
  list_outputs <- list()
  
  for (i in k_range) {
    # cut the tree in k clusters 
    clust_output <- data.frame(cluster = cutree(tree = tree, k = i))
    
    # compute validity measures
    clust_stats <- fpc::cluster.stats(as.dist(d), clust_output$cluster)
    
    # and save them
    ki_df <- data.frame(k = clust_stats$cluster.number, 
                        method = method, 
                        size = clust_stats$cluster.size,
                        separation = round(clust_stats$separation, 3),
                        average.distance = round(clust_stats$average.distance, 3), 
                        median.distance = round(clust_stats$median.distance, 3),
                        avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 
                                             3), 
                        average.toother = round(clust_stats$average.toother, 3), 
                        min.clust.size = clust_stats$min.cluster.size)
    
    list_outputs <- append(list_outputs, list(ki_df))
    
  }
  
  df0 <- data.frame(k = NA, 
                    method = NA,
                    size = NA,
                    separation = NA,
                    average.distance = NA, 
                    median.distance = NA,
                    avg.silwidth = NA, 
                    average.toother = NA, 
                    min.clust.size = NA)
  
  for (i in 1:length(k_range)) {
    df0 <- rbind(df0, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df.to.plot <- df0[-1,]
  
  # create file
  openxlsx::write.xlsx(df.to.plot, 
                       file = paste0("output/05.HC/find_k_validity_measures_", 
                                     file_name,
                                     ".xlsx"))
  
  # call output
  df.to.plot
  
  
}


#'
#'
#'
#'
# function to show validating values of the outputs
# for different numbers of clusters on a boxplot
boxplot_clust_find_k_val <- function(find_k_output, 
                                     file_name
) {
  
  
  # set color palette 
  if (length(unique(find_k_output$k)) >= 7) {
    diff <- length(unique(find_k_output$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
  }
  
  find_k_output |>
    dplyr::mutate(k = as.factor(k)) |>
    tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                        names_to = "validity.variable", 
                        values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = k, y = value, group = k, fill = k)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~validity.variable, scale = "free") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 15), 
                   legend.position = "none")
  
  
  # save plot 
  ggplot2::ggsave(paste0("output/05.HC/", 
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 6, width = 8)
  
  
}



#'
#'
#'
#'
# function to show means of validating values of the outputs
# for different numbers of clusters
means_clust_find_k_val <- function(find_k_output, 
                                   file_name) {
  
  # set color palette 
  if (length(unique(find_k_output$k)) >= 7) {
    diff <- length(unique(find_k_output$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
  }
  
  find_k_output |>
    dplyr::mutate(k = as.factor(k)) |>
    tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                        names_to = "validity.variable", 
                        values_to = "value") |>
    dplyr::group_by(k, validity.variable) |>
    dplyr::summarize(mean = mean(value)) |>
    ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~validity.variable, scale = "free") +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 15), 
                   legend.position = "none")
  
  # save plot 
  ggplot2::ggsave(paste0("output/05.HC/validity_measures_means_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 6, width = 8)
  
}


#'
#'
#'
#'
# function to perform clustering
# using Principal components estimated by PCA
# with hclust algorithm
clust_compo_PCs <- function(res_pca, 
                            k, 
                            file_name, # of the table with validity measures
                            method = "ward.D2"
) {
  
  # define the PCs to keep (total > 90% of var. explained)
  pcomp <- c(1:5)
  
  # extract the data i.e coordinates of individuals on the PCs
  data.act <- as.data.frame(res_pca$scores[, pcomp])
  
  # define distance matrix
  d <- dist(data.act)
  
  # perform clustering
  tree <- stats::hclust(d, method = method)
  
  # cut the tree in k clusters and save output in a df
  clust_output <- data.frame(cluster = cutree(tree = tree, k = k))
  
  # compute validity measures and save them
  clust_stats <- fpc::cluster.stats(as.dist(d), clust_output$cluster)
  
  clust.val <- data.frame(k = clust_stats$cluster.number, 
                          method = method, 
                          size = clust_stats$cluster.size,
                          separation = round(clust_stats$separation, 3),
                          average.distance = round(clust_stats$average.distance, 3), 
                          median.distance = round(clust_stats$median.distance, 3),
                          avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 3), 
                          average.toother = round(clust_stats$average.toother, 3), 
                          min.clust.size = clust_stats$min.cluster.size) 
  
  openxlsx::write.xlsx(clust.val, 
                       file = paste0("output/05.HC/clust_PCs_validity_measures_", 
                                     file_name, 
                                     ".xlsx"))
  
  # output is the cluster attribution
  clust_output
  
}


#'
#'
#'
#'
# function to plot dendrogram for fish based on PC results of robust PCA
clust_compo_PCs_dendro <- function(res_pca, 
                                   compo_tib_mean_sp, 
                                   k, # useful when colour is "Cluster"
                                   file_name,
                                   method = "ward.D2",
                                   colour = "Family" # "Family", "Habitat" or "Cluster", "diet"
                                   
) {
  # select the 5 first PCs
  pcomp <- c(1:5)
  
  # extract the data i.e coordinates of individuals on the PCs
  data.act <- as.data.frame(res_pca$scores[, pcomp])
  
  # define distance matrix
  d <- dist(data.act)
  
  # perform clustering
  tree <- stats::hclust(d, method = method)
  
  # dendrogram
  dendro.dat <- ggdendro::dendro_data(tree, 
                                      type = "rectangle")
  
  # cut the tree in k clusters and save output in a df
  clust_output <- data.frame(cluster = stats::cutree(tree = tree, k = k))
  
  # identify species that are known to be prey of A. gazella
  compo_tib_full <- compo_tib_mean_sp |>
    dplyr::mutate(Species_n = dplyr::case_when(Species == "Stomias spp." ~ paste0("*Stomias* spp.", 
                                                                                " (n = ", 
                                                                                n, 
                                                                                ")"), 
                                               TRUE ~ paste0("*",
                                                             stringr::str_sub(Species, 
                                                                              start = 1, 
                                                                              end = 1), 
                                                             ". ", 
                                                             stringr::str_split_fixed(Species, " ", n = 2)[,2], 
                                                             "* (n = ", 
                                                             n, 
                                                             ")")))
  
  
  # change labels to species name and add colour grouping
  dendro.labels <- dendro.dat$labels |>
    dplyr::mutate(label = compo_tib_full$Species_n[tree$order], 
                  Family = compo_tib_full$Family[tree$order], 
                  Habitat = compo_tib_full$Habitat[tree$order], 
                  Cluster = factor(clust_output$cluster[tree$order]), 
                  diet = compo_tib_full$confirmed_forage_sp[tree$order]) |>
    dplyr::mutate(label = stringr::str_replace(label, "\n", " "), 
                  Family = factor(Family, 
                                  levels = c("Zoarcidae",
                                             "Stomiidae",
                                             "Paralepididae",
                                             "Nototheniidae",
                                             "Notosudidae",
                                             "Myctophidae",
                                             "Muraenolepididae",
                                             "Microstomatidae",
                                             "Melamphaidae",
                                             "Macrouridae",
                                             "Gempylidae",
                                             "Channichthyidae",
                                             "Carapidae",
                                             "Bathylagidae",
                                             "Bathydraconidae", 
                                             "Achiropsettidae"
                                  )))
  
  # define colour palette
  if (colour == "Family") {
    colour_palette <- c("Zoarcidae" = "#274637FF", 
                        "Stomiidae" = "#D8AF39FF", 
                        "Paralepididae" = "#5A6F80FF", 
                        "Nototheniidae" = "#4C413FFF", 
                        "Notosudidae" = "#44A57CFF", 
                        "Myctophidae" = "#278B9AFF",
                        "Muraenolepididae" = "#14191FFF", 
                        "Microstomatidae" = "#E75B64FF", 
                        "Melamphaidae" = "#B4DAE5FF", 
                        "Macrouridae" = "#DE7862FF", 
                        "Gempylidae" = "#1D2645FF",
                        "Channichthyidae" = "#58A449FF",
                        "Carapidae" = "#403369FF", 
                        "Bathylagidae" = "#E8C4A2FF",
                        "Bathydraconidae" = "#AE93BEFF", 
                        "Achiropsettidae" = "#F0D77BFF")
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      ggplot2::geom_text(data = dendro.labels, 
                         ggplot2::aes(x, y, 
                                      label = label, 
                                      colour = Family),
                         fontface = "italic",
                         hjust = 1, size = 4) +
      ggplot2::scale_color_manual(values = colour_palette) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-6, 6) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Family",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic() 
    
    # save plot 
    ggplot2::ggsave(paste0("output/05.HC/dendrogram_", 
                           file_name, "_",
                           colour, ".jpg"),
                    scale = 1,
                    height = 6, width = 8)
    
  } else if (colour == "Habitat") {
    colour_palette <- c("#3E6248FF", "#278B9AFF",
                        "#DE7862FF", "#D8AF39FF",
                        "#AE93BEFF")
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      ggplot2::geom_text(data = dendro.labels, 
                         ggplot2::aes(x, y, 
                                      label = label, 
                                      colour = Habitat),
                         hjust = 1, size = 4) +
      ggplot2::scale_color_manual(values = colour_palette) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-6, 6) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave(paste0("output/05.HC/dendrogram_", 
                           file_name, "_",
                           colour, ".jpg"),
                    scale = 1,
                    height = 6, width = 8)
    
  } else if (colour == "Cluster") {
    colour_palette <- c("#D8AF39FF",
                        "#58A449FF",
                        "#AE93BEFF",
                        "#B4DAE5FF",
                        "#E75B64FF",
                        "#1D2645FF")[1:max(clust_output$cluster)]
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      # ggplot2::geom_text(data = dendro.labels, 
      #                    ggplot2::aes(x, y, 
      #                                 label = label, 
      #                                 colour = Cluster),
      #                    fontface = "italic",
      #                    hjust = 1, size = 4) +
      ggtext::geom_richtext(data = dendro.labels, 
                            ggplot2::aes(x, y, 
                                         label = label, 
                                         colour = Cluster), 
                            hjust = 1, 
                            size = 3.9, 
                            fill = NA, label.color = NA # remove background and outline
                            ) +
      ggplot2::scale_color_manual(values = colour_palette) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-6.8, 6) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 13, 
                                                          face = "bold"),
                     legend.text = ggplot2::element_text(size = 13, 
                                                         face = "bold"), 
                     legend.key.height = ggplot2::unit(1, "cm"))
    
    # save plot 
    ggplot2::ggsave(paste0("output/05.HC/dendrogram_", 
                           file_name, "_",
                           colour, "_k", k, ".jpg"),
                    scale = 1,
                    height = 6, width = 5)
    
  } else if (colour == "diet") {
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      ggplot2::geom_text(data = dendro.labels, 
                         ggplot2::aes(x, y, 
                                      label = label, 
                                      colour = diet),
                         hjust = 1, size = 4) +
      ggplot2::scale_color_manual(values = c("#1D2645FF",
                                             "#278B9AFF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-5, 6) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.key.height = ggplot2::unit(1.5, "cm"))
    
    # save plot 
    ggplot2::ggsave(paste0("output/05.HC/dendrogram_", 
                           file_name, "_",
                           colour, ".jpg"),
                    scale = 1,
                    height = 5, width = 6)
    
  }
  
  
}




#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
boxplot_compo_clust <- function(clust_output,
                                compo_tib_means_sp, # used as input
                                file_name
) {
  
  # assign each sample to its cluster
  clust_vec <- clust_output$cluster
  colour_palette <- c("#D8AF39FF",
                      "#58A449FF",
                      "#AE93BEFF",
                      "#B4DAE5FF",
                      "#E75B64FF",
                      "#1D2645FF")[1:max(clust_output$cluster)]
  
  # tab with mean and median values for all samples to show on plots
  mean_median_tib <- compo_tib_means_sp |>
    tidyr::pivot_longer(cols = c("Ca":"Pb"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", 
                                               "As", "Se", "Ni", "Co", 
                                               "Sr", "Cd", "Pb", "Ag"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
  
  # both essential and non-essentials nutrients
  compo_tib_means_sp |> 
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec)) |>
    tidyr::pivot_longer(cols = c("Ca":"Pb"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co", 
                                               "Sr", "Cd", "Pb", "Ag"))) |>
    ggplot2::ggplot(ggplot2::aes(x = cluster, 
                                 y = concentration_mg_kg_dw, 
                                 fill = cluster)) +
    ggplot2::geom_violin(ggplot2::aes(color = cluster),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_fill_manual(values = colour_palette) +
    ggplot2::scale_color_manual(values = colour_palette) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", 
                        ncol = 4) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold", 
                                                        color = "black"), 
                   strip.background = ggplot2::element_rect(fill = "lightgrey"),
                   legend.position = "none")
  
  # save plot 
  ggplot2::ggsave(paste0("output/05.HC/clust_boxplot_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10)
  ggplot2::ggsave(paste0("output/05.HC/clust_boxplot_",
                         file_name,
                         ".svg"),
                  scale = 1,
                  height = 8, width = 10)
  
  
  # only essentials 
  compo_tib_means_sp |> 
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec)) |>
    tidyr::pivot_longer(cols = c("Ca":"Pb"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::filter(!(Nutrient %in% c("Sr", "Cd", "Pb", "Ag"))) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn",
                                               "As", "Se", "Ni", "Co")), 
                  y_lim = dplyr::case_when(Nutrient == "Ca" ~ 40000,
                                           Nutrient == "P" ~ 28000,
                                           Nutrient == "Na" ~ 26000,
                                           Nutrient == "K" ~ 19000,
                                           Nutrient == "Mg" ~ 4800,
                                           Nutrient == "Fe" ~ 240, 
                                           Nutrient == "Zn" ~ 120, 
                                           Nutrient == "Cu" ~ 14.5, 
                                           Nutrient == "Mn" ~ 13,
                                           Nutrient == "As" ~ 21, 
                                           Nutrient == "Se" ~ 4.5,
                                           Nutrient == "Ni" ~ 4.7, 
                                           Nutrient == "Co" ~ 0.24)) |>
    ggplot2::ggplot(ggplot2::aes(x = cluster, 
                                 y = concentration_mg_kg_dw, 
                                 fill = cluster)) +
    ggplot2::geom_violin(ggplot2::aes(color = cluster),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib |>
                          dplyr::filter(!(Nutrient %in% c("Sr", "Cd", "Pb", "Ag"))), 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib |>
                          dplyr::filter(!(Nutrient %in% c("Sr", "Cd", "Pb", "Ag"))), 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_fill_manual(values = colour_palette) +
    ggplot2::scale_color_manual(values = colour_palette) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", 
                        ncol = 4) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold", 
                                                        color = "white"), 
                   strip.background = ggplot2::element_rect(fill = "gray30"),
                   legend.position = "none")
  
  # save plot 
  ggplot2::ggsave(paste0("output/05.HC/clust_boxplot_",
                         file_name,
                         "_essentials_only.jpg"),
                  scale = 1,
                  height = 8, width = 10)
  
  # only non-essentials
  compo_tib_means_sp |> 
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec)) |>
    tidyr::pivot_longer(cols = c("Ca":"Pb"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::filter(Nutrient %in% c("Sr", "Cd", "Pb", "Ag")) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Sr", "Cd", "Pb", "Ag")), 
                  y_lim = dplyr::case_when(Nutrient == "Sr" ~ 250, 
                                           Nutrient == "Cd" ~ 2.1,
                                           Nutrient == "Pb" ~ 1.6, 
                                           Nutrient == "Ag" ~ 0.31)) |>
    ggplot2::ggplot(ggplot2::aes(x = cluster, 
                                 y = concentration_mg_kg_dw, 
                                 fill = cluster)) +
    ggplot2::geom_violin(ggplot2::aes(color = cluster),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib |>
                          dplyr::filter(Nutrient %in% c("Sr", "Cd", "Pb", "Ag")), 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib |>
                          dplyr::filter(Nutrient %in% c("Sr", "Cd", "Pb", "Ag")), 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::ylab("Nutrient concentration\n(in mg/kg dry weight)") +
    ggplot2::scale_fill_manual(values = colour_palette) +
    ggplot2::scale_color_manual(values = colour_palette) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", 
                        ncol = 4) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold", 
                                                        color = "black"), 
                   strip.background = ggplot2::element_rect(fill = "lightgrey"),
                   legend.position = "none")
  
  # save plot 
  ggplot2::ggsave(paste0("output/05.HC/clust_boxplot_",
                         file_name,
                         "_non-essentials_only_legend.jpg"),
                  scale = 1,
                  height = 2.5, width = 8)
  ggplot2::ggsave(paste0("output/05.HC/clust_boxplot_",
                         file_name,
                         "_non-essentials_only.jpg"),
                  scale = 1,
                  height = 2, width = 9)
  
}


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
stats_compo_clust <- function(clust_output,
                              compo_tib,
                              file_name
) {
  
  # assign each sample to its cluster
  clust_vec <- clust_output$cluster

  stats_tib <- rbind(compo_tib |> 
                       dplyr::ungroup() |>
                       dplyr::mutate(cluster = as.factor(clust_vec)) |>
                       tidyr::pivot_longer(cols = c("Ca":"Pb"), 
                                           names_to = "Nutrient", 
                                           values_to = "concentration_mg_kg_dw") |>
                       dplyr::mutate(Nutrient = factor(Nutrient, 
                                                       levels = c("Ca", "P", "Na", "K", "Mg", 
                                                                  "Fe", "Zn", "Cu", "Mn",
                                                                  "As", "Se", "Ni", "Co", 
                                                                  "Sr", "Cd", "Pb", "Ag"))) |>
                       dplyr::group_by(cluster, Nutrient) |>
                       dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 3),
                                        median = round(median(concentration_mg_kg_dw), 3),
                                        sd = round(sd(concentration_mg_kg_dw), 3)) |>
                       tidyr::pivot_longer(cols = c(mean, median, sd), 
                                           names_to = "statistic", 
                                           values_to = "value") |>
                       tidyr::pivot_wider(names_from = Nutrient, 
                                          values_from = value),
                     compo_tib |> 
                       dplyr::mutate(cluster = "all samples") |>
                       tidyr::pivot_longer(cols = c("Ca":"Pb"), 
                                           names_to = "Nutrient", 
                                           values_to = "concentration_mg_kg_dw") |>
                       dplyr::mutate(Nutrient = factor(Nutrient, 
                                                       levels = c("Ca", "P", "Na", "K", "Mg", 
                                                                  "Fe", "Zn", "Cu", "Mn",
                                                                  "As", "Se", "Ni", "Co", 
                                                                  "Sr", "Cd", "Pb", "Ag"))) |>
                       dplyr::group_by(cluster, Nutrient) |>
                       dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 3),
                                        median = round(median(concentration_mg_kg_dw), 3),
                                        sd = round(sd(concentration_mg_kg_dw), 3)) |>
                       tidyr::pivot_longer(cols = c(mean, median, sd), 
                                           names_to = "statistic", 
                                           values_to = "value") |>
                       tidyr::pivot_wider(names_from = Nutrient, 
                                          values_from = value)
  )
  
  # save 
  openxlsx::write.xlsx(stats_tib, 
                       file = paste0("output/05.HC/stats_compo_clust_", 
                                     file_name, ".xlsx"))
  
  stats_tib
}


#'
#'
#'
#'
# output matching species/samples with attributed cluster
clust_sp_attrib <- function(clust_output,
                          compo_tib,
                          file_name
) {
  
  # assign each sample to its cluster
  clust_vec <- clust_output$cluster

  clust_tib <- compo_tib |> 
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec)) 
  
  # save 
  openxlsx::write.xlsx(clust_tib, 
                       file = paste0("output/05.HC/clust_attribution_", 
                                     file_name, ".xlsx"))
}





#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k4 <- function(clust_output,
                            compo_tib,
                            file_name) {
  
  # assign each sample to its cluster
  clust_vec <- clust_output$cluster
  
  compo_tib <- compo_tib |>
    tidyr::pivot_longer(cols = c(Ca:Pb), 
                        names_to = "Nutrient", 
                        values_to = "mean_sp_conc_mg_kg_dw")
  
  nut_vec <- c("Ca", "P", "Na", "K", "Mg", 
               "Fe", "Zn", "Cu", "Mn",
               "As", "Se", "Ni", "Co",
               "Sr", "Cd", "Pb", "Ag")
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut)
    
    table$cluster <- factor(clust_output$cluster)
    
    table <- table |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = mean_sp_conc_mg_kg_dw) 
    
    clust1 <- na.omit(table$`1`)
    clust2 <- na.omit(table$`2`)
    clust3 <- na.omit(table$`3`)
    clust4 <- na.omit(table$`4`)
    
    nut_test <- data.frame(Nutrient = rep(nut, 6), 
                           Cluster_comp_1 = c("1", "1", "1",
                                              "2", "2", 
                                              "3"), 
                           Cluster_comp_2 = c("2", "3", "4",
                                              "3", "4", 
                                              "4"), 
                           alpha_MW = c(wilcox.test(clust1, clust2)[[3]],
                                        wilcox.test(clust1, clust3)[[3]],
                                        wilcox.test(clust1, clust4)[[3]],
                                        
                                        wilcox.test(clust2, clust3)[[3]],
                                        wilcox.test(clust2, clust4)[[3]],
                                        
                                        wilcox.test(clust3, clust4)[[3]]))
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
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
                       file = paste0("output/05.HC/MWW_test_clust_", 
                                     file_name, ".xlsx"))
  
  
  
  
}


#'
#'
#'
#'
#'
# function to make biplot with cluster grouping
biplot_after_clust <- function(res_pca, 
                               res_clust,
                               compo_tib,
                               file_name, # should be explicit regarding dataset (fish
                               # or scats), method (coda or nocoda), "cleanness" of 
                               # dataset (with ou without stat outliers), ellipse
                               # or not and PC chosen (if other than 1,2)
                               pcomp = c(1:2), # choices of the PC to plot (2 in total), 
                               # 1 and 2 by default
                               circle.prob = 0.69, # not sure yet why this value by default
                               var.add.scaling = 2, # constant to multiply coordinates
                               # of variables by so that they show on a similar scale as 
                               # that of observations # 2 seems to fit ok but could be changed 
                               ellipse.prob = 0.68 # size of the ellipse in Normal probability
                               # not sure yet why this value by default
) {
  
  # function constructed based on ggbiplot function on github 
  # https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
  # and was adapted to work for an pca CoDa object
  
  ###### biplot settings
  # common practice as explained in ?biplot() : 
  # variables are scaled by lambda^scale and observations are scaled by
  # lambda^(1-scale) where lambda are singular values as computed by PCA
  # i.e d below
  scale <- 0
  obs.scale <- 1 - scale
  var.scale <- scale
  
  ##### recover the single value decomposition SVD
  nobs.factor <- sqrt(nrow(res_pca$scores) - 1) # not sure what this is 
  # and what is it for
  
  # standard deviation of the PCs #lambda in ?biplot()
  d <- sqrt(res_pca$eigenvalues)
  
  u <- sweep(res_pca$scores, 2, 1 / (d * nobs.factor), FUN = '*')
  v <- res_pca$loadings
  
  
  #####
  # compute scores 
  # ie coordinates of individuals (observations) on each principal component (PC)
  # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
  df.u <- as.data.frame(sweep(u[,pcomp], 2, d[pcomp]^obs.scale, FUN='*'))
  # scale observations by lambda^(1-scale)
  
  # compute directions 
  # ie coordinates of the variables ie loadings * sdev of PCs
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, pcomp])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  df.u <- df.u * nobs.factor # so we are back to the original scores - res_pca$scores
  # ie the coordinates of the individuals on the PCs
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores (as done with ggbiplot)
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4) 
  
  # scale directions
  # v^2 = cos2 = quality of representation of variables on each PC 
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  # multiply then by another constant to get arrows on the same scale as observations 
  # coordinates
  # as mentioned in 
  # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
  # "it might be necessary to scale arrows by some arbitrary constant factor so 
  # that both arrows and data points appear roughly on the same scale "
  df.v <- var.add.scaling * df.v
  
  # scale scores 
  # as done by 
  # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
  # with r <- 1 
  # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
  # df.u=.99*df.u/r.scale
  # this version was set aside as we are more interested in comparing individuals 
  # and not structuring variables, so we went for an additional scaling 
  # of variables coordinates instead - see above
  
  # Append the proportion of explained variance to the axis labels
  if (res_pca$method == "robust") {
    u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')  
  } else if (res_pca$method == "classical") {
    u.axis.labs <- paste('PC', pcomp, ' (clr - classical)', sep='') 
    
  }
  # add explained variance
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * res_pca$eigenvalues[pcomp]/sum(res_pca$eigenvalues)))
  
  # Score Labels (labels of the observations)
  df.u$labels <- compo_tib$Code_sample
  
  # define groups
  # grouping per cluster
  df.u$groups <- as.factor(res_clust$cluster)
  
  
  # Variable Names
  df.v$varname <- rownames(v)
  
  # Variables for text label placement
  varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
  var.axes <- TRUE # draw arrow for the variable
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # draw circle 
  # theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  # circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
  
  ############## draw biplot
  g <- 
    
    # Base plot
    ggplot2::ggplot(data = df.u, ggplot2::aes(x = xvar, y = yvar)) + 
    ggplot2::xlab(u.axis.labs[1]) + 
    ggplot2::ylab(u.axis.labs[2]) + 
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    # # draw circle 
    # ggplot2::geom_path(data = circle, color = 'black', 
    #           size = 1/2, alpha = 1/3) +
    # Draw directions
    ggplot2::geom_segment(data = df.v,
                          ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                        'picas')), 
                          color = 'darkred') +
    # Draw either labels or points
    ggplot2::geom_point(ggplot2::aes(color = groups), 
                        size = 1.5,
                        alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
    ) + 
    # Label the variable axes
    ggplot2::geom_text(data = df.v, 
                       ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                    angle = angle, hjust = hjust), 
                       color = 'darkred', size = 5) +
    viridis::scale_color_viridis(option = "cividis", 
                                 discrete = TRUE, 
                                 name = "Cluster") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   legend.text = ggplot2::element_text(size = 15))
  
  # Overlay a concentration ellipse of clusters
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  
  ell <- plyr::ddply(df.u, 'groups', function(x) {
    if(nrow(x) <= 2) {
      return(NULL)
    }
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
               groups = x$groups[1])
  })
  names(ell)[1:2] <- c('xvar', 'yvar')
  
  g <- g + ggplot2::geom_path(data = ell, ggplot2::aes(color = groups, group = groups))
  
  g

  # save plot 
  ggplot2::ggsave(paste0("output/05.HC/clust_biplot_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10)
}


