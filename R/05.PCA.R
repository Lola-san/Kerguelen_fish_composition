################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr/lola.gilbert@lilo.org
#
# June 2024
# 05.PCA.R
#
# Script with functions to compute PCA on fish compositional data 
################################################################################

#'
#'
#'
#'
#'
# function to perform Principal Component Analysis using robust method 
# for composition data with package robCompositions
pca_coda_essentials <- function(compo_tib_means_sp) {
  
  compo_tib_essentials <- compo_tib_means_sp |>
    dplyr::select(-c(Ag, Cd, Pb, Sr)) |>
    dplyr::select(c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn))
  
  data.act <- as.data.frame(compo_tib_essentials[, 3:15]) # [, 6:18]
  
  ## robust estimation (default):
  res.rob. <- robCompositions::pcaCoDa(data.act)
  res.rob.
}


#'
#'
#'
#'
#'
# function to perform Principal Component Analysis using robust method 
# for composition data with package robCompositions
pca_coda_essentials_and_not <- function(compo_tib_means_sp) {
  
  # pca is computed on species av. as not same amount of samples per species
  data.act <- as.data.frame(compo_tib_means_sp[, 6:22])
  
  ## robust estimation (default):
  res.rob. <- robCompositions::pcaCoDa(data.act)
  res.rob.
}

#'
#'
#'
#'
#'
# function to create biplot for PCA coda (robust or non-robust) output 
biplot_pca_coda <- function(res_pca, 
                            compo_tib_means_sp,
                            file_name, # should be explicit
                            pcomp = c(1:2), # choices of the PC to plot (2 in total), 
                            # 1 and 2 by default
                            groups, # either "family" or "habitat"
                            circle = FALSE, # weither to draw correlation circle or not 
                            circle.prob = 0.69, # not sure yet why this value by default
                            var.add.scaling = 2, # constant to multiply coordinates
                            # of variables by so that they show on a similar scale as 
                            # that of observations # 2 seems to fit ok but could be changed 
                            ellipse = FALSE, # logical weither to draw ellipse around groups
                            # of points or not 
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
  df.u$labels <- compo_tib_means_sp$Species
  
  
  # define groups (and output folder)
  if (groups == "Family") {
    # grouping per Family (fish composition)
    df.u$groups <- compo_tib_means_sp$Family
    color_scale <- c("Zoarcidae" = "#274637FF", 
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
  } else if (groups == "habitat") {
    # grouping per habitat (fish composition)
    df.u$groups <- compo_tib$habitat
    color_scale <- c("#3E6248FF", "#278B9AFF",
                     "#DE7862FF", "#D8AF39FF", 
                     "#44A57CFF", "#14191FFF")
  } 
  
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
    ggplot2::scale_color_manual(values = color_scale) +
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
  
  if (ellipse == TRUE) { # Overlay a concentration ellipse if there are groups
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
    
  } else {
    g
  }
  
  # save plot 
  ggplot2::ggsave(paste0("output/04.PCA/PCA_biplot_",
                         file_name, 
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10
  )
  
  # # compute coord, cos2 and contrib for variables and individuals 
  # 
  # 
  # #### variables 
  # # coordinates
  # # helper function 
  # var_coord_func <- function(loadings, comp.sdev){
  #   loadings*comp.sdev
  # }
  # 
  # loadings <- res_pca$loadings
  # sdev <- sqrt(res_pca$eigenvalues)
  # var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
  # head(var.coord[, 1:4])
  # 
  # # contrib
  # contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
  
  
}


#'
#'
#'
#'
#'
# function to create histogram showing scores of variables on PCs 
###### CAREFUL
# In this function, some data is entered by hand so it should be used
# with these outputs only : PCA_fish_clean_scats_nopup_coda_rob, 
# PCA_fish_clean_coda_rob and PCA_scats_nopup_coda_rob
pca_coda_loadings_nut <- function(res_pca, # coda method
                                  file_name # should be explicit regarding dataset 
) {
  
  load_tib_large <- tibble::as_tibble(res_pca$loadings) |>
    # add nutrients as they were rownames and no variable in pca output
    dplyr::mutate(Nutrient = c("As", "Ca", "Co", "Cu", "Fe", "K", "Mg", 
                               "Mn", "Na", "Ni", "P", "Se", "Zn")) |>
    dplyr::rename("PC1" = `Comp.1`,
                  "PC2" = `Comp.2`,
                  "PC3" = `Comp.3`,
                  "PC4" = `Comp.4`,
                  "PC5" = `Comp.5`,
                  "PC6" = `Comp.6`,
                  "PC7" = `Comp.7`,
                  "PC8" = `Comp.8`,
                  "PC9" = `Comp.9`,
                  "PC10" = `Comp.10`,
                  "PC11" = `Comp.11`,
                  "PC12" = `Comp.12`)
  
  # save table as is 
  openxlsx::write.xlsx(load_tib_large, 
                       file = paste0("output/PCA/Nut_loadings_PCs_", 
                                     file_name, 
                                     ".xlsx"))
  
  # plots with the 6th first PCs
  load_tib_long <- load_tib_large|>
    tidyr::pivot_longer(cols = c(PC1:PC12), 
                        names_to = "PC", 
                        values_to = "score") |>
    dplyr::mutate(abs_score = abs(score))
  
  
  
  # the latter are not accessible automatically from summary(res_pca)
  # (or I failed to find the way)
  # so these are entered by hand here... and this function should thus be used
  # with these outputs !!!
  if (type == "both") {
    comp_prop_var <- c(0.7413316, 0.08776791, 0.06398254, 
                       0.03519628, 0.02495698, 0.01146791)
    # take the first 4 into account for means (90% of var)
    comp_for_means <- c("PC1", "PC2", "PC3", "PC4")
    
    # plot settings
    hght <- 8
    wdth <- 10
    
  } else if (type == "scats") {
    comp_prop_var <- c(0.8461865, 0.06193904, 0.03429155, 
                       0.01849665, 0.01608361, 0.009448452)
    # take the first 2 into account for means (90% var)
    comp_for_means <- c("PC1", "PC2")
    
    # plot settings
    hght <- 6
    wdth <- 9
    
  } else if (type == "fish") {
    comp_prop_var <- c(0.5042501, 0.2335199, 0.09193904, 
                       0.05291126, 0.04654996, 0.03140671)
    # take the first 5 into account for means 
    comp_for_means <- c("PC1", "PC2", "PC3", "PC4", "PC5")
    
    # plot settings
    hght <- 8
    wdth <- 14
  }
  
  # with details per PCs, with proportion of variance explained by each PC  
  load_tib_long |> 
    # order nutrients as they are on the first PCs
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("K", "As", "Na", "Fe", "Mn", 
                                               "Ca", "Cu", "Zn", "P", "Co", 
                                               "Mg", "Ni", "Se"))) |>
    dplyr::filter(PC %in% comp_for_means) |>
    dplyr::mutate(PC_prop_var = dplyr::case_when(PC == "PC1" ~ paste0(PC,
                                                                      " (",
                                                                      100*round(comp_prop_var[1], 
                                                                                3), 
                                                                      "%)"),
                                                 PC == "PC2" ~ paste0(PC,
                                                                      " (",
                                                                      100*round(comp_prop_var[2], 
                                                                                3), 
                                                                      "%)"),
                                                 PC == "PC3" ~ paste0(PC,
                                                                      " (",
                                                                      100*round(comp_prop_var[3], 
                                                                                3), 
                                                                      "%)"), 
                                                 PC == "PC4" ~ paste0(PC,
                                                                      " (",
                                                                      100*round(comp_prop_var[4], 
                                                                                3), 
                                                                      "%)"), 
                                                 PC == "PC5" ~ paste0(PC,
                                                                      " (",
                                                                      100*round(comp_prop_var[5], 
                                                                                3), 
                                                                      "%)"))) |>
    ggplot2::ggplot() + 
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = abs_score, 
                                   fill = Nutrient), 
                      stat = "identity") +
    ggplot2::facet_wrap(~ PC_prop_var) +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Absolute loading on Principal Component") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text = ggplot2::element_text(size = 15, 
                                                      face = "bold"),
                   legend.position = "none")
  # save plot 
  ggplot2::ggsave(paste0("output/PCA/", 
                         folder, 
                         "/Nut_loadings_PCs16_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = hght, width = wdth
  )
  
  
}
