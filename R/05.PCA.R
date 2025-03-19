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
  ggplot2::ggsave(paste0("output/03.PCA/PCA_biplot_",
                         file_name, 
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10
  )
  
}
