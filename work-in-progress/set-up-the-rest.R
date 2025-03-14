################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# set-up-the-rest.R
################################################################################

# with not necessarily functions in this script

######### For relative composition of fish plot, 0.2.description_compo.R
res_fish_tib <- targets::tar_read(table_fish_sp_conc_results_loq_replaced)

# to order samples so that it is prettier on plots, find order or conc with 
# the nutrient with the highest relative concentration, i.e. Ca
order_Ca <- res_fish_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                  Ag + Pb) |>
  tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                               Fe, Zn, Sr, Cu, Mn, Se,
                               Ni, Cd, As, Co, 
                               Ag, Pb), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::filter(statistic == "mean") |>
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Mg", "Na", "K", 
                                             "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                             "Ni", "Cd", "As", "Co", 
                                             "Ag", "Pb"))) |> 
  dplyr::filter(Nutrient == "Ca") |>
  dplyr::arrange(desc(conc_relative))

order_Ca$Species


order_Fe <- res_fish_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Sr + Cu + Mn + Se + Ni + Cd + As + Co +
                  Ag + Pb) |>
  tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                               Fe, Zn, Sr, Cu, Mn, Se,
                               Ni, Cd, As, Co, 
                               Ag, Pb), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::filter(statistic == "mean") |>
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Mg", "Na", "K", 
                                             "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                             "Ni", "Cd", "As", "Co", 
                                             "Ag", "Pb"))) |> 
  dplyr::filter(Nutrient == "Fe") |>
  dplyr::arrange(desc(conc_relative))

order_Fe$Species


## correlation plot, function corr_compo_fish in 03.description_compo
res_fish_tib <- targets::tar_read(tibble_mean_compo_per_sp)

corr_mat <- robCompositions::corCoDa(
  as.data.frame(res_fish_tib |>
                  dplyr::ungroup()|>
                  dplyr::select(c(Ca, P, Na, K, Mg, 
                                  Fe, Zn, Cu, Mn,
                                  As, Ni, Se, Co,
                                  Sr, Cd, Pb, Ag 
                  )))) 

colnames(corr_mat) <- rownames(corr_mat) <- c("Ca", "P", "Na", "K", "Mg", 
                                              "Fe", "Zn", "Cu", "Mn",
                                              "As", "Se", "Ni", "Co",
                                              "Sr", "Cd", "Pb", "Ag")

get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

melted_cormat1 <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                   na.rm = TRUE)) 

melted_cormat2 <- melted_cormat1 |>
  dplyr::filter(value != 1) #exclude the Ca/Ca etc 

table(melted_cormat2$value < -0.4)
12/(124+12)
table(melted_cormat2$value > 0.4)
25/(111+25)
table(melted_cormat2$value > -0.4 & melted_cormat2$value < 0.4)
99/(99+37)

16+15+14+13+12+11+10+9+8+7+6+5+4+3+2+1
