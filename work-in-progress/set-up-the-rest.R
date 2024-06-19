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
