################################################################################
# Kerguelen_fish_composition project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2024
# set_up-R-project.R
################################################################################

# initiate renv
renv::init()

# create folder that will contain all R function scripts
dir.create("R")

# create data and output folders
dir.create("data")
dir.create("output")


# install packages 
renv::install("devtools")
renv::install("targets")
renv::install("openxlsx")
renv::install("tidyverse")
renv::install("purrr")
renv::install("ncdf4")
renv::install("tarchetypes")
renv::install("kableExtra")
renv::install("imager")
renv::install("ghibli")
renv::install("rmarkdown")
renv::install("here")
renv::install("kdensity")
renv::install("wesanderson")
renv::install("fpc")

renv::install("ggtext")

# checking for updates
renv::status()


