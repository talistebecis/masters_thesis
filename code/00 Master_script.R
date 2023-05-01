# clean memory
rm(list=ls())

# load libraries
pacman::p_load(data.table)
pacman::p_load(dplyr)
pacman::p_load(tidyr)
pacman::p_load(readxl)
pacman::p_load(stringr)
pacman::p_load(gets)
pacman::p_load(here)
pacman::p_load(ggplot2)
pacman::p_load(devtools)
devtools::install_github("moritzpschwarz/getspanel")
library(getspanel)

# call scripts
source(here("code", "functions", "identify_indicator_timings.R"))
source(here("code", "functions", "plot_counterfactual.R"))
