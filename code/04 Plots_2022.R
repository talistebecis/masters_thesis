# Packages ----------------------------------------------------------------
rm(list=ls())

library(getspanel)
pacman::p_load(dplyr, data.table, ggplot2, ggpubr)

# set up functions
source(here("code", "functions", "identify_indicator_timings.R"))
source(here("code", "functions", "plot_counterfactual.R"))


# Setup -------------------------------------------------------
set.seed(1230)

#Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

#import data
data_full <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>% 
  filter(year >= 1995,
         country %in% EU15)

# Incineration of waste (4.c) ---------------------------------------------

data <- data_full %>% 
  filter(category == "4.C") %>% 
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
waste <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

waste_plot <- plot_counterfactualTT(waste,
                                    title = "Incineration of waste")


# Lime production (2.A.2) -------------------------------------------------

data <- data_full %>% 
  filter(category == "2.A.2") %>% 
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
lime <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

# lime_plot <- plotoutput # positive breaks removed manually


# Petroleum refining (1.A.1.bc) -------------------------------------------

data <- data_full %>% 
  filter(category == "1.A.1.bc") %>% 
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
petrol <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

petrol_plot <- plot_counterfactualTT(petrol,
                                     title = "Petroleum refining")


# Water-borne Navigation (1.A.3.d) ----------------------------------------

data <- data_full %>% 
  filter(category == "1.A.3.d") %>% 
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
water <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

water_plot <- plot_counterfactualTT(water,
                                    title = "Water-borne navigation")



# Complete plot -----------------------------------------------------------

ggarrange(waste_plot, lime_plot, petrol_plot, water_plot,
          ncol = 2, nrow = 2)


# # Road Transportation (1.A.3.b_noRES) ----------------------------------------
# 
# data <- data_full %>% 
#   filter(category == "1.A.3.b_noRES") %>% 
#   select(-category)
# 
# # add lags
# data <- as.data.table(data)
# data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
# data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
# data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
# data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]
# 
# #model (sample = EU15, p.value = 0.001)
# road <- isatpanel(
#   data = data,
#   formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
#   index = c("country", "year"),
#   effect = "twoways",
#   iis = T,
#   fesis = T, 
#   t.pval=.05
# )
# 
# plot_counterfactual(road)
# break_uncertainty(road)
