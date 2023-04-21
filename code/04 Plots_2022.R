# Packages ----------------------------------------------------------------

pacman::p_load(getspanel)
pacman::p_load(dplyr)


# Setup -------------------------------------------------------

rm(list=ls())
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
is <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

plot_counterfactual(is)
break_uncertainty(is, m = 15, interval = 0.99)


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
is <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

plot_counterfactual(is)
break_uncertainty(is, m = 15, interval = 0.99)


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
is <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

plot_counterfactual(is)
break_uncertainty(is, m = 15, interval = 0.99)

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
is <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=.05
)

plot_counterfactual(is)
break_uncertainty(is, m = 15, interval = 0.99)

