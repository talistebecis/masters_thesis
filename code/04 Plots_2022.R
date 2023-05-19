# Packages ----------------------------------------------------------------
rm(list=ls())

library(getspanel)
pacman::p_load(dplyr, data.table, ggplot2, ggpubr, here, tidyverse)

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


################################################################################
# COUNTERFACTUAL PLOTS
################################################################################

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


################################################################################
# MODEL FIT
################################################################################

# Road Transportation (1.A.3.b_noRES) ----------------------------------------

data <- data_full %>%
  filter(category == "1.A.3.b_noRES") %>%
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
road <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.05
)

road_plot <- plot_modelfit(road, title = "Road Transportation")


# Electricity and heat production (1.A.1.a) ----------------------------------------

data <- data_full %>%
  filter(category == "1.A.1.a") %>%
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
electricity <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.05
)

electricity_plot <- plot_modelfit(electricity, title = "Electricity and heat production")


# Residential and other sectors (1.A.4) ----------------------------------------

data <- data_full %>%
  filter(category == "1.A.4") %>%
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
residential <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.05
)

residential_plot <- plot_modelfit(residential, title = "Residential and other sectors")


# Manufacturing and Construction (1.A.2) ----------------------------------------

data <- data_full %>%
  filter(category == "1.A.2") %>%
  select(-category)

# add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#model (sample = EU15, p.value = 0.001)
manufacturing <- isatpanel(
  data = data,
  formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.05
)

manufacturing_plot <- plot_modelfit(manufacturing, title = "Manufacturing and Construction")

# Complete plot -----------------------------------------------------------

ggarrange(road_plot, electricity_plot, residential_plot, manufacturing_plot,
          ncol = 2, nrow = 2)


################################################################################
# BREAKDOWN OF EMISSIONS
################################################################################


# Plot --------------------------------------------------------------------

emissions_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description) %>% 
  mutate(IPCC_description = str_replace(IPCC_description, "Main Activity Electricity and Heat Production", "Electricity and Heat Production"),
         IPCC_description = str_replace(IPCC_description, "Petroleum Refining - Manufacture of Solid Fuels and Other Energy Industries", "Petroleum Refining"),
         IPCC_description = str_replace(IPCC_description, "Non-Energy Products from Fuels and Solvent Use", "Non-Energy Fuels"),
         IPCC_description = str_replace(IPCC_description, "Incineration and Open Burning of Waste", "Incineration of Waste"),
         IPCC_description = str_replace(IPCC_description, "Manufacturing Industries and Construction", "Manufacturing and Construction"),
         IPCC_description = str_replace(IPCC_description, "Road Transportation no resuspension", "Road Transportation"))

data <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>% 
  filter(country == "Austria",
         year == 2021) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100)

combined_waste_lime_water <- data %>% 
  filter(IPCC_description %in% c("Incineration of Waste", "Water-borne Navigation", "Lime production")) %>%
  select(percent) %>% 
  sum()

data %>% 
  mutate(IPCC_description=factor(IPCC_description, IPCC_description)) %>%
  ggplot(aes(x=IPCC_description, y=total_emissions)) +
  geom_segment(aes(x=IPCC_description ,xend=IPCC_description, y=0, yend=total_emissions), color="grey") +
  geom_point(size=3, color="gray") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab("IPCC emissions category") +
  ylab("Total emissions (kilotonnes carbon dioxide)")


# Aggregated levels - Level 1 -------------------------------------------------------

emissions_codes1 <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>%
  select(IPCC, Level.1)

data1 <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>% 
  filter(country == "Austria",
         year == 2021) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes1, by = c("category" = "IPCC")) %>% 
  group_by(Level.1) %>% 
  summarise(total_emissions = sum(total_emissions)) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100)

# Aggregated levels - Level 2 -------------------------------------------------------

emissions_codes2 <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>%
  select(IPCC, Level.2)

data2 <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>% 
  filter(country == "Austria",
         year == 2021) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes2, by = c("category" = "IPCC")) %>% 
  group_by(Level.2) %>% 
  summarise(total_emissions = sum(total_emissions)) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100)


################################################################################
# Time series
################################################################################

# Level 3 -----------------------------------------------------------------


emissions_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description) %>% 
  mutate(IPCC_description = str_replace(IPCC_description, "Main Activity Electricity and Heat Production", "Electricity & Heat Production"),
         IPCC_description = str_replace(IPCC_description, "Petroleum Refining - Manufacture of Solid Fuels and Other Energy Industries", "Petroleum Refining"),
         IPCC_description = str_replace(IPCC_description, "Non-Energy Products from Fuels and Solvent Use", "Non-Energy Fuels"),
         IPCC_description = str_replace(IPCC_description, "Incineration and Open Burning of Waste", "Incineration of Waste"),
         IPCC_description = str_replace(IPCC_description, "Manufacturing Industries and Construction", "Manufacturing & Construction"),
         IPCC_description = str_replace(IPCC_description, "Road Transportation no resuspension", "Road Transportation"),
         IPCC_description = str_replace(IPCC_description, "Other Process Uses of Carbonates", "Process Uses of Carbonates"),
         IPCC_description = str_replace(IPCC_description, "Residential and other sectors", "Residential & other sectors"))

data_full <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>%
  left_join(emissions_codes,
            by = c("category" = "IPCC")) %>%
  filter(!is.na(lemissions_pc),
         year >= 1995)

AUT_emissions_pc <- data_full %>%
  filter(country == "Austria") %>%
  group_by(year) %>%
  summarise(tot_emissions_pc = sum(emissions_pc))

ggplot(data = subset(data_full, country == "Austria")) +
  geom_line(mapping = aes(x = year, y = emissions_pc*1000000)) +
  facet_wrap(~ IPCC_description, scale = "free") +
  labs(x = "Year",
       y = "Emissions per capita (kg carbon dioxide)") +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 7))


# Level 2 -----------------------------------------------------------------

data_full1 <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022_level2.csv") %>% 
  left_join(emissions_codes,
            by = c("category_name" = "IPCC_description")) %>% 
  filter(!is.na(lemissions_pc))

ggplot(data = subset(data_full1, country == "Austria")) +
  geom_line(mapping = aes(x = year, y = emissions_pc*1000000)) +
  facet_wrap(~ category_name, scale = "free") +
  labs(x = "Year",
       y = "Emissions per capita (kg carbon dioxide)") +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 6))

# Level 1 -----------------------------------------------------------------

data_full1 <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022_level1.csv") %>% 
  left_join(emissions_codes,
            by = c("category_name" = "IPCC_description")) %>% 
  filter(!is.na(lemissions_pc))

ggplot(data = subset(data_full1, country == "Austria")) +
  geom_line(mapping = aes(x = year, y = emissions_pc*1000000)) +
  facet_wrap(~ category_name, scale = "free") +
  labs(x = "Year",
       y = "Emissions per capita (kg carbon dioxide)") +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 8))


################################################################################
# BREAKDOWN OF EMISSIONS
################################################################################


# Plot --------------------------------------------------------------------

emissions_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description) %>% 
  mutate(IPCC_description = str_replace(IPCC_description, "Main Activity Electricity and Heat Production", "Electricity and Heat Production"),
         IPCC_description = str_replace(IPCC_description, "Petroleum Refining - Manufacture of Solid Fuels and Other Energy Industries", "Petroleum Refining"),
         IPCC_description = str_replace(IPCC_description, "Non-Energy Products from Fuels and Solvent Use", "Non-Energy Fuels"),
         IPCC_description = str_replace(IPCC_description, "Incineration and Open Burning of Waste", "Incineration of Waste"),
         IPCC_description = str_replace(IPCC_description, "Manufacturing Industries and Construction", "Manufacturing and Construction"),
         IPCC_description = str_replace(IPCC_description, "Road Transportation no resuspension", "Road Transportation"))

data <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>% 
  filter(country == "Austria",
         year == 2005) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100)


waste_reduction <- data %>% 
  filter(IPCC_description == "Incineration of Waste") %>%
  select(total_emissions) %>% 
  as.numeric()*1.54
petrol_reduction <- data %>% 
  filter(IPCC_description == "Petroleum Refining") %>%
  select(total_emissions) %>% 
  as.numeric()*0.19
lime_reduction <- data %>% 
  filter(IPCC_description == "Lime production") %>%
  select(total_emissions) %>% 
  as.numeric()*0.82
water_reduction <- data %>% 
  filter(IPCC_description == "Water-borne Navigation") %>%
  select(total_emissions) %>% 
  as.numeric()*0.22

total_reduction <- waste_reduction + petrol_reduction + lime_reduction + water_reduction
total_reduction/sum(data$total_emissions)*100
