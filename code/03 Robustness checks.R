# Packages ----------------------------------------------------------------
rm(list=ls())

library(getspanel)
pacman::p_load(dplyr, data.table, ggplot2, ggpubr, here, tidyverse)
set.seed(1230)

#import data
data_full <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv") %>% 
  filter(year >= 1995)

#Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")
EU16 <- c("Croatia", "Bulgaria", "Cyprus", "Czechia", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland", 
          "Norway")
EU31 <- c(EU15, EU16)

#IPCC categories
remove_no <- c(4, 14, 24) #3 categories for which there are no emissions in AUT

emissions_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>% 
  select(IPCC) %>%
  slice(-remove_no)

categories <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>% 
  select(IPCC_description) %>%
  slice(-remove_no)


# Quick check -------------------------------------------------------------

# # Incineration of waste (4.c), EU15
# data <- data_full %>% 
#   filter(category == "4.C",
#          country %in% EU15) %>% 
#   select(-category)
# 
# #basic model (sample = EU15, p.value = 0.05)
# waste <- isatpanel(
#   data = data,
#   formula = lemissions_pc ~ lgdp + lgdp_sq + lpop,
#   index = c("country", "year"),
#   effect = "twoways",
#   iis = T,
#   fesis = T, 
#   t.pval=.05
# )
# 
# #no log model (sample = EU15, p.value = 0.05)
# waste_no_log <- isatpanel(
#   data = data,
#   formula = emissions_pc ~ gdp + gdp^2 + pop,
#   index = c("country", "year"),
#   effect = "twoways",
#   iis = T,
#   fesis = T, 
#   t.pval=.05
# )
# 
# #no per capita model (sample = EU15, p.value = 0.05)
# waste_no_pc <- isatpanel(
#   data = data,
#   formula = lemissions ~ lgdp + lgdp^2 + lpop,
#   index = c("country", "year"),
#   effect = "twoways",
#   iis = T,
#   fesis = T, 
#   t.pval=.05
# )
# 
# #all per capita model (sample = EU15, p.value = 0.05)
# waste_all_pc <- isatpanel(
#   data = data,
#   formula = lemissions_pc ~ log(gdp/pop) + log(gdp/pop)^2 + lpop,
#   index = c("country", "year"),
#   effect = "twoways",
#   iis = T,
#   fesis = T, 
#   t.pval=.05
# )


# # Analysis ----------------------------------------------------------------

#run loop over all variables to produce results for all IPCC categories
for(IPCC_code in 1:21){

  #set IPCC code and category
  emissions_code <- emissions_codes[IPCC_code,]
  emissions_category <- categories[IPCC_code,]

  #set up document
  file_name <- paste0("Data/03 Output_data/robust_no_pc/",emissions_category)

  #filter data
  data <- data_full %>%
    filter(category == emissions_code) %>%
    select(-category)

  #add lags
  data <- as.data.table(data)
  data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
  data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
  data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
  data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

  # Heterogenous effects preparation
  group.interactions <- c("lgdp_EU15", "lgdp_EU16", "lgdpsq_EU15", "lgdpsq_EU16",
                          "lpop_EU15", "lpop_EU16")
  data$lgdp_EU15 <- data$lgdp * (data$country %in% EU15)
  data$lgdp_EU16 <- data$lgdp * (data$country %in% EU16)
  data$lgdpsq_EU15 <- data$lgdp_sq * (data$country %in% EU15)
  data$lgdpsq_EU16 <- data$lgdp_sq * (data$country %in% EU16)
  data$lpop_EU15 <- data$lpop * (data$country %in% EU15)
  data$lpop_EU16 <- data$lpop * (data$country %in% EU16)

  #results document header
  cat(
    paste0(
      "#################################################################### \n",
      "#                                                                  # \n",
      "#                 CO2 DRIVERS EU - ANALYSIS                        # \n",
      "#                                                                  # \n",
      "#################################################################### \n",
      "\n \n \n"),
    file = file_name
  )

  # Analysis
  for(group in 1:2){

    # Prepare sample and data
    sample <- list(EU15, EU31)[[group]]
    dat <- filter(data, country %in% sample, year>=1995)

    # Print Sample Header
    cat(
      paste0(
        "############################## \n",
        "#  SAMPLE = EU", length(sample), " \n",
        "############################## \n",
        "\n \n "),
      file = file_name,
      append = T
    )

    for(p.value in c(.05, .01, .001)){

      # Break analysis:
      is <- isatpanel(
        data = dat,
        formula = ifelse(
          group == 1, "lemissions ~ lgdp + lgdp_sq + lpop",
          paste0(
            "lemissions ~ ",
            paste(group.interactions, collapse = " + ")
          )
        ) %>% as.formula,
        index = c("country", "year"),
        effect = "twoways",
        iis = T,
        fesis = T,
        t.pval=p.value
      )

      # Print analysis results
      cat(
        paste0(
          " \n ###########################",
          " \n # p-value: ", p.value,
          " \n \n "),
        file = file_name,
        append = T)

      sink(file_name, append=T)
      print(is)
      sink()

      cat(" \n \n \n \n \n",
          file = file_name,
          append = T)
    }
  }
}
