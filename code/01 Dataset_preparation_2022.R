# Overall data creation TT ------------------------------------------------
rm(list=ls())

### data assembly

set.seed(123)

EU31 <- c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
          "Czechia", "Germany", "Denmark", "Spain", "Estonia", "Finland",
          "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
          "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden", 
          "Switzerland", "Iceland", "Norway")

#### Import and merge Data
gdp <- read.csv("Data/01 Source_data/WB_gdpconst_2022.csv", skip = 3) %>%
  filter(Country.Name %in% EU31) %>%
  select(c(1,14:66)) %>%
  pivot_longer(2:54, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            gdp=value)

pop <- read.csv("Data/01 Source_data/WB_totpop_2022.csv", skip=3) %>%
  filter(Country.Name %in% EU31) %>%
  select(c(1,13:66)) %>%
  pivot_longer(5:55, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            pop=value)

emissions_total <- read_excel("Data/01 Source_data/IEA_EDGAR_CO2_1970-2021.xlsx",
                              range = "IPCC 2006!A11:BH4784") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",Name))) %>%
  filter(Name %in% EU31,
         fossil_bio == "fossil") %>% 
  pivot_longer(9:60, "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report)

data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(emissions_total, c("country", "year"))


#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 Intermediary_data/CO2DriversEU_dataset_2022.csv"), row.names = F)


# Level 1 sectors ------------------------------------------------------

category_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv")

emissions_level1 <- emissions_total %>% 
  left_join(category_codes, c("category" = "IPCC")) %>% 
  group_by(country, year, Level.1) %>% 
  summarise(emissions = sum(emissions)) %>% 
  rename(category_name = Level.1)

data_level1 <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(emissions_level1, c("country", "year"))

#### Transform Variables
data_level1$lgdp <- log(data_level1$gdp)
data_level1$lpop <- log(data_level1$pop)
data_level1$lemissions <- log(data_level1$emissions)
data_level1$const <- 1
data_level1$lgdp_sq <- data_level1$lgdp^2
data_level1$emissions_pc <- data_level1$emissions/data_level1$pop
data_level1$lemissions_pc <- log(data_level1$emissions_pc)

#### Output
write.csv(data_level1, here("Data/02 Intermediary_data/CO2DriversEU_dataset_2022_level1.csv"), row.names = F)


# Level 2 sectors ---------------------------------------------------------

category_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv")

emissions_level2 <- emissions_total %>% 
  left_join(category_codes, c("category" = "IPCC")) %>% 
  group_by(country, year, Level.2) %>% 
  summarise(emissions = sum(emissions)) %>% 
  rename(category_name = Level.2)

data_level2 <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(emissions_level2, c("country", "year"))

#### Transform Variables
data_level2$lgdp <- log(data_level2$gdp)
data_level2$lpop <- log(data_level2$pop)
data_level2$lemissions <- log(data_level2$emissions)
data_level2$const <- 1
data_level2$lgdp_sq <- data_level2$lgdp^2
data_level2$emissions_pc <- data_level2$emissions/data_level2$pop
data_level2$lemissions_pc <- log(data_level2$emissions_pc)

#### Output
write.csv(data_level2, here("Data/02 Intermediary_data/CO2DriversEU_dataset_2022_level2.csv"), row.names = F)
