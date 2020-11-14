library(dplyr)
library(readr)
dta_raw = read_csv(here::here("outputs/raw_data.csv"), 
                   col_types = 
                     cols(
                       country = col_character(),
                       time = col_date(format = ""),
                       cases_sum = col_double(),
                       cases_diff = col_double(),
                       deaths_sum = col_double(),
                       deaths_diff = col_double(),
                       source = col_character()
                     )) 


DF_population_countries = read_csv("data/population_countries.csv",
                                   col_types = 
                                     cols(
                                       country = col_character(),
                                       population = col_double()
                                     )) %>% 
  mutate(country = 
           case_when(
             country == "Korea, South" ~ "Korea (South)",
             TRUE ~ country
           )) %>% 
  mutate(continent_name = 
           case_when(
             country %in% c("Mexico", "Dominican Republic", "Panama", "Honduras", "Cuba", "Costa Rica", "Guatemala", "El Salvador") ~ "Latin America",
             country %in% c("Russia", "Kazakhstan", "Azerbaijan", "Armenia") ~ "Asia",
             continent_name == "South America" ~ "Latin America",
             TRUE ~ continent_name
           )) %>% 
  mutate(country = 
           case_when(
             # country == "USA" ~ "US",
             TRUE ~ country
           )) %>% 
  mutate(continent_name = as.factor(continent_name))


# Menu vars ---------------------------------------------------------------

 DF_menu <- dta_raw %>%
  filter(!country %in% c("Total:", "Diamond Princess")) %>% 
  arrange(desc(cases_sum)) %>% 
  distinct(country) %>% 
  left_join(DF_population_countries %>% select(-population), by = "country") %>% 
  ungroup() %>% 
  arrange(continent_name, country) %>% 
  filter(continent_name %in% c("Asia", "Europe", "North America", "Latin America"))

 V1_alternatives <<- DF_menu %>% 
  pull(country)
