
# packages --------
library(readxl)
library(tidyverse)
library(leaflet)
library(sf)
library(strayr)


# read Victoria boundaries ----

lgas_2022 <- strayr::read_absmap("lga2022")

vic_lgas_2022 <- lgas_2022 %>% filter(state_name_2021=="Victoria")

# read rents data
vic_rents <- read_csv("data/Median Weekly Rents_202503.csv")



# clean data ----

# spatial data
vic_lgas_2022 <- vic_lgas_2022 %>% 
  mutate(
    lga_abs = case_when(
      str_detect(lga_name_2022, "\\(|\\)") ~ str_remove(lga_name_2022, "\\s*\\([^()]*\\)"),
      TRUE ~ lga_name_2022
  )
  ) %>% 
  filter(!lga_name_2022 %in% c("Unincorporated Vic", 
                               "No usual address (Vic.)", 
                               "Migratory - Offshore - Shipping (Vic.)")
  )

# rents data
vic_rents_latest <- vic_rents %>%
  filter(!lga %in% c("Group Total", "Victoria")) %>% 
  filter(
    date >= "2018-03-01", 
    series=="Median",
    !is.na(lga)) %>% 
  mutate(
    lga_abs = case_when(
      lga=="Mornington Penin'a" ~ "Mornington Peninsula",
      lga=="Merri-bek" ~ "Moreland",
      lga=="Colac-Otway" ~ "Colac Otway",
      TRUE ~ lga
    )
  ) %>% 
  select(date, lga, lga_abs, series, value, dwelling_type)


# export geopackage ----

lgas <- vic_lgas_2022 %>% 
  left_join(vic_rents_latest)

write_sf(lgas, "data/vic_rents.gpkg")

