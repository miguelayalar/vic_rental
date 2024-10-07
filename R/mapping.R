
# packages --------
library(readxl)
library(tidyverse)
library(leaflet)
library(sf)
library(strayr)
library(mapview)


#read Victoria boundaries

lgas_2022 <- strayr::read_absmap("lga2022")

vic_lgas_2022 <- lgas_2022 %>% filter(state_name_2021=="Victoria")

# read rents data

vic_rents <- read_csv("data/Median Weekly Rents_202406.csv")



# clean data ----
vic_lgas_2022 <- vic_lgas_2022 %>% 
  mutate(lga_abs = case_when(
    str_detect(lga_name_2022, "\\(|\\)") ~ str_remove(lga_name_2022, "\\s*\\([^()]*\\)"),
    TRUE ~ lga_name_2022
  )) %>% 
  filter(!lga_name_2022 %in% c("Unincorporated Vic", "No usual address (Vic.)", "Migratory - Offshore - Shipping (Vic.)")
  )


vic_rents_latest <- vic_rents %>% 
  filter(!lga %in% c("Group Total", "Victoria")) %>% 
  filter(date=="2024-06-01" & dwelling_type=="All Properties" & series=="Median" & !is.na(lga)) %>% 
  mutate(
    lga_abs = case_when(
      lga=="Mornington Penin'a" ~ "Mornington Peninsula",
      lga=="Merri-bek" ~ "Moreland",
      lga=="Colac-Otway" ~ "Colac Otway",
      TRUE ~ lga
    )
  ) %>% 
  select(lga, lga_abs, series, value)


# export geopackage


# transform data -----
a <- vic_lgas_2022 %>% 
  left_join(vic_rents_latest)


s <- mapview(a, zcol = "value",
             alpha.regions = 0.4, aplha = 1)


s
mapview::mapview2leaflet(s)

