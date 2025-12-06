
library(Haver)
library(tidyverse)
library(writexl)

haver.path(set = "C:/Users/ayala/Haver")

haver.direct(1)

hvr_codes <- c("VIMEHNV", "VI28NHNV", "VIEMNHNV", "VIIENHNV", "VI21NHNV", "VINMNHNV",
               "VIWTNHNV", "VIMZNHNV", "VISMNHNV", "VIMWNHNV")

clean_names <- c("Melbourne", "Bayside", "Eastern Melbourne", "Inner Eastern Melbourne", "Melbourne City",
                 "Melbourne North", "North West Melbourne", "South East Melbourne",
                 "South West Melbourne", "Western Melbourne")

code_names <- tibble(hvr_codes, clean_names) %>% 
  mutate(hvr_codes = tolower(hvr_codes))


hvr_sqm <- haver.data(codes = hvr_codes, database = "ANZR", rtype = 'data.frame') %>% 
  as_tibble(rownames = "date") %>% 
  dplyr::mutate(date = as_date(paste0(date,"-01")))

hvr_sqm_long <- hvr_sqm %>% 
  pivot_longer(2:11, names_to = "hvr_codes", values_to = "vacancy_rate") %>% 
  left_join(code_names)

# replace codes for names
names(hvr_sqm) <- c("dates", clean_names)

# export data
write_xlsx(hvr_sqm, "data-raw/SQM/vacany rates vic.xlsx")





