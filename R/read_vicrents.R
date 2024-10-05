

# packages --------
library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(fpp3)


# url and file location ----------

lga_rents_url <- "https://www.dffh.vic.gov.au/quarterly-median-rents-local-government-area-june-quarter-2024"
  
# "https://www.dffh.vic.gov.au/quarterly-median-rent-local-government-area-march-quarter-2024-excel"


rents_loc <- tempfile(fileext = ".xlsx", pattern = paste0("LGA_quarterly_median_rents_","2024 Q2_"), tmpdir = "C:\\Users\\MAyala\\Desktop\\shiny\\vic_rental\\data-raw")


# if data is downloaded 
rents_loc <- "data-raw/LGA_quarterly_median_rents_2024 Q1_48f4316c79bf.xlsx"

file <- "data-raw/LGA_quarterly_median_rents_2024 Q2_678c187bec5.xlsx"
sheet <- excel_sheets(rents_loc)[7]



# download and clean raw data ------

download.file(url = lga_rents_url,
              destfile = rents_loc,
              mode = "wb")



# function to read and clean rents raw data ----

read_sheet <- function(file, sheet, area='LGA') {
  
  raw_df <- suppressMessages(read_excel(file,
                                        sheet = sheet,
                                        skip = 1,
                                        col_names = FALSE)
                             )
  
  dates <- raw_df[1, 3:ncol(raw_df)] |>
    pivot_longer(cols = everything(),
                 names_to = "col",
                 values_to = "date") |>
    fill(date) %>% 
    dplyr::mutate(
      date = case_when(
        #fix error in excel file (duplicate of quarter date)
        date == "Dec 2003" & row_number() < 31 ~ "Dec 2002",
        date == "Mar 2017" & row_number() > 144 ~ "Jun 2017",
        TRUE ~ date)
      )
  
  
  col_names <- raw_df |>
    filter(row_number() == 2) |>
    pivot_longer(everything(),
                 names_to = "col",
                 values_to = "series")
  
  
  if(area=='LGA'){
    tidy_df <- raw_df |>
      filter(row_number() > 2) |>
      fill(1) |>
      pivot_longer(cols = !c(1, 2),
                   names_to = "col")  |>
      left_join(col_names, by = "col") |>
      left_join(dates, by = "col") |>
      dplyr::select(-col) |>
      dplyr::rename(region = 1,
             lga = 2) |>
      dplyr::mutate(
        value = suppressWarnings(as.numeric(value)),
        date = lubridate::dmy(paste0("01 ", date))
        )
  }else{
    
    tidy_df <- raw_df |>
      filter(row_number() > 2) |>
      fill(1) |>
      pivot_longer(cols = !c(1, 2),
                   names_to = "col")  |>
      left_join(col_names, by = "col") |>
      left_join(dates, by = "col") |>
      select(-col) |>
      dplyr::rename(region = 1,
             suburb = 2) |>
      dplyr::mutate(
        value = suppressWarnings(as.numeric(value)),
        date = lubridate::dmy(paste0("01 ", date))
        )
  }
  
  tidy_df$dwelling_type <- sheet
  
  tidy_df <- tidy_df %>% 
    dplyr::mutate(
      
      region = case_when(
        region == "METRO NON-METRO" & lga == "Metro" ~ "Metro",
        region == "METRO NON-METRO" & lga == "Non-Metro" ~ "Non-Metro",
        region == "METRO NON-METRO" & lga == "Victoria" ~ "All",
        TRUE ~ region
        ),
      lga = case_when(
        lga == "Metro" ~ "Greater Melbourne",
                                  lga == "Non-Metro" ~ "Regional Victoria",
                                  TRUE ~ lga)) %>% 
    filter(!region %in% c("Table Total","METRO NON-METRO"))
  
  tidy_df
}


# extract tab names
sheets <- excel_sheets(rents_loc)


#compiles all rents data
vic_rents <- map_dfr(.x = sheets, area = "LGA",
                     .f = read_sheet,
                     file = rents_loc)


#convert to tsibble
vic_rents_ts <- vic_rents |>  
  as_tsibble(
    key = c(lga, dwelling_type, series, region),
    index = date) %>% 
  mutate(date = as.Date(date))


# Note: if tsibble gives an error for duplicated dates, check what dates are duplicated
#check for duplicates
x <- vic_rents %>% duplicates(key = c(lga,dwelling_type, series, region),index = date)

#export clean data
write_csv(vic_rents_ts,"data/Median Weekly Rents_202406.csv")



# check data ------

# a <- vic_rents %>% 
#   filter(
#     series == "Median",
#     dwelling_type == "All Properties",
#     !lga %in% c("Group Total", "Greater Melbourne", "Regional Victoria", "Victoria")
#   )


