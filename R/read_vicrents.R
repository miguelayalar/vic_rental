

# packages --------
library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(tsibble)

# url and file location ----------

# function to download raw data from website

download_dffh <- function(filename){

  # base url
  base <- "https://www.dffh.vic.gov.au/"
  
  url <- paste0(base, filename)
  year_file <- str_extract(filename, "\\d+\\.?\\d*")
  qrt_file <- str_extract(tolower(filename), "march|june|september|december")

  qrt_file <- case_when(
    qrt_file=="march" ~ " Q1",
    qrt_file=="june" ~ " Q2",
    qrt_file=="september" ~ " Q3",
    qrt_file=="december" ~ " Q4"
    )
  
    
  # create a temp file
  rents_loc <- tempfile(
    fileext = ".xlsx", 
    pattern = paste0("LGA_quarterly_median_rents_",year_file, qrt_file,"_"), 
    tmpdir = "C:\\Users\\MAyala\\Desktop\\shiny\\vic_rental\\data-raw"
  )
  
  curl::curl_download(url = url,
                      destfile = rents_loc,
                      mode = "wb")
  
}

# filename
f <- "quarterly-median-rents-local-government-area-march-quarter-2025-excel"

# httr::GET(
#   url,
#   httr::write_disk(rents_loc, overwrite = TRUE),
#   httr::user_agent("Mozilla/5.0")
# )


download_dffh(filename = f)


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


# filename
file <- "data-raw/LGA_quarterly_median_rents_2025 Q1_11745236f6b.xlsx"

# extract tab names
sheets <- excel_sheets(file)


#compiles all rents data
vic_rents <- map_dfr(.x = sheets, area = "LGA",
                     .f = read_sheet,
                     file = file)


#convert to tsibble
vic_rents_ts <- vic_rents |>  
  as_tsibble(
    key = c(lga, dwelling_type, series, region),
    index = date) %>% 
  mutate(date = as.Date(date))


# Note: if tsibble gives an error for duplicated dates, check what dates are duplicated
#check for duplicates
vic_rents %>% duplicates(key = c(lga,dwelling_type, series, region),index = date)



#export clean data
write_csv(vic_rents_ts,"data/Median Weekly Rents_202503.csv")



