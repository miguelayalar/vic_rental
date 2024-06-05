
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(readabs)


load_data <- function(dbs_name = 'Median Weekly Rents_202312', path = "data/") {
  if(is.null(path)){stop("you need to specify database path")}
  # file <- paste0(path, dbs_name, ".rds")
  # dbs <- readRDS(file)
  # 
  file <- paste0(path, dbs_name, ".csv")
  dbs <- read_csv(file, show_col_types = FALSE)
  
}

#x <- load_data(path = "data/")

load_data() %>% dplyr::distinct(lga)  %>%
  dplyr::pull(lga) %>%
  assign("lgas", ., envir = .GlobalEnv)

load_data() %>% dplyr::distinct(region)  %>%
  dplyr::pull(region) %>%
  assign("regions", ., envir = .GlobalEnv)


load_data_vancancy <- function(dbs_name = 'Vacancy rates Vic', path = "data-raw/SQM/") {
  if(is.null(path)){stop("you need to specify database path")}
  # file <- paste0(path, dbs_name, ".rds")
  # dbs <- readRDS(file)
  # 
  #Sys.setenv(TZ="Australia/Sydney")
  file <- paste0(path, dbs_name, ".csv")
  dbs <- read_csv(file, show_col_types = FALSE)
  
  dbs <- dbs %>% pivot_longer(cols = 2:11, names_to = "area", values_to = "value") %>% 
  dplyr::mutate(date = as.Date(paste0(substr(Dates,6,9),"-",substr(Dates,3,4),"-01"))) %>% select(-Dates) %>% drop_na()
  
}

load_data_vancancy() %>% dplyr::distinct(area)  %>%
  dplyr::pull(area) %>%
  assign("areas", ., envir = .GlobalEnv)

