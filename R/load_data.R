
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(readabs)
library(sf)


load_data <- function(file, path = "data/") {
  
  if(is.null(path)){stop("you need to specify database path")}
  
  if (stringr::str_detect(file, "gpkg")) {
    
    dbs <- read_sf(file)
  }else{
  
    dbs <- read_csv(file, show_col_types = FALSE)  
    
  }
  
  
}



data_list <- function(){
  
  dbs_files <- list.files(path = "data/", full.names = TRUE)
  
  dbs_names <- c(
    "rental" = dbs_files[1],
    "vacancy" = dbs_files[2],
    "map_rents" = dbs_files[3]
  )
  
  d <- map(dbs_names, .f = load_data)
  
  return(d)
  
}

datalist <- data_list()

saveRDS(datalist, "output/datalist.rds")

