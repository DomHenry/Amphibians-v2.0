## ________________________________________________________________________

## Title:    Extract and process occ data
## Purpose:  Run first step of modelling process to extract amphibian data
## Author:   Dominic Henry
## Date:     09/02/2022

## Libraries
library(glue)
library(lubridate)
library(sf)
library(viridis)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(tidyverse)
## ________________________________________________________________________
print(glue("PROCESSING: {sppselect}"))

# Create species directory ------------------------------------------------
if (dir.exists(glue("data output/sdm data processing/{sppselect}"))) {
  print("Folder exists")
} else {
  print("Folder created")
  dir.create(glue("data output/sdm data processing/{sppselect}"))
}

# Extract species data ----------------------------------------------------

## Write raw data to file
occ_data_all <- read_csv("data output/occ_filtered.csv") %>%
  filter(scientific_name  %in% sppselect) %>% 
  write_csv(glue("data output/sdm data processing/{sppselect}/occurrence_raw_{sppselect}.csv"))


# Filter data -------------------------------------------------------------

## Remove QDS data
occ_data <- occ_data_all %>% 
  filter(!qds %in% "1")

## Check
unique(occ_data$qds)

## Write filtered occurrence data
occ_data %>%
  write_csv(glue("data output/sdm data processing/{sppselect}/occurrence_clean_{sppselect}.csv"))

# Filter spatial records --------------------------------------------------

## Import RSA shapefile
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/RSA_fixed.shp", crs = latlongCRS)
za_buff_uni <- st_union(st_buffer(za, 0.01)) %>%
  st_make_valid()

## Remove clear outliers & convert occ_data to simple feature
occ_data_sf <- occ_data %>%
  filter(!(is.na(decimal_latitude) | is.na(decimal_longitude) |
             decimal_latitude == 0 | decimal_longitude == 0 |
             decimal_latitude > -15 | decimal_latitude < -35 |
             decimal_longitude < 15 | decimal_longitude > 35)) %>%
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = latlongCRS) %>%
  st_crop(st_bbox(za))

## Intersect with RSA feature
occ_data_sf <- occ_data_sf %>%
  st_intersection(za_buff_uni)

## Identify records with location errors
occ_data_locerr <- occ_data %>%
  filter(is.na(decimal_latitude) | is.na(decimal_longitude) |
           decimal_latitude == 0 | decimal_longitude == 0 |
           decimal_latitude > -21 | decimal_latitude < -35 |
           decimal_longitude < 15 | decimal_longitude > 35) %>%
  print(n = 10) %>%
  write_csv(glue("data output/sdm data processing/{sppselect}/spatial_errors_{sppselect}.csv"))


# Identify spatial outliers based on expert polygons ----------------------
expert_file <- glue("data input/expert_polygons/{sppselect}.shp")

expert_range <- st_read(expert_file) %>% 
  st_transform(crs = latlongCRS)%>%
  st_make_valid(.)

plot(expert_range$geometry)

source("helper functions/check_occ_expert.R")
expert_results <- check_occ_expert(expert_range, occ_data_sf)

expert_df <- expert_results %>%
  st_drop_geometry()

## NOTE Important to note that left_join will add thousands of rows if there are NAs in the join by column (from either data frame). Need to make sure that each occ record has a unique ID.
occ_data <- occ_data %>%
  left_join(expert_df %>% dplyr::select(origin_id, core:buff3), by = "origin_id")

occ_data_sf <- occ_data_sf %>%
  left_join(expert_df %>% dplyr::select(origin_id, core:buff3), by = "origin_id")

## Identify points outside all three buffers
buff_rm <- occ_data_sf %>%
  filter(core == FALSE & buff1 == FALSE & buff2 == FALSE & buff3 == FALSE)

# Write outlying points to file -------------------------------------------
buff_rm %>%
  st_write(glue("data output/sdm data processing/{sppselect}/points_outside_buffers_{sppselect}.shp"),
           delete_dsn = TRUE)

buff_rm %>%
  st_drop_geometry() %>%
  st_write(glue("data output/sdm data processing/{sppselect}/points_outside_buffers_{sppselect}.csv"),
           delete_dsn = TRUE)

# Remove outliers ---------------------------------------------------------
occ_data_sf <- occ_data_sf %>%
  filter(!origin_id %in% buff_rm$origin_id)

# Write to shapefile ------------------------------------------------------
st_write(occ_data_sf, glue("data output/sdm data processing/{sppselect}/occ_records_full_{sppselect}.shp"),
         delete_dsn = TRUE, delete_layer = TRUE)

# Write workspace --------------------------------------------------------
save(
  list = c("occ_data", "occ_data_sf", "sppselect"),
  file = glue("data output/sdm data processing/{sppselect}/occ_data_clean.RData")
)