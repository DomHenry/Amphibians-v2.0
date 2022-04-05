## ________________________________________________________________________

## Title:   Spatiotemporal occ plots
## Purpose: Plots to check the spatial and temporal distribution of occ points
## Author:  Dominic Henry
## Date:    10/02/2022

## Libraries
library(sf)
library(tidyverse)
library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(glue)
library(SDMutils)
## ________________________________________________________________________

## Load occurrence data
sdm_dir <- "data output/sdm data processing"
load(glue("{sdm_dir}/{sppselect}/occ_data_clean.RData"))

# Investigate duplicates --------------------------------------------------

# Add latitude and longitude columns 
occ_data_sf <- occ_data_sf %>% 
  bind_cols(as_tibble(st_coordinates(occ_data_sf))) %>% 
  select(-geometry,geometry)

glimpse(occ_data_sf)

## Print duplicates when origin ID is removed
janitor::get_dupes(st_drop_geometry(occ_data_sf)%>% select(-origin_id))

## Proportion unique records
print(glue::glue(
  "{round(nrow(distinct(occ_data_sf %>% 
  select(-origin_id)))/nrow(occ_data %>% 
  select(-origin_id))*100,2)}% UNIQUE RECORDS"))

## Write duplicates to file (non-distinct values)
occ_data %>% 
  group_by_at(vars(-origin_id)) %>% 
  filter(n() > 1) %>%  # keep non-distinct values only (i.e. duplicates)
  ungroup() %>% 
  arrange(order,
          genus,
          scientific_name,
          decimal_latitude ,decimal_longitude) %>% 
  mutate_at(vars(core:buff3), as.character) %>% 
  write_csv(glue("{sdm_dir}/{sppselect}/duplicate_records_{sppselect}.csv"))

## Remove spatial duplicates from remainder of analysis (select distinct points) 
occ_data_sf <- occ_data_sf %>% 
  distinct(geometry, .keep_all = TRUE)

# Temporal plots ----------------------------------------------------------

## Add date variables
occ_data_sf <- occ_data_sf %>% 
  mutate(year = ifelse(year == "-9999", NA, year)) %>% 
  mutate(year_date = dmy(str_c("01","01",year, sep = "-"))) %>% 
  mutate(decade = as.factor(year(floor_date(year_date, years(10)))))

## Count undated records
undated <- occ_data_sf %>% 
  filter(is.na(year_date)) %>% 
  nrow()

## Set ggplot theme
ptheme <- theme(axis.text.x = element_text(angle = 90, size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size = 16),
                title = element_text(size = 18))

## Generate plots
SDMutils::plot_occ_temporal(occ_data_sf, ptheme)

# Spatial plots -----------------------------------------------------------

## Import spatial data
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/RSA_fixed.shp",crs = latlongCRS)
range <- st_read(glue("data input/expert_polygons/{sppselect}.shp"))

## Set theme
ptheme <- theme(legend.text = element_text(size = 22),
                legend.title = element_text(size = 24),
                axis.text = element_text(size = 18),
                plot.title = element_text(size = 24))

SDMutils::plot_occ_spatial(occ_data_sf, range, za, ptheme)
