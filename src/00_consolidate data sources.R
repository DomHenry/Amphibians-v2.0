## ________________________________________________________________________

## Title:   Consolidate data sources
## Purpose: Import all amphibian data sets, filter & combine
## Author:  Dominic Henry
## Date:    09/02/2022

## Libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(tidylog)
library(janitor)
## ________________________________________________________________________


## Process and combine data from various sources (NMB, PEM, iSpot, ADU, EWT)

# Read in SCC including NT species ----------------------------------------
spp <- read_xlsx("Amphibians_processing_summary-v2.xlsx", 
                 sheet = "Full species list")$`scientific name`

# Import primary amphibian database ---------------------------------------
amph <- read_csv("data input/occurence_data/SA_Amphibians_All_data_full.csv",
                 guess_max = 5000) %>% 
  clean_names() %>% 
  select(object_id,occurrence_id, institution_code, absent,order,family,genus,scientific_name,
         decimal_latitude,decimal_longitude,coordinate_precision,basis_of_record,year,month,day,
         coord_notes,qds,errors,error_notes,country,state_province,locality) %>% 
  distinct_at(vars(occurrence_id:locality), .keep_all = TRUE) %>%  # Unique when object_id is removed
  mutate(origin_id = str_c("AMPHMAST_",c(1:nrow(.)))) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-"))) %>% 
  mutate(occurrence_id = as.character(occurrence_id)) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), as.numeric)

glimpse(amph)

## Correct taxonomy
amph <- amph %>% 
  mutate(genus = ifelse(genus == "Amietophrynus","Sclerophrys",genus)) %>%
  mutate(scientific_name = ifelse(str_detect(scientific_name,"Amietophrynus"),
                                  str_replace(scientific_name,"Amietophrynus", "Sclerophrys"), 
                                  scientific_name)) %>% 
  mutate(scientific_name = ifelse(str_detect(scientific_name,"pantherinus"),
                                  str_replace(scientific_name,"pantherinus", "pantherina"), 
                                  scientific_name))

## Remove absence records and filter EST species
amph <- amph %>% 
  filter(absent == 0) %>% 
  filter(scientific_name %in% spp) 

## Check records
amph %>% 
  group_by(scientific_name) %>% 
  tally

# Import 2021 ADU data ----------------------------------------------------
adu <- read_csv("data input/occurence_data/FrogMAP_shareable_records_EWT_20210225.csv",
                guess_max = 5000) %>% 
  clean_names() %>% 
  mutate(scientific_name = str_c(str_to_sentence(genus), " ", 
                                 str_to_lower(species))) %>%  
  select(catalog_number, institution_code, order,family,genus,scientific_name,
         decimal_latitude,decimal_longitude,basis_of_record,year_collected,
         month_collected, day_collected,
         country,state_province,locality) %>% 
  mutate(origin_id = str_c("ADU2021_",c(1:nrow(.)))) %>% 
  rename(day = day_collected, month = month_collected, year = year_collected) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-"))) %>% 
  mutate(occurrence_id = as.character(catalog_number)) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), as.numeric) %>% 
  mutate(qds = 0)

glimpse(adu)

adu <- adu %>% 
  filter(scientific_name %in% spp) 

adu %>% 
  group_by(scientific_name) %>% 
  tally

names(adu)[which(names(adu) %in% names(amph))]

# Import EWT consolidated data from ---------------------------------------
ewt <- read_xlsx("data input/occurence_data/Amphibian_New_Mastersheet_OC.xlsx") %>% 
  clean_names() %>% 
  mutate(origin_id = str_c("EWTAMPH_",c(1:nrow(.)))) %>% 
  filter(scientific_name %in% spp) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-"))) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), as.numeric) %>% 
  mutate(qds = 0)

## Check records
ewt %>% 
  group_by(scientific_name) %>% 
  tally

# Import PEM data ---------------------------------------------------------
pem <- read_xlsx("data input/occurence_data/All PEM RSA Amphibians 15Feb2019.xlsx") %>% 
  clean_names() %>% 
  rename(object_id = catalog_number,
         scientific_name = taxon_full,
         decimal_latitude = latitude,
         decimal_longitude = longitude,
         locality = locality_name,
         date = collection_date) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  filter(scientific_name %in% spp) %>% 
  mutate(origin_id = str_c("PEM_",c(1:nrow(.)))) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), as.numeric) %>% 
  mutate(qds = 0)


## Check records
pem %>% 
  group_by(scientific_name) %>% 
  tally

names(pem)[which(names(pem) %in% names(amph))]

# Import Cacosternum data -------------------------------------------------
caco <- read_csv("data input/occurence_data/PEM_cacosternum thorini.csv") %>% 
  clean_names() %>% 
  rename(object_id = accession_no,
         scientific_name = genus_species,
         decimal_latitude = d_lat,
         decimal_longitude = d_long,
         date = collection_date) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  mutate(origin_id = str_c("CACO_",c(1:nrow(.)))) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), funs(as.numeric)) %>% 
  mutate(object_id = as.character(object_id)) %>% 
  mutate(qds = 0)

names(caco)[which(names(caco) %in% names(amph))]

# Import National Museum Bloemfontein --------------------------------------------

### create QDS variable
nmb <- read_xlsx("data input/occurence_data/NMBAmphibians_May2019_Tolley.xlsx",
                 guess_max = 5000) %>% 
  clean_names() %>% 
  mutate(scientific_name = str_c(str_to_sentence(genus), " ", 
                                 str_to_lower(species))) %>% 
  filter(scientific_name %in% spp) %>% 
  rename(object_id = nmb,
         family = family_name,
         day = cday,
         month = cmon,
         year = cyr) %>% 
  mutate(date = dmy(str_c(day, month, year, sep = "-"))) %>% 
  mutate(origin_id = str_c("NMB_",c(1:nrow(.)))) %>% 
  mutate_at(vars(lats1:longe3),funs(as.numeric)) %>% 
  mutate_at(vars(lats1:longe3),funs(replace_na(., 0))) %>% 
  mutate(decimal_latitude=-(lats1 + lats2/60 + lats3/60^2),
         decimal_longitude=longe1 + longe2/60 + longe3/60^2) %>% 
  mutate(decimal_latitude = ifelse(decimal_latitude == 0,NA,decimal_longitude),
         decimal_longitude = ifelse(decimal_longitude == 0,NA,decimal_longitude)) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), funs(as.numeric)) %>% 
  mutate(qds = ifelse(lats2 == 0, 1, 0))

nmb
names(nmb)[which(names(nmb) %in% names(amph))]

nmb %>% 
  filter(qds == 0)

## Check records
nmb %>% 
  group_by(scientific_name) %>% 
  tally


# Import Amatola toad - historical & Bionerds ------------------------------------
amatoad <- read_xlsx("data input/occurence_data/Amatola Toad Sites_2020 Survey Data.xlsx") %>% 
  mutate_at(vars(decimal_latitude:decimal_longitude), as.numeric) %>% 
  mutate(qds = 0)


# Import EWT 2021 records --------------------------------------------------------
ewt21 <- read_csv("data input/occurence_data/EWT 2021_Threatened amphibian records 2019-2021.csv")%>% 
  clean_names() %>% 
  mutate(origin_id = str_c("EWT21_",c(1:nrow(.)))) %>% 
  filter(scientific_name %in% spp) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-"))) %>% 
  mutate_at(vars(decimal_latitude,decimal_longitude,day,month,year), as.numeric) %>% 
  mutate(qds = 0)

## Check records
ewt21 %>% 
  group_by(scientific_name) %>% 
  tally


# Import Anhydrophryne rattrayi data from PEM -----------------------------
anhydro <- read_xlsx("data input/occurence_data/Anhydrophryne rattrayi PEM FBIP data.xlsx") %>% 
  clean_names() %>% 
  mutate(origin_id = str_c("ANHYDRO_",c(1:nrow(.)))) %>% 
  mutate(scientific_name = str_c(genus, species, sep = " ")) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-"))) %>% 
  mutate(qds = 0)

glimpse(anhydro)

# Import Bionerds Xenopus records -----------------------------------------
xeno <- read_xlsx("data input/occurence_data/Bionerds Xenopus records.xlsx") %>% 
  mutate(date = dmy(str_c(day, month, year, sep = "-"))) %>% 
  mutate(qds = 0)

glimpse(xeno)

# Join all ----------------------------------------------------------------
amph_df <- amph %>% 
  bind_rows(adu %>%
              select(names(adu)[which(names(adu) %in% names(amph))])) %>% 
  bind_rows(ewt %>%
              select(names(ewt)[which(names(ewt) %in% names(amph))])) %>% 
  bind_rows(caco %>%
              select(names(caco)[which(names(caco) %in% names(amph))])) %>% 
  bind_rows(pem %>%
              select(names(pem)[which(names(pem) %in% names(amph))])) %>% 
  bind_rows(nmb %>%
              select(names(nmb)[which(names(nmb) %in% names(amph))])) %>% 
  bind_rows(amatoad %>%
              select(names(amatoad)[which(names(amatoad) %in% names(amph))])) %>% 
  bind_rows(ewt21 %>%
              select(names(ewt21)[which(names(ewt21) %in% names(amph))]))  %>% 
  bind_rows(anhydro %>%
              select(names(anhydro)[which(names(anhydro) %in% names(amph))])) %>% 
  bind_rows(xeno %>%
              select(names(xeno)[which(names(xeno) %in% names(amph))]))

glimpse(amph_df)
glimpse(tail(amph_df,10))

## Check it all adds up
nrow(amph)+nrow(caco)+nrow(adu)+nrow(ewt)+nrow(pem)+
  nrow(nmb)+nrow(amatoad)+nrow(ewt21)+nrow(anhydro)+nrow(xeno)== nrow(amph_df)

# QDS records -------------------------------------------------------------

## Check QDS tally 
amph_df %>% 
  filter(qds == 1) %>% 
  group_by(scientific_name) %>% 
  tally() %>% 
  rename (QDS_record = n) %>% 
  full_join(
    amph_df %>% 
      filter(qds == 0) %>% 
      group_by(scientific_name) %>% 
      tally() %>% 
      rename(GPS_record = n), 
    by = "scientific_name"
  ) %>% 
  mutate(total = QDS_record + GPS_record,
         propQDS = QDS_record/total*100)

## Write QDS records
amph_df %>% 
  filter(qds == 1) %>% 
  write_csv("data output/qds_occ_records.csv") 

# Write csvs --------------------------------------------------------------

## All data
write_csv(amph_df,"data output/occ_filtered.csv")

## Source data
names <- c("AMPH","ADU2021","CACO","EWT","NMB","PEM","AMATOAD","EWT21","ANHYDRO","XENO")
dfs <- list(amph,adu,caco,ewt,nmb,pem,amatoad,ewt21,anhydro,xeno)

walk2(dfs,names,
      .f = function(x,y){ 
        write_csv(x,glue::glue("data output/occurrence records processing/occID_{y}.csv"))}
)

