# Step 1: select focal species --------------------------------------------
#TODO Double check NT list with OC

# sppselect <- "Cacosternum capense"
# sppselect <- "Breviceps gibbosus"
sppselect <- "Hemisus guttatus"

# Step 2: extract the occurrence data from amphibian databases ----------
source("src/01_extract and process occ data.R")
rm(list = ls()[!(ls() %in% "sppselect")])

# Step 3: create plots ----------------------------------------------------
source("src/02_spatiotemporal occ plots.R")
rm(list = ls()[!(ls() %in% "sppselect")])

# Step 4: prepare data for SDMs  ------------------------------------------
source("src/03_prepare SDM inputs.R")
rm(list = ls()[!(ls() %in% "sppselect")])

# Step 5: Run BART SDMs ---------------------------------------------------
source("src/04_BART SDMs.R")
rm(list = ls()[!(ls() %in% "sppselect")])
