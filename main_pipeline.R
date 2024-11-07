# R-script main_pipeline.R

# References --------------------------------------------------------------

# A pipeline to call functions sequentially

# Setup -------------------------------------------------------------------

# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")
source("volume/etl/data_locations.R")
source("volume/etl/data_emig.R")
source("volume/etl/data_exports.R")
source("volume/etl/data_imig.R")
source("volume/etl/data_diversity.R")

# source("fct_createDF.R")
# source("fct_global.R")
# source("fct_local.R")
# source("fct_corMatrix.R")

# Main -------------------------------------------------------------------
data_locations(grouped_by = "munic")
data_locations(grouped_by = "micro")

data_emig(grouped_by = "munic")
data_emig(grouped_by = "micro")

data_exports(grouped_by = "munic")
data_exports(grouped_by = "micro")

data_imig(grouped_by = "mun")
data_imig(grouped_by = "micro")
