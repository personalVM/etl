# R-script main_pipeline.R

# References --------------------------------------------------------------

# A pipeline to call functions sequentially

# Setup -------------------------------------------------------------------

# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")
source("volume/etl/data_exports.R")
# source("fct_createDF.R")
# source("fct_global.R")
# source("fct_local.R")
# source("fct_corMatrix.R")

# Main -------------------------------------------------------------------
data_exports(grouped_by = "mun")









