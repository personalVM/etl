# R-script main_pipeline.R

# References --------------------------------------------------------------

# A pipeline to call functions sequentially

# Setup -------------------------------------------------------------------

# rm(list = ls())
gc()
options(scipen = 666, stringsAsFactors = F)
source("volume/etl/util_loadPackages.R")
source("volume/etl/data_locations.R")
source("volume/etl/data_emig.R")
source("volume/etl/data_exports.R")
source("volume/etl/data_imig.R")
source("volume/etl/data_diversity.R")
source("volume/etl/data_area.R")
source("volume/etl/data_distCoast.R")
source("volume/etl/data_education.R")

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

data_imig(grouped_by = "munic")
data_imig(grouped_by = "micro")

data_area(grouped_by = "munic") # TODO: fix warning:
data_area(grouped_by = "micro")

data_distCoast(grouped_by="munic")
data_distCoast(grouped_by="micro")

data_education(grouped_by="munic")
data_education(grouped_by="micro")






