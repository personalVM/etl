# R-script main_pipeline.R

# References --------------------------------------------------------------

# A pipeline to call functions sequentially

# Setup -------------------------------------------------------------------

getwd()
setwd("/home/rstudio/")

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
source("volume/etl/data_distance.R")
source("volume/etl/data_education.R")
source("volume/etl/data_labor_CEMPRE_IBGE.R")
source("volume/etl/data_nationalAccounts_IBGE.R")
source("volume/etl/data_area.R")
source("volume/etl/data_geoclass.R")

# Main -------------------------------------------------------------------
data_locations(grouped_by = "munic")
data_locations(grouped_by = "micro")

data_emig(grouped_by = "munic")
data_emig(grouped_by = "micro")

# data_imig(grouped_by = "munic")
data_imig(grouped_by = "micro")

data_exports(grouped_by = "munic")
data_exports(grouped_by = "micro") 

# data_area(grouped_by = "munic") # TODO: fix warning:
data_area(grouped_by = "micro")

# Other distances in the same function:
# Distance to SP, DF, to the closest capital city, distance from the borders.
# data_distCoast(grouped_by="munic") # other distances here also
data_distance(grouped_by="micro")

data_education(grouped_by="munic")
data_education(grouped_by="micro")

# There is more to be developed
# Other years?
# get data directly in micro form?
# Oportunity: mean size of firms in BR, as by deviding firms by labor
data_labor_CEMPRE_IBGE(grouped_by = "munic")
data_labor_CEMPRE_IBGE(grouped_by = "micro")

# data_nationalAccounts_IBGE(grouped_by = "munic")
data_nationalAccounts_IBGE(grouped_by = "micro")

#
data_geoclass(grouped_by = "munic")
data_geoclass(grouped_by = "micro")








