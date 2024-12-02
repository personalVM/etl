# R-script data_geoclass.R


# References --------------------------------------------------------------

# Function ----------------------------------------------------------------
data_geoclass <- function(grouped_by = "micro") {
  
  # Setup
  # rm(list = ls())
  gc()
  options(scipen = 666, stringsAsFactors = F)
  source("volume/etl/util_loadPackages.R")
  
  res <- readr::read_csv("volume/data/clean_data/munic/df_locations_munic.csv") %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()

  if (grouped_by == "munic") {
    res %>%
      select(cd_munic, nm_munic, cd_micro, nm_micro, cd_meso, nm_meso, cd_rgime, nm_rgime, cd_rgint, nm_rgint, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      rio::export(., "volume/data/curated_data/munic/df_geoclass_munic.csv")
  } else if (grouped_by == "micro") {
    res %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      dplyr::group_by(cd_micro, nm_micro, cd_meso, nm_meso, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      dplyr::summarise(.groups = "drop") %>%
      rio::export(., "volume/data/curated_data/micro/df_geoclass_micro.csv")
  } else if (grouped_by == "meso") {
    res %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      dplyr::group_by(cd_meso, nm_meso, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      dplyr::summarise(.groups = "drop") %>%
      rio::export(., "volume/data/curated_data/meso/df_geoclass_meso.csv")
  } else if (grouped_by == "rgime") {
    res %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      dplyr::group_by(cd_rgime, nm_rgime, cd_rgint, nm_rgint, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      dplyr::summarise(.groups = "drop") %>%
      rio::export(., "volume/data/curated_data/rgime/df_geoclass_rgime.csv")
  } else if (grouped_by == "rgint") {
    res %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      dplyr::group_by(cd_rgint, nm_rgint, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      dplyr::summarise(.groups = "drop") %>%
      rio::export(., "volume/data/curated_data/rgint/df_geoclass_rgint.csv")
  } else if (grouped_by == "state") {
    res %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      dplyr::group_by(cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      dplyr::summarise(.groups = "drop") %>%
      rio::export(., "volume/data/curated_data/state/df_geoclass_state.csv")
  } else if (grouped_by == "region") {
    res %>%
      filter(cd_munic%in%c(1200401, 2704302, 1600303, 1302603, 2927408, 2304400, 5300108, 3205309, 5208707, 2111300, 5103403, 5002704, 3106200, 1501402, 2507507, 4106902, 2611606, 2211001, 3304557, 2408102, 4314902, 1100205, 1400100, 4205407, 3550308, 2800308, 1721000)) %>% 
      dplyr::group_by(cd_region, sg_region, nm_region) %>%
      dplyr::summarise(.groups = "drop") %>%
      rio::export(., "volume/data/curated_data/reg/df_geoclass_reg.csv")
  }
}

# data_geoclass()
