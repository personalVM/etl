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
  
  
  capital_munic = c("1200401", "2704302", "1600303", "1302603", "2927408", "2304400", "5300108", "3205309", "5208707", "2111300", "5103403", "5002704", "3106200", "1501402", "2507507", "4106902", "2611606", "2211001", "3304557", "2408102", "4314902", "1100205", "1400100", "4205407", "3550308", "2800308", "1721000")
  capital_meso = c("1101", "1202", "1303", "1401", "1503", "1602", "1702", "2101", "2202", "2303", "2404", "2504", "2605", "2703", "2803", "2905", "3107", "3203", "3306", "3515", "4110", "4205", "4305", "5002", "5104", "5203", "5301")
  capital_micro = c("11001", "12004", "13007", "14001", "15007", "16003", "17006", "21002", "22003", "23016", "24018", "25022", "26017", "27011", "28011", "29021", "31030", "32009", "33018", "35061", "41037", "42016", "43026", "50004", "51017", "52010", "53001")
  capital_rgint = c("1101", "1201", "1301", "1401", "1501", "1601", "1701", "2101", "2201", "2301", "2401", "2501", "2601", "2701", "2801", "2901", "3101", "3201", "3301", "3501", "4101", "4201", "4301", "5001", "5101", "5201", "5301")
  capital_rgime = c("110001", "120001", "130001", "140001", "150001", "160001", "170001", "210001", "220001", "230001", "240001", "250001", "260001", "270001", "280001", "290001", "310001", "320001", "330001", "350001", "410001", "420001", "430001", "500001", "510001", "520001", "530001")


  if (grouped_by == "munic") {
    res %>%
      select(cd_munic) %>%
      mutate(is_capital = ifelse(cd_munic %in% capital_munic, 1, 0)) %>% 
      rio::export(., "volume/data/curated_data/munic/df_geoclass_munic.csv")
  } else if (grouped_by == "micro") {
    res %>%
      dplyr::group_by(cd_micro) %>%
      dplyr::summarise(.groups = "drop") %>%
      mutate(is_capital = ifelse(cd_micro %in% capital_micro, 1, 0)) %>% 
      rio::export(., "volume/data/curated_data/micro/df_geoclass_micro.csv")
  } else if (grouped_by == "meso") {
    res %>%
      dplyr::group_by(cd_meso) %>%
      dplyr::summarise(.groups = "drop") %>%
      mutate(is_capital = ifelse(cd_meso %in% capital_meso, 1, 0)) %>%
      rio::export(., "volume/data/curated_data/meso/df_geoclass_meso.csv")
  } else if (grouped_by == "rgime") {
    res %>%
      dplyr::group_by(cd_rgime) %>%
      dplyr::summarise(.groups = "drop") %>%
      mutate(is_capital = ifelse(cd_rgime %in% capital_rgime, 1, 0)) %>%
      rio::export(., "volume/data/curated_data/rgime/df_geoclass_rgime.csv")
  } else if (grouped_by == "rgint") {
    res %>%
      dplyr::group_by(cd_rgint) %>%
      dplyr::summarise(.groups = "drop") %>%
      mutate(is_capital = ifelse(cd_rgint %in% capital_rgint, 1, 0)) %>%
      rio::export(., "volume/data/curated_data/rgint/df_geoclass_rgint.csv")
  }
}

# data_geoclass()
