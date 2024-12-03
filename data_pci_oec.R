# R-script data_exports.R

# Functionto get PCI - Product Complexity Index - values from OEC

# References --------------------------------------------------------------

# https://oec.world/olap-proxy/data.jsonrecords?cube=complexity_pci_a_hs96_hs4&drilldowns=HS4,PCI+Rank,Year&measures=PCI&parents=false&sparse=false

# Function PCI oec --------------------------------------------------------
data_pci_oec <- function(use_backup=T){
  
  library(httr)
  source("volume/etl/util_loadPackages.R")

  # LOAD Phttr# LOAD PCI - Product Complexity Index
  if(use_backup==F){
    api_url <- "https://oec.world/api/olap-proxy/data.jsonrecords?cube=complexity_pci_a_hs96_hs4&drilldowns=HS4%2CPCI+Rank%2CYear&measures=PCI&parents=false&sparse=false"
    response <- httr::GET(api_url)
    if (status_code(response) == 200) {
      data <- content(response, "text", encoding = "UTF-8") %>%
        fromJSON(flatten = TRUE)
      pci_oec <- as.data.frame(data$data) %>% 
        janitor::clean_names()
    } else {
      print(paste("Failed to fetch data. Status code:", status_code(response)))
    }
  } else if (use_backup==F) {
    pci_oec <- read_csv("volume/data/clean_data/pci_oec.csv")
  }
  
  df_pci <- pci_oec %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(
      cd_sh4=as.character(hs4_id),
      nm_sh4=as.character(hs4),
    ) %>% 
    dplyr::filter(year==2010) %>% 
    dplyr::select(cd_sh4, pci) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  rio::export(df_pci, "volume/data/clean_data/pci_2010_oec_curated.csv")
  
}

# data_pci_oec()


